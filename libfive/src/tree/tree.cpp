/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2020  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <fstream>
#include <iostream>
#include <map>
#include <stack>
#include <unordered_set>

#include "libfive/tree/tree.hpp"
#include "libfive/tree/data.hpp"
#include "libfive/tree/archive.hpp"
#include "libfive/eval/eval_array.hpp"
#include "libfive/oracle/oracle_clause.hpp"

namespace libfive {

Tree::~Tree() {
    if (!ptr) {
        return;
    }
    std::stack<const Data*> todo;
    todo.push(ptr);
    while (!todo.empty()) {
        auto t = todo.top();
        todo.pop();
        // If this was the last remaining reference to this tree, then
        // empty it out (so that its destructor doesn't recurse) and add
        // its children to the queue for refcount subtraction.
        if (!--t->refcount) {
            // Move the children out of the Tree, adding them to the queue
            // before deleting the tree (to prevent recursion).  We use
            // std::exchange to steal the pointer out from the tree; we'll
            // decrement its refcount (and possibly recurse) in later
            // iterations of the loop.
            if (auto d = std::get_if<TreeUnaryOp>(t)) {
                todo.push(std::exchange(d->lhs.ptr, nullptr));
            } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
                todo.push(std::exchange(d->lhs.ptr, nullptr));
                todo.push(std::exchange(d->rhs.ptr, nullptr));
            } else if (auto d = std::get_if<TreeRemap>(t)) {
                todo.push(std::exchange(d->x.ptr, nullptr));
                todo.push(std::exchange(d->y.ptr, nullptr));
                todo.push(std::exchange(d->z.ptr, nullptr));
                todo.push(std::exchange(d->t.ptr, nullptr));
            }
            delete t;
        }
    }
}

Tree::Tree(float f)
    : Tree(new Data(TreeConstant { f }))
{
    // Nothing to do here
}

Tree::Tree(const Data* d, bool increment_refcount)
    : ptr(d)
{
    if (d && increment_refcount) {
        d->refcount++;
    }
}

Tree::Tree(std::unique_ptr<const OracleClause>&& o)
    : Tree(new Data( TreeOracle { std::move(o) }))
{
    // Nothing to do here
}

Tree Tree::var() {
    return Tree::nonary(Opcode::VAR_FREE);
}

Tree Tree::unary(Opcode::Opcode op, const Tree& lhs) {
    // We can only build unary operations with this function
    if (Opcode::args(op) != 1) {
        return invalid();
    }
    // Collapse constant operations
    else if (lhs->op() == Opcode::CONSTANT) {
        auto tmp = Tree(new Data(TreeUnaryOp { op, lhs }));

        OptimizedTree opt;
        opt.tree = tmp;
        ArrayEvaluator eval(opt);

        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // abs is idempotent after abs() or square()
    else if (op == Opcode::OP_ABS) {
        if (lhs->op() == Opcode::OP_ABS) {
            return lhs;
        } else if (lhs->op() == Opcode::OP_SQUARE) {
            return lhs;
        }
    }
    // Double-negative returns the original
    else if (op == Opcode::OP_NEG && lhs->op() == Opcode::OP_NEG) {
        return lhs->lhs();
    }
    // Default if we didn't fall into any special cases
    return Tree(new Data(TreeUnaryOp { op, lhs }));
}

Tree Tree::nonary(Opcode::Opcode op) {
    // We can only build unary operations with this function
    if (Opcode::args(op) != 0) {
        return invalid();
    }
    // Special-case handling to re-use X/Y/Z singletons
    switch (op) {
        case Opcode::VAR_X: return X();
        case Opcode::VAR_Y: return Y();
        case Opcode::VAR_Z: return Z();
        default: break;
    }
    // Default if we didn't fall into any special cases
    return Tree(new Data(TreeNonaryOp { op }));
}

Tree Tree::binary(Opcode::Opcode op, const Tree& lhs, const Tree& rhs) {
    // We can only build binary operations with this function
    if (Opcode::args(op) != 2) {
        return invalid();
    }
    // Collapse constant operations
    else if (lhs->op() == Opcode::CONSTANT && rhs->op() == Opcode::CONSTANT) {
        auto tmp = Tree(new Data(TreeBinaryOp { op, lhs, rhs }));

        // Use the private constructor to construct an OptimizedTree without
        // actually doing the optimization pass, which would trigger infinite
        // recursion.
        OptimizedTree opt;
        opt.tree = tmp;
        ArrayEvaluator eval(opt);

        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // Division by 1 is ignored
    else if (op == Opcode::OP_DIV) {
        if (rhs->op() == Opcode::CONSTANT && rhs->value() == 1.0f) {
            return lhs;
        }
    }
    else if (op == Opcode::OP_ADD) {
        if (lhs->op() == Opcode::CONSTANT && lhs->value() == 0.0f) {
            return rhs;
        } else if (rhs->op() == Opcode::CONSTANT && rhs->value() == 0.0f) {
            return lhs;
        } else if (rhs->op() == Opcode::OP_NEG) {
            const Tree& t = rhs->lhs();
            const Tree& q = lhs;
            return q - t;
        } else if (lhs->op() == Opcode::OP_NEG) {
            return rhs - lhs->lhs();
        }
    } else if (op == Opcode::OP_SUB) {
        if (lhs->op() == Opcode::CONSTANT && lhs->value() == 0.0f) {
            return -rhs;
        } else if (rhs->op() == Opcode::CONSTANT && rhs->value() == 0.0f) {
            return lhs;
        } else if (rhs->op() == Opcode::OP_NEG) {
            return lhs + rhs->lhs();
        }
    } else if (op == Opcode::OP_MUL) {
        if (lhs->op() == Opcode::CONSTANT) {
            if (lhs->value() == 0) {
                return lhs;
            } else if (lhs->value() == 1) {
                return rhs;
            } else if (lhs->value() == -1) {
                return -rhs;
            }
        } else if (rhs->op() == Opcode::CONSTANT) {
            if (rhs->value() == 0) {
                return rhs;
            } else if (rhs->value() == 1) {
                return lhs;
            } else if (rhs->value() == -1) {
                return -lhs;
            }
        } else if (lhs.id() == rhs.id()) {
            return square(lhs);
        }
    } else if (op == Opcode::OP_NTH_ROOT || op == Opcode::OP_POW) {
        if (rhs->op() == Opcode::CONSTANT && rhs->value() == 1.0f) {
            return lhs;
        }
    } else if (op == Opcode::OP_MIN || op == Opcode::OP_MAX) {
        if (lhs.id() == rhs.id()) {
            return lhs;
        }
    }
    // Default if we didn't fall into any special cases
    return Tree(new Data(TreeBinaryOp {op, lhs, rhs}));
}

// Use Meyer's singletons for X/Y/Z, since they're the most common trees
Tree Tree::X() {
    static auto x = Tree(new Data(TreeNonaryOp { Opcode::VAR_X }));
    return x;
}
Tree Tree::Y() {
    static auto y = Tree(new Data(TreeNonaryOp { Opcode::VAR_Y }));
    return y;
}
Tree Tree::Z() {
    static auto z = Tree(new Data(TreeNonaryOp { Opcode::VAR_Z }));
    return z;
}

Tree Tree::invalid() {
    static auto i = Tree(new Data(TreeInvalid {}));
    return i;
}

Tree Tree::with_const_vars() const {
    return Tree::unary(Opcode::CONST_VAR, *this);
}

bool Tree::operator==(const Tree& other) const {
    return get() == other.get();
}

bool Tree::operator!=(const Tree& other) const {
    return !(*this == other);
}

bool Tree::operator<(const Tree& other) const {
    return get() < other.get();
}

std::ostream& Tree::print_prefix(std::ostream& s) const {
    std::stack<std::variant<const Data*, char>> todo;
    todo.push(get());

    std::stack<Opcode::Opcode> ops;
    ops.push(Opcode::INVALID); // Remove need to check ops.size()

    while (todo.size()) {
        auto t = todo.top();
        todo.pop();
        if (auto c = std::get_if<char>(&t)) {
            switch (*c) {
                case ')': s << ')'; ops.pop(); break;
                case '|': ops.pop(); break;
                case ' ': s << ' '; break;
            }
        } else if (auto d = std::get_if<const Data*>(&t)) {
            auto op = (**d).op();
            if (op == Opcode::CONSTANT) {
                s << (**d).value();
            } else if (op == Opcode::ORACLE) {
                s << '\'' << (**d).oracle_clause().name();
            } else if (Opcode::args(op) == 0) {
                s << Opcode::toOpString(op);
            } else {
                if (Opcode::isCommutative(op) && ops.top() == op) {
                    todo.push('|');
                } else {
                    s << "(" << Opcode::toOpString(op) << " ";
                    todo.push(')');
                }
                ops.push(op);
                if (Opcode::args(op) == 1) {
                    todo.push((**d).lhs().get());
                } else {
                    todo.push((**d).rhs().get());
                    todo.push(' ');
                    todo.push((**d).lhs().get());
                }
            }
        }
    }
    return s;
}

void Tree::serialize(std::ostream& out) const {
    return Archive(*this).serialize(out);
}

Tree Tree::deserialize(std::istream& in) {
    auto s = Archive::deserialize(in).shapes;
    assert(s.size() == 1);
    return s.front().tree;
}

Tree Tree::load(const std::string& filename) {
    std::ifstream in(filename, std::ios::in|std::ios::binary);
    return in.is_open() ? deserialize(in) : invalid();
}

////////////////////////////////////////////////////////////////////////////////

bool Tree::is_valid() const {
    return !std::get_if<TreeInvalid>(get());
}

Tree Tree::remap(Tree X, Tree Y, Tree Z) const {
    // Only insert a remap operation if the tree has things that could be
    // remapped (i.e. x/y/z coordinates or an oracle clause).
    if (ptr->flags & (TreeData::TREE_FLAG_HAS_XYZ |
                      TreeData::TREE_FLAG_HAS_ORACLE))
    {
        return Tree(new Data(TreeRemap {
                    X.flatten(),
                    Y.flatten(),
                    Z.flatten(),
                    *this }));
    } else {
        return *this;
    }
}

Tree Tree::flatten() const {
    if (!(ptr->flags & TreeData::TREE_FLAG_HAS_REMAP)) {
        return *this;
    }

    using State = TreeRemapKey;
    const auto root_key = std::make_tuple(Tree::X().get(),
                                          Tree::Y().get(),
                                          Tree::Z().get(), ptr);

    std::stack<State> todo;
    todo.push(root_key);

    std::map<State, const Tree::Data*> remap;

    while (todo.size()) {
        auto k = todo.top();
        todo.pop();

        // If this tree has already been explored (with the given remapping),
        // then keep going.
        auto itr = remap.find(k);
        if (itr != remap.end()) {
            continue;
        }
    }

    auto itr = remap.find(root_key);
    return itr == remap.end() ? *this : Tree(itr->second);
}

Tree Tree::substitute(std::unordered_map<Tree::Id, Tree>&& s) const {
    if (ptr->flags & TreeData::TREE_FLAG_HAS_REMAP) {
        throw TreeData::RemapException();
    }
    auto flat = walk();
    for (auto t : flat) {
        Tree changed = Tree::invalid();

        if (auto d = std::get_if<TreeUnaryOp>(t)) {
            auto itr = s.find(d->lhs.id());
            if (itr != s.end()) {
                changed = Tree::unary(d->op, itr->second);
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
            auto lhs = s.find(d->lhs.id());
            auto rhs = s.find(d->rhs.id());
            if (lhs != s.end() || rhs != s.end()) {
                changed = Tree::binary(
                    d->op,
                    (lhs == s.end()) ? d->lhs : lhs->second,
                    (rhs == s.end()) ? d->rhs : rhs->second);
            }
        } else if (auto d = std::get_if<TreeRemap>(t)) {
            // This should never happen, because we call flatten() above
            assert(false);
            throw TreeData::RemapException();
        }

        if (changed.is_valid()) {
            s.insert({t, changed});
        }
    }

    auto itr = s.find(get());
    return (itr == s.end()) ? *this : itr->second;
}

std::vector<const Tree::Data*> Tree::walk() const {
    if (ptr->flags & TreeData::TREE_FLAG_HAS_REMAP) {
        return flatten().walk();
    }
    // Store how many times each tree (by id) is referenced
    std::unordered_map<Id, unsigned> count;
    std::stack<const Data*> todo;
    todo.push(get());
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.top();
        todo.pop();

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push(d->lhs.get());
            }
            if (count[d->rhs.id()]++ == 0) {
                todo.push(d->rhs.get());
            }
        } else if (auto d = std::get_if<TreeRemap>(next)) {
            // This should never happen because of the check above
            assert(false);
            throw TreeData::RemapException();
        }
    }

    // Flatten the tree.  This is a heap-allocated recursive
    // descent, to avoid running into stack limitations.
    todo.push(get());

    std::vector<const Data*> flat;
    while (todo.size()) {
        auto next = todo.top();
        todo.pop();
        flat.push_back(next);

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            // Schedule child branches to be flattened *after all* of their
            // parents, since we'll be reversing the order of this tape
            // afterwards, meaning children will be evaluated *before all*
            // of their parents.
            if (--count.at(d->lhs.id()) == 0) {
                todo.push(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (--count.at(d->lhs.id()) == 0) {
                todo.push(d->lhs.get());
            }
            if (--count.at(d->rhs.id()) == 0) {
                todo.push(d->rhs.get());
            }
        }
    }
    // Reverse the tree so that the walk is from leaves to root
    std::reverse(flat.begin(), flat.end());

    return flat;
}

const Tree::Data* Tree::release() {
    return std::exchange(ptr, nullptr);
}

Tree Tree::reclaim(const Data* ptr) {
    return Tree(ptr, false); // Don't increment refcount
}

Tree Tree::unique_helper(std::unordered_map<Id, const Data*>& remapped,
                         std::map<TreeDataKey, const Data*>& canonical,
                         std::vector<Tree>& new_trees) const
{
    auto flat = walk();

    for (auto t : flat) {
        // Get canonical key by applying remap to all children
        auto key = t->key();
        bool changed = false;
        if (auto k = std::get_if<TreeUnaryKey>(&key)) {
            auto itr = remapped.find(std::get<1>(*k));
            if (itr != remapped.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
        } else if (auto k = std::get_if<TreeBinaryKey>(&key)) {
            auto itr = remapped.find(std::get<1>(*k));
            if (itr != remapped.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
            itr = remapped.find(std::get<2>(*k));
            if (itr != remapped.end()) {
                std::get<2>(*k) = itr->second;
                changed = true;
            }
        } else if (std::get_if<TreeRemapKey>(&key)) {
            // This should never happen, because walk() automatically flattens
            // trees that contain a remapped operation.
            assert(false);
            throw TreeData::RemapException();
        }

        auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remapped this tree to the canonical version and keep going.
        if (k_itr != canonical.end()) {
            remapped.insert({t, k_itr->second});
        } else if (!changed) {
            // This is the canonical tree, and it requires
            // no remapping, so we're done!
            canonical.insert(k_itr, {key, t});
        } else {
            // We need make a new canonical tree, using remapped arguments
            Tree out = Tree::invalid();
            if (auto d = std::get_if<TreeUnaryOp>(t)) {
                auto itr = remapped.find(d->lhs.id());
                assert(itr != remapped.end());
                out = Tree::unary(d->op, Tree(itr->second));
            } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
                auto lhs = remapped.find(d->lhs.id());
                auto rhs = remapped.find(d->rhs.id());
                assert(lhs != remapped.end() || rhs != remapped.end());
                out = Tree::binary(
                    d->op,
                    (lhs == remapped.end())
                        ? d->lhs
                        : Tree(lhs->second),
                    (rhs == remapped.end())
                        ? d->rhs
                        : Tree(rhs->second));
            }

            // The new tree is the canonical tree; folks that were using
            // the original tree need to use it instead.
            canonical.insert(k_itr, {key, out.get()});
            remapped.insert({t, out.get()});

            // The new pointer is owned by the new_trees list
            new_trees.emplace_back(Tree(std::move(out)));
        }
    }

    auto itr = remapped.find(get());
    return (itr == remapped.end())
        ? *this
        : Tree(itr->second);
}

Tree Tree::optimized() const {
    std::unordered_map<Id, const Data*> remap;
    std::map<Data::Key, const Data*> canonical;
    std::vector<Tree> new_trees;

    return optimized_helper(remap, canonical, new_trees);
}

Tree Tree::optimized_helper(std::unordered_map<Id, const Data*>& remap,
                            std::map<Data::Key, const Data*>& canonical,
                            std::vector<Tree>& new_trees) const
{
    // TODO: expand remaps here

    auto out = unique_helper(remap, canonical, new_trees);

    // Give all oracles a chance to optimize themselves as well, reusing
    // any deduplicated trees in the maps above.
    std::unordered_map<Tree::Id, Tree> remap_oracles;
    for (auto d : out.walk()) {
        if (auto t = std::get_if<TreeOracle>(d)) {
            if (auto o = t->oracle->optimized(remap, canonical, new_trees)) {
                remap_oracles.insert({d, Tree(std::move(o))});
            }
        }
    }
    if (remap_oracles.size()) {
        out = out.substitute(std::move(remap_oracles));
    }

    return out.collect_affine();
}

Tree Tree::unique() const {
    // If a specific tree should be remapped, that fact is stored here
    // These remap pointers can point either into the existing tree or
    // to Trees in the new_trees list below, so we store the bare pointer
    // and use Tree(ptr) to rehydrate it.
    std::unordered_map<Id, const Data*> remap;

    // The canonical tree for each Key is stored here
    std::map<Data::Key, const Data*> canonical;

    // New pointers are owned here, because the maps above hold
    // raw pointers instead of shared_ptrs.
    std::vector<Tree> new_trees;

    return unique_helper(remap, canonical, new_trees);
}

Tree::AffineMap Tree::explore_affine() const {
    using map = std::unordered_map<const Data*, float>;
    std::stack<std::optional<map>> maps;

    // Empty root map; this saves us from having to check whether
    // maps.size() > 0 whenever we want to look at maps.back().
    maps.push(std::nullopt);

    struct Pop { const Id id; };
    struct Node { const Data* data; float scale; };
    std::stack<std::variant<Pop, Node>> todo;
    todo.push(Node { get(), 1 });

    AffineMap out;

    while (todo.size()) {
        const auto q = todo.top();
        todo.pop();

        if (auto n = std::get_if<Node>(&q)) {
            const auto t = n->data;
            const auto scale = n->scale;

            using namespace Opcode;
            const auto op = t->op();
            const bool could_be_affine = (op == OP_NEG) ||
                (op == OP_ADD) || (op == OP_SUB) ||
                (op == OP_MUL && (t->lhs()->op() == CONSTANT ||
                                  t->rhs()->op() == CONSTANT));

            if (could_be_affine) {
                // Recurse if we haven't already solved for this node
                auto itr = out.find(t);
                if (itr == out.end()) {
                    // If this is an affine node that isn't part of an affine tree,
                    // then store a new affine map onto the data stack before
                    // recursing down the node's children.
                    if (!maps.top().has_value()) {
                        todo.push(Pop{t});
                        maps.push(map());
                    }

                    if (op == OP_NEG) {
                        todo.push(Node { t->lhs().get(), -scale });
                    } else if (op == OP_ADD) {
                        todo.push(Node { t->lhs().get(),  scale });
                        todo.push(Node { t->rhs().get(),  scale });
                    } else if (op == OP_SUB) {
                        todo.push(Node { t->lhs().get(),  scale });
                        todo.push(Node { t->rhs().get(), -scale });
                    } else if (op == OP_MUL) {
                        if (t->lhs()->op() == CONSTANT) {
                            const float c = t->lhs()->value();
                            todo.push(Node { t->rhs().get(), scale * c });
                        } else if (t->rhs()->op() == CONSTANT) {
                            const float c = t->rhs()->value();
                            todo.push(Node { t->lhs().get(), scale * c });
                        }
                    }
                // If we've seen this node before, then just accumulate
                // its affine terms into the parent afine node
                } else {
                    auto& m = maps.top();
                    if (m.has_value()) {
                        for (const auto& k: itr->second) {
                            (*m)[k.first.get()] += scale * k.second;
                        }
                    }
                }
            } else {
                // If there's an affine map that's under construction,
                // contribute to it then hide it with an EmptyMap, since
                // we're about to recurse into a non-affine subtree.
                auto& m = maps.top();
                if (m.has_value()) {
                    (*m)[t] += scale;
                    if (args(op) > 0) {
                        todo.push(Pop{t});
                        maps.push(std::nullopt); // Marker for empty map
                    }
                }
                if (auto d = std::get_if<TreeUnaryOp>(t)) {
                    todo.push(Node { d->lhs.get(), 1.0f });
                } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
                    todo.push(Node { d->lhs.get(), 1.0f });
                    todo.push(Node { d->rhs.get(), 1.0f });
                } else if (std::get_if<TreeRemap>(t)) {
                    // This could happen if someone isn't careful!
                    throw TreeData::RemapException();
                }
            }
        } else if (auto p = std::get_if<Pop>(&q)) {
            // If this is a real map (rather than an empty map), then
            // accumulate its results into our output map.
            const auto& m = maps.top();
            if (m.has_value()) {
                auto& v = out[p->id];
                for (const auto& k: (*m)) {
                    v.push_back({Tree(k.first), k.second});
                }
            }
            maps.pop();
        }
    }
    return out;
}

Tree Tree::reduce_binary(std::vector<AffinePair>::const_iterator a,
                         std::vector<AffinePair>::const_iterator b)
{
    using itr = std::vector<AffinePair>::const_iterator;
    using pair = std::pair<itr, itr>;
    using option = std::optional<pair>;

    option v = pair(a, b);

    // This is a heap implementation of a recursive depth-first traverse,
    // to avoid blowing up the stack on deep trees.  This implementation
    // uses one stack for the tasks yet to do, and a second stack for the
    // outputs.
    std::stack<option> todo;
    todo.push(v);
    std::stack<Tree> out;
    while (todo.size()) {
        const auto t = todo.top();
        todo.pop();
        if (t.has_value()) {
            const auto a = t->first;
            const auto b = t->second;
            const auto delta = b - a;
            if (delta == 0) {
                out.push(Tree(0.0f));
            } else if (delta == 1) {
                out.push(a->first * a->second);
            } else {
                todo.push(std::nullopt); // marker to pop stack
                todo.push(pair(a, a + delta / 2));
                todo.push(pair(a + delta / 2, b));
            }
        } else {
            auto a = out.top();
            out.pop();
            auto b = out.top();
            out.pop();
            out.push(a + b);
        }
    }
    assert(out.size() == 1);
    return out.top();
}

Tree Tree::collect_affine() const {
    AffineMap map = explore_affine();

    std::unordered_map<Tree::Id, Tree> remap;
    for (auto& m : map) {
        // Sorting isn't strictly necessary, and could be a *tiny* performance
        // hit, but lets us make deterministic unit tests, which is appealing.
        //
        // We sort by the multiplier, since pointer comparisons are hit-or-miss
        std::sort(m.second.begin(), m.second.end(),
                  [](auto a, auto b) { return a.second > b.second; });
        remap.insert({m.first, reduce_binary(m.second.begin(),
                                             m.second.end())});
    }
    return substitute(std::move(remap));
}

size_t Tree::size() const {
    std::unordered_set<Id> seen;

    std::stack<const Data*> todo;
    todo.push(get());
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.top();
        todo.pop();
        if (!seen.insert(next).second) {
            continue;
        }

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            todo.push(d->lhs.get());
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            todo.push(d->lhs.get());
            todo.push(d->rhs.get());
        } else if (auto d = std::get_if<TreeRemap>(next)) {
            todo.push(d->x.get());
            todo.push(d->y.get());
            todo.push(d->y.get());
            todo.push(d->z.get());
        }
    }
    return seen.size();
}

////////////////////////////////////////////////////////////////////////////////

// Mass-produce definitions for overloaded operations
#define OP_UNARY(name, opcode)                      \
Tree name(const Tree& lhs) {                        \
    return Tree::unary(Opcode::opcode, lhs);        \
}
#define OP_BINARY(name, opcode)                     \
Tree name(const Tree& lhs, const Tree& rhs) {       \
    return Tree::binary(Opcode::opcode, lhs, rhs);  \
}
TREE_OPERATORS

////////////////////////////////////////////////////////////////////////////////

std::ostream& operator<<(std::ostream& stream, const Tree& tree)
{
    return tree.print_prefix(stream);
}

}   // namespace libfive
