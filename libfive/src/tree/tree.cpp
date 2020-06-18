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
#include <unordered_set>

#include "libfive/tree/tree.hpp"
#include "libfive/tree/data.hpp"
#include "libfive/tree/archive.hpp"
#include "libfive/eval/eval_array.hpp"
#include "libfive/oracle/oracle_clause.hpp"

namespace libfive {

Tree::Tree(float f)
    : Tree(std::make_shared<Data>(TreeConstant { f }))
{
    // Nothing to do here
}

Tree::Tree(std::shared_ptr<const Data> d)
    : std::shared_ptr<const Data>(d)
{
    // Nothing to do here
}

Tree::Tree(std::unique_ptr<const OracleClause>&& o)
    : Tree(std::make_shared<Data>( TreeOracle { std::move(o) }))
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
        auto tmp = Tree(std::make_shared<Data>(TreeUnaryOp { op, lhs }));

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
    return Tree(std::make_shared<Data>(
           TreeUnaryOp { op, lhs }));
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
    return Tree(std::make_shared<Data>(TreeNonaryOp { op }));
}

Tree Tree::binary(Opcode::Opcode op, const Tree& lhs, const Tree& rhs) {
    // We can only build binary operations with this function
    if (Opcode::args(op) != 2) {
        return invalid();
    }
    // Collapse constant operations
    else if (lhs->op() == Opcode::CONSTANT && rhs->op() == Opcode::CONSTANT) {
        auto tmp = Tree(std::make_shared<Data>(TreeBinaryOp { op, lhs, rhs }));

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
    return Tree(std::make_shared<Data>(
           TreeBinaryOp {op, lhs, rhs}));
}

// Use Meyer's singletons for X/Y/Z, since they're the most common trees
Tree Tree::X() {
    static auto x = std::make_shared<Data>(TreeNonaryOp { Opcode::VAR_X });
    return Tree(x);
}
Tree Tree::Y() {
    static auto y = std::make_shared<Data>(TreeNonaryOp { Opcode::VAR_Y });
    return Tree(y);
}
Tree Tree::Z() {
    static auto z = std::make_shared<Data>(TreeNonaryOp { Opcode::VAR_Z });
    return Tree(z);
}

Tree Tree::invalid() {
    static auto i = std::make_shared<Data>(TreeInvalid {});
    return Tree(i);
}

Tree Tree::with_const_vars() const {
    return Tree::unary(Opcode::CONST_VAR, *this);
}

std::ostream& Tree::print_prefix(std::ostream& s) const {
    std::vector<std::variant<const Data*, char>> todo;
    std::vector<Opcode::Opcode> ops;
    todo.push_back(get());

    while (todo.size()) {
        auto t = todo.back();
        todo.pop_back();
        if (auto c = std::get_if<char>(&t)) {
            switch (*c) {
                case ')': s << ')'; ops.pop_back(); break;
                case '|': ops.pop_back(); break;
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
                if (Opcode::isCommutative(op) && ops.size() && ops.back() == op) {
                    todo.push_back('|');
                } else {
                    s << "(" << Opcode::toOpString(op) << " ";
                    todo.push_back(')');
                }
                ops.push_back(op);
                if (Opcode::args(op) == 1) {
                    todo.push_back((**d).lhs().get());
                } else {
                    todo.push_back((**d).rhs().get());
                    todo.push_back(' ');
                    todo.push_back((**d).lhs().get());
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

    // If a specific tree (by id) should be remapped, that fact is stored here
    std::unordered_map<Id, Tree> m = {
            {Tree::X().id(), X},
            {Tree::Y().id(), Y},
            {Tree::Z().id(), Z}};
    return remap_from(m);
}

Tree Tree::remap_from(std::unordered_map<Tree::Id, Tree> remap) const {
    auto flat = walk();
    for (auto t : flat) {
        Tree changed = Tree::invalid();

        if (auto d = std::get_if<TreeUnaryOp>(t)) {
            auto itr = remap.find(d->lhs.id());
            if (itr != remap.end()) {
                changed = Tree::unary(d->op, itr->second);
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
            auto lhs = remap.find(d->lhs.id());
            auto rhs = remap.find(d->rhs.id());
            if (lhs != remap.end() || rhs != remap.end()) {
                changed = Tree::binary(
                    d->op,
                    (lhs == remap.end()) ? d->lhs
                                         : lhs->second,
                    (rhs == remap.end()) ? d->rhs
                                         : rhs->second );
            }
        } else if (auto d = std::get_if<TreeOracle>(t)) {
            // Oracles only support the basic remapping of X/Y/Z, so we detect
            // this case and handle it below.  Otherwise, we don't change the
            // change the oracle (e.g. we can't push an affine or other custom
            // remapping *through* an Oracle with the current API).
            auto x = remap.find(Tree::X().id());
            auto y = remap.find(Tree::Y().id());
            auto z = remap.find(Tree::Z().id());
            if (x != remap.end() && y != remap.end() && z != remap.end()) {
                auto r = d->oracle->remap(Tree(t->shared_from_this()),
                                          x->second, y->second, z->second);
                if (r != d->oracle) {
                    changed = Tree(std::move(r));
                }
            }
        }

        if (changed.is_valid()) {
            remap.insert({t, changed});
        }
    }

    auto itr = remap.find(get());
    return (itr == remap.end()) ? *this : itr->second;
}

std::vector<const Tree::Data*> Tree::walk() const {
    // Store how many times each tree (by id) is referenced
    std::unordered_map<Id, unsigned> count;
    std::vector todo = {get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (count[d->lhs.id()]++ == 0) {
                todo.push_back(d->lhs.get());
            }
            if (count[d->rhs.id()]++ == 0) {
                todo.push_back(d->rhs.get());
            }
        }
    }

    // Flatten the tree.  This is a heap-allocated recursive
    // descent, to avoid running into stack limitations.
    todo = {get()};

    std::vector<const Data*> flat;
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        flat.push_back(next);

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            // Schedule child branches to be flattened *after all* of their
            // parents, since we'll be reversing the order of this tape
            // afterwards, meaning children will be evaluated *before all*
            // of their parents.
            if (--count.at(d->lhs.id()) == 0) {
                todo.push_back(d->lhs.get());
            }
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            if (--count.at(d->lhs.id()) == 0) {
                todo.push_back(d->lhs.get());
            }
            if (--count.at(d->rhs.id()) == 0) {
                todo.push_back(d->rhs.get());
            }
        }
    }
    // We'll walk from the leafs up to the root, storing the first
    // unique instance of a given operation in the maps above, and
    // marking subsequent instances in the remap table.
    std::reverse(flat.begin(), flat.end());

    return flat;
}

Tree Tree::unique_helper(std::unordered_map<Id, const Data*>& remap,
                         std::map<TreeDataKey, const Data*>& canonical,
                         std::vector<Tree>& new_trees) const
{
    auto flat = walk();

    for (auto t : flat) {
        // Get canonical key by applying remap to all children
        auto key = t->key();
        bool changed = false;
        if (auto k = std::get_if<TreeUnaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
        } else if (auto k = std::get_if<TreeBinaryKey>(&key)) {
            auto itr = remap.find(std::get<1>(*k));
            if (itr != remap.end()) {
                std::get<1>(*k) = itr->second;
                changed = true;
            }
            itr = remap.find(std::get<2>(*k));
            if (itr != remap.end()) {
                std::get<2>(*k) = itr->second;
                changed = true;
            }
        }

        auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remap this tree to the canonical version and keep going.
        if (k_itr != canonical.end()) {
            remap.insert({t, k_itr->second});
        } else if (!changed) {
            // This is the canonical tree, and it requires
            // no remapping, so we're done!
            canonical.insert(k_itr, {key, t});
        } else {
            // We need make a new canonical tree, using remapped arguments
            std::shared_ptr<const Data> out;
            if (auto d = std::get_if<TreeUnaryOp>(t)) {
                auto itr = remap.find(d->lhs.id());
                assert(itr != remap.end());
                out = Tree::unary(d->op, Tree(itr->second->shared_from_this()));
            } else if (auto d = std::get_if<TreeBinaryOp>(t)) {
                auto lhs = remap.find(d->lhs.id());
                auto rhs = remap.find(d->rhs.id());
                assert(lhs != remap.end() || rhs != remap.end());
                out = Tree::binary(
                    d->op,
                    (lhs == remap.end())
                        ? d->lhs
                        : Tree(lhs->second->shared_from_this()),
                    (rhs == remap.end())
                        ? d->rhs
                        : Tree(rhs->second->shared_from_this()));
            }

            // The new tree is the canonical tree; folks that were using
            // the original tree need to use it instead.
            canonical.insert(k_itr, {key, out.get()});
            remap.insert({t, out.get()});

            // The new pointer is owned by the new_trees list
            new_trees.emplace_back(Tree(std::move(out)));
        }
    }

    auto itr = remap.find(get());
    return (itr == remap.end())
        ? *this
        : Tree(itr->second->shared_from_this());
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
        out = out.remap_from(remap_oracles);
    }

    return out.collect_affine();
}

Tree Tree::unique() const {
    // If a specific tree should be remapped, that fact is stored here
    // These remap pointers can point either into the existing tree or
    // to shared_ptrs in the new_trees list below, so we store the bare
    // pointer and use shared_from_this to rehydrate it.
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
    std::vector<std::optional<map>> maps;

    // Empty root map; this saves us from having to check whether
    // maps.size() > 0 whenever we want to look at maps.back().
    maps.push_back(std::nullopt);

    struct Pop { const Id id; };
    struct Node { const Data* data; float scale; };
    std::vector<std::variant<Pop, Node>> todo;
    todo.push_back(Node { get(), 1 });

    AffineMap out;

    while (todo.size()) {
        const auto q = todo.back();
        todo.pop_back();

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
                // If this is an affine node that isn't part of an affine tree,
                // then store a new affine map onto the data stack.
                if (!maps.back().has_value()) {
                    todo.push_back(Pop{t});
                    maps.push_back(map());
                }

                // Recurse if we haven't already solved for this node
                auto itr = out.find(t);
                if (itr == out.end()) {
                    if (op == OP_NEG) {
                        todo.push_back(Node { t->lhs().get(), -scale });
                    } else if (op == OP_ADD) {
                        todo.push_back(Node { t->lhs().get(),  scale });
                        todo.push_back(Node { t->rhs().get(),  scale });
                    } else if (op == OP_SUB) {
                        todo.push_back(Node { t->lhs().get(),  scale });
                        todo.push_back(Node { t->rhs().get(), -scale });
                    } else if (op == OP_MUL) {
                        if (t->lhs()->op() == CONSTANT) {
                            const float c = t->lhs()->value();
                            todo.push_back(Node { t->rhs().get(), scale * c });
                        } else if (t->rhs()->op() == CONSTANT) {
                            const float c = t->rhs()->value();
                            todo.push_back(Node { t->lhs().get(), scale * c });
                        }
                    }
                // If we've seen this node before, then just accumulate
                // its affine terms into the parent afine node
                } else {
                    auto& m = maps.back();
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
                auto& m = maps.back();
                if (m.has_value()) {
                    (*m)[t] += scale;
                    if (args(op) > 0) {
                        todo.push_back(Pop{t});
                        maps.push_back(std::nullopt); // Marker for empty map
                    }
                }
                switch (args(op)) {
                    case 2: todo.push_back(Node { t->rhs().get(), 1.0f }); // FALLTHROUGH
                    case 1: todo.push_back(Node { t->lhs().get(), 1.0f }); // FALLTHROUGH
                    default: break;
                }
            }
        } else if (auto p = std::get_if<Pop>(&q)) {
            // If this is a real map (rather than an empty map), then
            // accumulate its results into our output map.
            const auto& m = maps.back();
            if (m.has_value()) {
                auto& v = out[p->id];
                for (const auto& k: (*m)) {
                    v.push_back({Tree(k.first->shared_from_this()), k.second});
                }
            }
            maps.pop_back();
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
    std::vector<option> todo = {v};
    std::vector<Tree> out;
    while (todo.size()) {
        const auto t = todo.back();
        todo.pop_back();
        if (t.has_value()) {
            const auto a = t->first;
            const auto b = t->second;
            const auto delta = b - a;
            if (delta == 0) {
                out.push_back(Tree(0.0f));
            } else if (delta == 1) {
                out.push_back(a->first * a->second);
            } else {
                todo.push_back(std::nullopt); // marker to pop stack
                todo.push_back(pair(a, a + delta / 2));
                todo.push_back(pair(a + delta / 2, b));
            }
        } else {
            auto a = out.back();
            out.pop_back();
            auto b = out.back();
            out.pop_back();
            out.push_back(a + b);
        }
    }
    assert(out.size() == 1);
    return out.at(0);
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
    return remap_from(remap);
}

size_t Tree::size() const {
    std::unordered_set<Id> seen;

    std::vector<const Data*> todo = {get()};
    // Count how many branches reach to a given node.
    // This matters when flattening, since we're doing a topological sort
    while (todo.size()) {
        auto next = todo.back();
        todo.pop_back();
        if (!seen.insert(next).second) {
            continue;
        }

        if (auto d = std::get_if<TreeUnaryOp>(next)) {
            todo.push_back(d->lhs.get());
        } else if (auto d = std::get_if<TreeBinaryOp>(next)) {
            todo.push_back(d->lhs.get());
            todo.push_back(d->rhs.get());
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
