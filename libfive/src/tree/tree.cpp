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
    todo.push(ptr);

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
            if (auto t = std::get_if<TreeNonaryOp>(*d)) {
                s << Opcode::toOpString(t->op);
            } else if (auto t = std::get_if<TreeUnaryOp>(*d)) {
                s << "(" << Opcode::toOpString(t->op) << " ";
                ops.push(t->op);
                todo.push(')');
                todo.push(t->lhs.get());
            } else if (auto t = std::get_if<TreeBinaryOp>(*d)) {
                if (Opcode::isCommutative(t->op) && ops.top() == t->op) {
                    todo.push('|');
                } else {
                    s << "(" << Opcode::toOpString(t->op) << " ";
                    todo.push(')');
                }
                ops.push(t->op);
                todo.push(t->rhs.get());
                todo.push(' ');
                todo.push(t->lhs.get());
            } else if (auto t = std::get_if<TreeConstant>(*d)) {
                s << t->value;
            } else if (auto t = std::get_if<TreeOracle>(*d)) {
                s << '\'' << t->oracle->name();
            } else if (auto t = std::get_if<TreeRemap>(*d)) {
                s << "(remap ";
                todo.push(')');
                ops.push(Opcode::INVALID); // dummy op for remap
                todo.push(t->z.get());
                todo.push(' ');
                todo.push(t->y.get());
                todo.push(' ');
                todo.push(t->x.get());
                todo.push(' ');
                todo.push(t->t.get());
            } else {
                throw TreeData::InvalidException();
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
        return Tree(new Data(TreeRemap { X, Y, Z, *this }));
    } else {
        return *this;
    }
}

Tree Tree::flatten() const {
    if (!(ptr->flags & TreeData::TREE_FLAG_HAS_REMAP)) {
        return *this;
    }

    /*  If t is an axis, then pushes x/y/z to the output stack
     *
     *  If t is a remap, then pushes
     *      Down(t->x)  [executed first]
     *      Down(t->y)
     *      Down(t->z)
     *      Remap(t->tree)
     *  to the task stack
     *
     *  If t has children, pushes Up(t) to the task stack, followed by
     *  Down(t->lhs/rhs) for each child.
     *
     *  Otherwise, pushes t to the output stack immediately */
    struct Down {
        Tree x;
        Tree y;
        Tree z;
        const Data* t;
    };

    /*  If t has children, then pops them from the out stack and pushes
     *  a modified version of t to the out stack.
     *
     *  Otherwise, if t is a remap, then pops x', y', and z' from the output
     *  stack then pushes Down { x', y', z', t->t } to the task stack */
    struct Up {
        const Data* t;
    };

    using Task = std::variant<Down, Up>;
    std::stack<Task> todo;
    std::stack<Tree> out;

    todo.push(Down {Tree::X(), Tree::Y(), Tree::Z(), ptr });

    while (todo.size()) {
        auto k = todo.top();
        todo.pop();

        if (auto d=std::get_if<Down>(&k)) {
            if (auto t=std::get_if<TreeNonaryOp>(d->t)) {
                switch (t->op) {
                    case Opcode::VAR_X: out.push(d->x); break;
                    case Opcode::VAR_Y: out.push(d->y); break;
                    case Opcode::VAR_Z: out.push(d->z); break;
                    default: out.push(Tree(d->t)); break;
                }
            } else if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                todo.push(Up { d->t });
                todo.push(Down { d->x, d->y, d->z, t->lhs.ptr });
            } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                todo.push(Up { d->t });
                todo.push(Down { d->x, d->y, d->z, t->lhs.ptr });
                todo.push(Down { d->x, d->y, d->z, t->rhs.ptr });
            } else if (auto t=std::get_if<TreeConstant>(d->t)) {
                out.push(Tree(d->t));
            } else if (auto t=std::get_if<TreeOracle>(d->t)) {
                const Tree new_oracle(t->oracle->remap(
                            Tree(d->t), d->x, d->y, d->z));
                out.push(new_oracle);
            } else if (auto t=std::get_if<TreeRemap>(d->t)) {
                todo.push(Up { d->t });
                todo.push(Down { d->x, d->y, d->z, t->z.ptr });
                todo.push(Down { d->x, d->y, d->z, t->y.ptr });
                todo.push(Down { d->x, d->y, d->z, t->x.ptr });
            }
        } else if (auto d=std::get_if<Up>(&k)) {
            if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                const auto lhs = out.top();
                out.pop();
                if (lhs.ptr != t->lhs.ptr) {
                    out.push(Tree::unary(t->op, lhs));
                } else {
                    out.push(Tree(d->t));
                }
            } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                const auto lhs = out.top();
                out.pop();
                const auto rhs = out.top();
                out.pop();
                if (lhs.ptr != t->lhs.ptr || rhs.ptr != t->rhs.ptr) {
                    out.push(Tree::binary(t->op, lhs, rhs));
                } else {
                    out.push(Tree(d->t));
                }
            } else if (auto t=std::get_if<TreeRemap>(d->t)) {
                const auto z = out.top();
                out.pop();
                const auto y = out.top();
                out.pop();
                const auto x = out.top();
                out.pop();
                todo.push(Down { x, y, z, t->t.ptr });
            } else {
                // We shouldn't ever get here, because Nonary, Constant,
                // Oracles, and Remap shouldn't ever be pushed as Up nodes.
                assert(false);
            }
        }
    }

    assert(out.size() == 1);
    return out.top();
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

Tree Tree::substitute_with(std::function<const TreeData* (Tree)> fn) const {
    if (ptr->flags & TreeData::TREE_FLAG_HAS_REMAP) {
        return flatten().substitute_with(fn);
    }

    /*  Pushes Up(t) to the task stack, followed by Down(t->lhs/rhs)
     *  for each child (if any). */
    struct Down {
        const Data* t;
    };

    /*  If t has children, then pops them from the out stack, applies fn to
     *  t, then pushes either t or the output of fn(t) to the output stack. */
    struct Up {
        const Data* t;
    };

    using Task = std::variant<Down, Up>;
    std::stack<Task> todo;
    std::stack<Tree> out;

    todo.push(Down { ptr });

    while (todo.size()) {
        auto k = todo.top();
        todo.pop();

        if (const auto d=std::get_if<Down>(&k)) {
            todo.push(Up { d->t });
            if (const auto t=std::get_if<TreeNonaryOp>(d->t)) {
                // Nothing to do here
            } else if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                todo.push(Down { t->lhs.ptr });
            } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                todo.push(Down { t->lhs.ptr });
                todo.push(Down { t->rhs.ptr });
            } else if (auto t=std::get_if<TreeConstant>(d->t)) {
                // Nothing to do here
            } else if (auto t=std::get_if<TreeOracle>(d->t)) {
                // Nothing to do here
            } else if (std::get_if<TreeRemap>(d->t)) {
                // This should never happen, because we flatten above
                assert(false);
            } else {
                throw TreeData::InvalidException();
            }
        } else if (auto d=std::get_if<Up>(&k)) {
            if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                const auto lhs = out.top();
                out.pop();
                const auto n = lhs.ptr != t->lhs.ptr
                        ? Tree::unary(t->op, lhs)
                        : Tree(d->t);
                if (auto ptr = fn(n)) {
                    out.push(Tree(ptr));
                } else {
                    out.push(n);
                }
            } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                const auto lhs = out.top();
                out.pop();
                const auto rhs = out.top();
                out.pop();
                const auto n = lhs.ptr != t->lhs.ptr || rhs.ptr != t->rhs.ptr
                        ? Tree::binary(t->op, lhs, rhs)
                        : Tree(d->t);
                if (auto ptr = fn(n)) {
                    out.push(Tree(ptr));
                } else {
                    out.push(n);
                }
            } else {
                if (auto ptr = fn(Tree(d->t))) {
                    out.push(Tree(ptr));
                } else {
                    out.push(Tree(d->t));
                }
            }
        }
    }

    assert(out.size() == 1);
    return out.top();
}

Tree Tree::unique_helper(std::map<TreeDataKey, Tree>& canonical) const {
    if (ptr->flags & TreeData::TREE_FLAG_HAS_REMAP) {
        return flatten().unique_helper(canonical);
    }

    return substitute_with([&canonical](const Tree t) {
        const auto key = t->key();
        const auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remapped this tree to the canonical version and keep going.
        if (k_itr != canonical.end()) {
            return k_itr->second.ptr;
        } else {
            canonical.insert(k_itr, {key, Tree(t)});
            return t.ptr;
        }
    });
}

Tree Tree::optimized() const {
    std::map<Data::Key, Tree> canonical;
    return optimized_helper(canonical);
}

Tree Tree::optimized_helper(std::map<Data::Key, Tree>& canonical) const {
    Tree out = *this;

    // Expand any remap operations in the tree
    out = out.flatten();

    // Deduplicate the tree, so that shared nodes are reused
    out = out.unique_helper(canonical);

    // Give all oracles a chance to optimize themselves as well, reusing
    // any deduplicated trees in the maps above.
    if (out->flags & TreeData::TREE_FLAG_HAS_ORACLE) {
        out = out.substitute_with([&canonical](Tree d) {
            if (auto t = std::get_if<TreeOracle>(d.ptr)) {
                if (auto o = t->oracle->optimized(canonical)) {
                    return new TreeData(TreeOracle { std::move(o) });
                }
            }
            return static_cast<TreeData*>(nullptr);
        });
    }

    // Collect and collapse affine equations, e.g. 2*X + 3*X --> 5*X
    out = out.collect_affine();

    // And we're done!
    return out;
}

Tree Tree::unique() const {
    // The canonical tree for each Key is stored here
    std::map<Data::Key, Tree> canonical;

    return unique_helper(canonical);
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
    return substitute_with([&](Tree t) {
        auto itr = remap.find(t.ptr);

        // We can return the raw pointer because it's owned by remap above
        return itr == remap.end()
            ? nullptr
            : itr->second.ptr;
    });
}

size_t Tree::size() const {
    std::unordered_set<Id> seen;

    std::stack<const Data*> todo;
    todo.push(get());
    // Count how many branches reach to a given node.
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
