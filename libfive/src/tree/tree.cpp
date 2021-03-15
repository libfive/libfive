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
#include <numeric>
#include <stack>
#include <unordered_set>

#include <boost/container/small_vector.hpp>

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
    // Special case if this tree isn't being freed here: we return early
    // to avoid allocating data on the heap.
    if (--ptr->refcount) {
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
        if (t == ptr || !--t->refcount) {
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
            } else if (auto d = std::get_if<TreeApply>(t)) {
                todo.push(std::exchange(d->target.ptr, nullptr));
                todo.push(std::exchange(d->value.ptr, nullptr));
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

Tree::Tree(double f)
    : Tree(static_cast<float>(f))
{
    // Nothing to do here
}

Tree::Tree(const Data* d, bool increment_refcount, uint32_t flags)
    : ptr(d), flags(flags)
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
    return Tree::nullary(Opcode::VAR_FREE);
}

Tree Tree::unary(Opcode::Opcode op, const Tree& lhs) {
    // We can only build unary operations with this function
    if (Opcode::args(op) != 1) {
        return invalid();
    }
    // Collapse constant operations
    else if (std::get_if<TreeConstant>(lhs.ptr)) {
        auto tmp = Tree(new Data(TreeUnaryOp { op, lhs }));
        ArrayEvaluator eval(tmp.with_flags(TREE_FLAG_IS_OPTIMIZED));

        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // abs is idempotent after abs() or square()
    else if (op == Opcode::OP_ABS) {
        if (auto v = std::get_if<TreeUnaryOp>(lhs.ptr)) {
            if (v->op == Opcode::OP_ABS || v->op == Opcode::OP_SQUARE) {
                return lhs;
            }
        }
    }
    // Double-negative returns the original
    else if (op == Opcode::OP_NEG) {
        if (auto v = std::get_if<TreeUnaryOp>(lhs.ptr)) {
            if (v->op == Opcode::OP_NEG) {
                return v->lhs;
            }
        }
    }
    // Default if we didn't fall into any special cases
    return Tree(new Data(TreeUnaryOp { op, lhs }));
}

Tree Tree::nullary(Opcode::Opcode op) {
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
        ArrayEvaluator eval(tmp.with_flags(TREE_FLAG_IS_OPTIMIZED));

        const float v = eval.value({0.0f, 0.0f, 0.0f});
        return Tree(v);
    }
    // Division by 1 is ignored
    else if (op == Opcode::OP_DIV) {
        if (auto v = std::get_if<TreeConstant>(rhs.ptr)) {
            if (v->value == 1.0f) {
                return lhs;
            }
        }
    }
    else if (op == Opcode::OP_ADD) {
        if (auto v = std::get_if<TreeConstant>(lhs.ptr)) {
            if (v->value == 0.0) {
                return rhs;
            }
        } else if ((v = std::get_if<TreeConstant>(rhs.ptr))) {
            if (v->value == 0.0) {
                return lhs;
            }
        } else if (auto v = std::get_if<TreeUnaryOp>(lhs.ptr)) {
            if (v->op == Opcode::OP_NEG) {
                return rhs - v->lhs;
            }
        } else if (auto v = std::get_if<TreeUnaryOp>(rhs.ptr)) {
            if (v->op == Opcode::OP_NEG) {
                return lhs - v->lhs;
            }
        }
    } else if (op == Opcode::OP_SUB) {
        if (auto v = std::get_if<TreeConstant>(lhs.ptr)) {
            if (v->value == 0.0) {
                return -rhs;
            }
        } else if ((v = std::get_if<TreeConstant>(rhs.ptr))) {
            if (v->value == 0.0) {
                return lhs;
            }
        } else if (auto v = std::get_if<TreeUnaryOp>(rhs.ptr)) {
            if (v->op == Opcode::OP_NEG) {
                return lhs + v->lhs;
            }
        }
    } else if (op == Opcode::OP_MUL) {
        if (auto v = std::get_if<TreeConstant>(lhs.ptr)) {
            if (v->value == 0) {
                return lhs;
            } else if (v->value == 1) {
                return rhs;
            } else if (v->value == -1) {
                return -rhs;
            }
        } else if ((v = std::get_if<TreeConstant>(rhs.ptr))) {
            if (v->value == 0) {
                return rhs;
            } else if (v->value == 1) {
                return lhs;
            } else if (v->value == -1) {
                return -lhs;
            }
        } else if (lhs.id() == rhs.id()) {
            return square(lhs);
        }
    } else if (op == Opcode::OP_NTH_ROOT || op == Opcode::OP_POW) {
        if (auto v = std::get_if<TreeConstant>(rhs.ptr)) {
            if (v->value == 1.0f) {
                return lhs;
            }
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

Tree Tree::one() {
    static auto o = Tree(new Data(TreeConstant { 1.0f }));
    return o;
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

bool Tree::eq(const Tree& other) const {
    std::unordered_map<Data::Key, Tree> canonical;

    return cooptimize(canonical) == other.cooptimize(canonical);
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
            } else if (auto t = std::get_if<TreeApply>(*d)) {
                s << "(apply ";
                todo.push(')');
                ops.push(Opcode::INVALID); // dummy op for remap
                todo.push(t->target.get());
                todo.push(' ');
                todo.push(t->value.get());
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
    // If this remap is a no-op, then skip it
    if (X == Tree::X() && Y == Tree::Y() && Z == Tree::Z()) {
        return Tree(*this);
    }
    // Only insert a remap operation if the tree has things that could be
    // remapped (i.e. x/y/z coordinates or an oracle clause).
    else if (ptr->flags & (TreeData::TREE_FLAG_HAS_XYZ |
                           TreeData::TREE_FLAG_HAS_ORACLE))
    {
        return Tree(new Data(TreeRemap { X, Y, Z, *this }));
    } else {
        return *this;
    }
}

Tree Tree::apply(Tree var, Tree value) const {
    if (var->op() != Opcode::VAR_FREE) {
        throw TreeData::ApplyException();
    }
    return Tree(new Data(TreeApply { var, value, *this }));
}

Tree Tree::flatten() const {
    if (!(ptr->flags & TreeData::TREE_FLAG_HAS_REMAP)) {
        return *this;
    }

    /*  When going down:
     *  ======================
     *  If t is a nullary operation:
     *      If there's a remap for it, push the remapped value to the output
     *      Otherwise, push t to the output stack directly
     *
     *  If t is a remap, then pushes
     *      Up(t->tree)
     *      Down(t->z)
     *      Down(t->y)
     *      Down(t->x)  [executed first]
     *  to the task stack
     *
     *  If t is an apply, then pushes
     *      Up(t->tree)
     *      Down(t->value) [executed first]
     *
     *  If t has children, pushes Up(t) to the task stack, followed by
     *  Down(t->lhs/rhs) for each child.
     *
     *  Otherwise, pushes t to the output stack immediately
     *
     *  When going up:
     *  ======================
     *  If t has children, then pops them from the out stack and pushes
     *  a modified version of t to the out stack.
     *
     *  Otherwise, if t is a remap, then pops x', y', and z' from the output
     *  stack then pushes Down { x', y', z', t->m, t->t } to the task stack.
     *
     *  Finally, if t is an Apply, then pops the modified value from the
     *  output stack, then pushes Down { x, y, z, m', t->t } to the task stack
     */
    using MapPointer = std::shared_ptr<std::unordered_map<const Data*, Tree>>;
    struct Task {
        enum { DOWN, UP } v;
        const Data* t;
        MapPointer m;
    };
    std::stack<Task> todo;
    std::stack<Tree> out;

    todo.push({Task::DOWN, ptr, nullptr});

    while (todo.size()) {
        auto k = todo.top();
        todo.pop();

        auto get = [&](const Data* t) {
            if (k.m) {
                const auto k_itr = k.m->find(t);
                if (k_itr != k.m->end()) {
                    return k_itr->second;
                }
            }
            return Tree(t);
        };

        if (k.v == Task::DOWN) {
            if (std::get_if<TreeNonaryOp>(k.t)) {
                out.push(get(k.t));
            } else if (auto t=std::get_if<TreeUnaryOp>(k.t)) {
                todo.push({Task::UP, k.t, k.m});
                todo.push({Task::DOWN, t->lhs.ptr, k.m});
            } else if (auto t=std::get_if<TreeBinaryOp>(k.t)) {
                todo.push({Task::UP, k.t, k.m});
                todo.push({Task::DOWN, t->lhs.ptr, k.m});
                todo.push({Task::DOWN, t->rhs.ptr, k.m});
            } else if (std::get_if<TreeConstant>(k.t)) {
                out.push(Tree(k.t));
            } else if (auto t=std::get_if<TreeOracle>(k.t)) {
                const Tree new_oracle(t->oracle->remap(Tree(k.t),
                            get(Tree::X().ptr),
                            get(Tree::Y().ptr),
                            get(Tree::Z().ptr)));
                out.push(new_oracle);
            } else if (auto t=std::get_if<TreeRemap>(k.t)) {
                todo.push({Task::UP, k.t, k.m});
                todo.push({Task::DOWN, t->z.ptr, k.m});
                todo.push({Task::DOWN, t->y.ptr, k.m});
                todo.push({Task::DOWN, t->x.ptr, k.m});
            } else if (auto t=std::get_if<TreeApply>(k.t)) {
                todo.push({Task::UP, k.t, k.m});
                todo.push({Task::DOWN, t->value.ptr, k.m});
            }
        } else if (k.v == Task::UP) {
            if (auto t=std::get_if<TreeUnaryOp>(k.t)) {
                const auto lhs = out.top();
                out.pop();
                if (lhs.ptr != t->lhs.ptr) {
                    out.push(Tree::unary(t->op, lhs));
                } else {
                    out.push(Tree(k.t));
                }
            } else if (auto t=std::get_if<TreeBinaryOp>(k.t)) {
                const auto lhs = out.top();
                out.pop();
                const auto rhs = out.top();
                out.pop();
                if (lhs.ptr != t->lhs.ptr || rhs.ptr != t->rhs.ptr) {
                    out.push(Tree::binary(t->op, lhs, rhs));
                } else {
                    out.push(Tree(k.t));
                }
            } else if (auto t=std::get_if<TreeRemap>(k.t)) {
                // Update the map with X/Y/Z values
                auto m = std::make_shared<std::unordered_map<const Data*, Tree>>(
                        k.m ? *(k.m) : std::unordered_map<const Data*, Tree>());

                m->insert_or_assign(Tree::Z().ptr, out.top());
                out.pop();
                m->insert_or_assign(Tree::Y().ptr, out.top());
                out.pop();
                m->insert_or_assign(Tree::X().ptr, out.top());
                out.pop();

                todo.push({Task::DOWN, t->t.ptr, m});
            } else if (auto t=std::get_if<TreeApply>(k.t)) {
                // Update the map with the new value
                auto m = std::make_shared<MapPointer::element_type>(
                        k.m ? *(k.m) : MapPointer::element_type());
                m->insert_or_assign(t->target.ptr, out.top());
                out.pop();
                todo.push({Task::DOWN, t->t.ptr, m});
            } else {
                // We shouldn't ever get here, because Nonary, Constant,
                // Oracles, Remap, and Apply shouldn't be pushed as Up nodes.
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
        // These cases should never happen because of the check above
        } else if (std::get_if<TreeRemap>(next)) {
            assert(false);
        } else if (std::get_if<TreeApply>(next)) {
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
    return Tree(ptr, false, 0); // Don't increment refcount
}

Tree Tree::with_flags(uint32_t extra_flags) const {
    return Tree(ptr, true, flags | extra_flags);
}

Tree Tree::without_flags(uint32_t clear_flags) const {
    return Tree(ptr, true, flags & (~clear_flags));
}

Tree Tree::optimized() const {
    std::unordered_map<Data::Key, Tree> canonical;
    return optimized_helper(canonical);
}

Tree Tree::optimized_helper(
        std::unordered_map<Data::Key, Tree>& canonical) const
{
    if (flags & TREE_FLAG_IS_OPTIMIZED) {
        return *this;
    } else if (ptr->flags & TreeData::TREE_FLAG_HAS_REMAP) {
        return flatten().optimized_helper(canonical);
    }

    using AffineMap = std::unordered_map<Tree, float>;
    std::stack<std::optional<AffineMap>> affine;
    affine.push(std::nullopt);

    struct CommutativeList {
        CommutativeList(Opcode::Opcode op) : op(op) {}

        boost::container::small_vector<Tree, 4> list;
        Opcode::Opcode op;
    };
    std::stack<std::optional<CommutativeList>> commutative;
    commutative.push(std::nullopt);

    struct Down {
        const Data* t;
        const float scale;
    };
    struct Up {
        const Data* t;
        const float scale;
    };
    struct UpAffine {
        // Marker struct which pops the top affine map, then pushes it to
        // either the output or a containing commutative list.
    };
    struct UpCommutative {
        // Marker struct which pops the top commutative list, then pushes it
        // to either the output stack or the affine stack
        const float scale;
    };

    using Task = std::variant<Down, Up, UpAffine, UpCommutative>;
    std::stack<Task> todo;
    todo.push(Down { ptr, 1 });

    std::stack<Tree> out;

    // Deduplicator function, which returns a raw pointer
    auto uniq = [&canonical](const Tree t) {
        const auto key = t->key();
        const auto k_itr = canonical.find(key);
        // We already have a canonical version of this tree,
        // so remapped this tree to the canonical version and keep going.
        if (k_itr != canonical.end()) {
            return k_itr->second;
        } else {
            canonical.insert(k_itr, {key, t});
            return t;
        }
    };

    while (todo.size()) {
        const auto t = todo.top();
        todo.pop();
        if (auto d = std::get_if<Down>(&t)) {
            using namespace Opcode;

            // Normally, we're descending throught the tree without an affine
            // or commutative accumulator active.  mark_affine/commutative()
            // undo these defaults.
            todo.push(Up { d->t, d->scale });
            commutative.push(std::nullopt);
            affine.push(std::nullopt);

            bool could_be_affine = false;
            auto mark_affine = [&]() {
                todo.pop();
                affine.pop();
                const bool has_map = affine.top().has_value();
                if (!has_map) {
                    affine.push(AffineMap());
                    todo.push(UpAffine { });
                } else {
                    commutative.pop();
                }
                could_be_affine = true;
            };
            bool could_be_commutative = false;
            auto mark_commutative = [&](libfive::Opcode::Opcode op) {
                todo.pop();
                commutative.pop();
                // If we've got a commutative list available, then the most
                // recent operation on the todo stack must be an UpCommutative,
                // since that's what consumes the list.
                const bool has_list = commutative.top().has_value();
                if (!has_list || (*commutative.top()).op != op) {
                    commutative.push(CommutativeList(op));
                    todo.push(UpCommutative { d->scale });
                } else {
                    affine.pop();
                }
                could_be_commutative = true;
            };

            if (auto g = std::get_if<TreeUnaryOp>(d->t)) {
                if (g->op == OP_NEG) {
                    mark_affine();
                    todo.push(Down { g->lhs.ptr, -d->scale });
                }
            } else if (auto g = std::get_if<TreeBinaryOp>(d->t)) {
                if (g->op == OP_ADD) {
                    mark_affine();
                    todo.push(Down { g->lhs.ptr, d->scale });
                    todo.push(Down { g->rhs.ptr, d->scale });
                } else if (g->op == OP_SUB) {
                    mark_affine();
                    todo.push(Down { g->lhs.ptr, d->scale });
                    todo.push(Down { g->rhs.ptr, -d->scale });
                } else if (g->op == OP_MUL) {
                    if (auto v = std::get_if<TreeConstant>(g->lhs.ptr)) {
                        mark_affine();
                        todo.push(Down { g->rhs.ptr, v->value * d->scale });
                    } else if ((v = std::get_if<TreeConstant>(g->rhs.ptr))) {
                        mark_affine();
                        todo.push(Down { g->lhs.ptr, v->value * d->scale });
                    } else {
                        mark_commutative(OP_MUL);
                    }
                } else if (g->op == OP_DIV) {
                    if (auto v = std::get_if<TreeConstant>(g->rhs.ptr)) {
                        mark_affine();
                        todo.push(Down { g->lhs.ptr, d->scale / v->value });
                    }
                } else if (g->op == OP_MIN || g->op == OP_MAX) {
                    mark_commutative(g->op);
                }
            }

            if (!could_be_affine) {
                if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                    todo.push(Down { t->lhs.ptr, 1.0f });
                } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                    todo.push(Down { t->lhs.ptr, 1.0f });
                    todo.push(Down { t->rhs.ptr, 1.0f });
                } else if (std::get_if<TreeRemap>(d->t)) {
                    // This should be caught above
                    assert(false);
                } else if (std::get_if<TreeApply>(d->t)) {
                    // This should be caught above
                    assert(false);
                } else {
                    // Don't do anything; this tree will be moved to
                    // the output stack in the Up { } task
                }
            }
        } else if (auto d = std::get_if<Up>(&t)) {
            // Pop the std::nullopt added in Down
            affine.pop();
            commutative.pop();

            Tree new_self = uniq(Tree(d->t));
            if (auto t=std::get_if<TreeUnaryOp>(d->t)) {
                auto lhs = out.top();
                out.pop();
                if (lhs != t->lhs) {
                    new_self = uniq(Tree::unary(t->op, lhs));
                }
            } else if (auto t=std::get_if<TreeBinaryOp>(d->t)) {
                auto lhs = out.top();
                out.pop();
                auto rhs = out.top();
                out.pop();
                if (lhs != t->lhs || rhs != t->rhs) {
                    new_self = uniq(Tree::binary(t->op, lhs, rhs));
                }
            } else if (auto t=std::get_if<TreeOracle>(d->t)) {
                if (auto o = t->oracle->optimized(canonical)) {
                    new_self = uniq(Tree(std::move(o)));
                }
            }
            assert(new_self == uniq(new_self));

            if (affine.top().has_value()) {
                assert(!commutative.top().has_value());
                if (auto v = std::get_if<TreeConstant>(new_self.ptr)) {
                    (*affine.top())[Tree::one()] += d->scale * v->value;
                } else {
                    (*affine.top())[new_self] += d->scale;
                }
                // In this case, we don't push anything to the Out stack
                // because it will all be accumulated by the UpAffine task
            } else if (commutative.top().has_value()) {
                (*commutative.top()).list.push_back(new_self);
                // In this case, we don't push anything to the Out stack
                // because it will all be accumulated by the UpCommutative task
            } else {
                out.push(new_self);
            }
        } else if (std::get_if<UpAffine>(&t)) {
            assert(affine.top().has_value());
            auto map = std::move(*affine.top());
            affine.pop();
            commutative.pop();

            // Split the affine terms into positive and negative
            // components, so that we can subtract them.  This also puts
            // all affine terms in the form X * positive constant, which
            // encourages tree re-use.
            using namespace boost::container;
            small_vector<std::pair<const TreeData*, float>, 3> pos_;
            small_vector<std::pair<const TreeData*, float>, 3> neg_;
            for (auto& p: map) {
                if (p.second > 0.0f) {
                    pos_.push_back({p.first.ptr, p.second});
                } else if (p.second < 0.0f) {
                    neg_.push_back({p.first.ptr, -p.second});
                } else if (p.second) {
                    // Store NaN terms in the positive array
                    pos_.push_back({p.first.ptr, p.second});
                } else {
                    // Skip any zero terms here, since they don't matter
                }
            }

            // Sorting (after uniquifying) means that any affine terms that
            // share the same set of Trees will be ordered deterministicly,
            // which in turn helps with common subexpression elimination
            //
            // We sort by the multiplier first (to make deterministic unit
            // tests easier), then by the pointer (to make CSE work)
            auto sort_fn = [](auto a, auto b) -> bool {
                if (a.second != b.second) {
                    return a.second < b.second;
                } else {
                    return a.first < b.first;
                }
            };
            std::sort(pos_.begin(), pos_.end(), sort_fn);
            std::sort(neg_.begin(), neg_.end(), sort_fn);

            auto collapse = [&uniq](const auto& v) {
                auto itr = v.begin();
                Tree out = Tree::invalid();
                while (itr != v.end()) {
                    // Find the subsequence with the same multiplier
                    const float multiplier = itr->second;
                    if (multiplier == 0.0f) {
                        continue;
                    }

                    Tree t((itr++)->first);
                    assert(t == uniq(t) || t == one());

                    // Accumulate all subtrees with the same multiplier
                    while (itr != v.end() && itr->second == multiplier) {
                        t = uniq(t + Tree((itr++)->first));
                    }

                    // Apply the multiplication to accumulated t
                    if (multiplier != 1) {
                        if (t == one()) {
                            t = uniq(multiplier);
                        } else {
                            t = uniq(t * uniq(multiplier));
                        }
                    }
                    if (out.is_valid()) {
                        out = uniq(out + t);
                    } else {
                        out = t;
                    }
                }
                return out.is_valid() ? out : uniq(0.0f);
            };

            const auto pos = collapse(pos_);
            const auto neg = collapse(neg_);
            Tree new_self = Tree::invalid();
            if (pos.is_valid()) {
                if (neg.is_valid()) {
                    new_self = uniq(pos - neg);
                } else {
                    new_self = pos;
                }
            } else {
                if (neg.is_valid()) {
                    new_self = uniq(-neg);
                } else {
                    new_self = uniq(0.0f);
                }
            }
            assert(new_self == uniq(new_self));

            // Store the new result either in the output stack or the
            // commutative accumulator (if one is present).
            if (commutative.top().has_value()) {
                (*commutative.top()).list.push_back(new_self);
            } else {
                out.push(new_self);
            }
        } else if (auto d = std::get_if<UpCommutative>(&t)) {
            assert(commutative.top().has_value());
            auto comm = std::move(*commutative.top());
            assert(comm.list.size() >= 2);
            affine.pop();
            commutative.pop();

            // Sort so that commutative operations with the same list of
            // children will be deduplicated, since the arguments are
            // already deduplicated by now.
            std::sort(comm.list.begin(), comm.list.end());

            // If this is a min or max operation, then cancel out duplicates
            // (since min(f, f) = f)
            auto end = Opcode::isIdempotent(comm.op)
                ? std::unique(comm.list.begin(), comm.list.end())
                : comm.list.end();
            auto start = comm.list.begin();
            Tree new_self = std::accumulate(
                start + 1, end, *start,
                [&uniq, &comm](Tree a, Tree b) {
                    return uniq(Tree::binary(comm.op, a, b));
                });
            assert(new_self == uniq(new_self));

            if (affine.top().has_value()) {
                assert(!commutative.top().has_value());
                if (auto v = std::get_if<TreeConstant>(new_self.ptr)) {
                    (*affine.top())[Tree::one()] += d->scale * v->value;
                } else {
                    (*affine.top())[new_self] += d->scale;
                }
                // In this case, we don't push anything to the Out stack
                // because it will all be accumulated by the UpAffine task
            } else if (commutative.top().has_value()) {
                // It's possible for the commutative stack to have another
                // list right here, with a different opcode
                (*commutative.top()).list.push_back(new_self);
            } else {
                out.push(new_self);
            }
        }
    }
    assert(out.size() == 1);
    return out.top().with_flags(TREE_FLAG_IS_OPTIMIZED);
}

Tree Tree::cooptimize(std::unordered_map<Data::Key, Tree>& canonical) const {
    return without_flags(TREE_FLAG_IS_OPTIMIZED).optimized_helper(canonical);
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
        } else if (auto d = std::get_if<TreeApply>(next)) {
            todo.push(d->target.get());
            todo.push(d->value.get());
            todo.push(d->t.get());
        }
    }
    return seen.size();
}

}   // namespace libfive
