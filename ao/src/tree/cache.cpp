#include <cassert>

#include "ao/tree/cache.hpp"
#include "ao/eval/eval_point.hpp"

namespace Kernel {

// Static class variables
std::recursive_mutex Cache::mut;
Cache Cache::_instance;

Cache::Node Cache::constant(float v)
{
    auto f = constants.find(v);
    if (f == constants.end())
    {
        Node out(new Tree::Tree_ {
            Opcode::CONST,
            Tree::FLAG_LOCATION_AGNOSTIC,
            0, // rank
            v, // value
            nullptr,
            nullptr });
        constants.insert({v, out});
        return out;
    }
    else
    {
        assert(!f->second.expired());
        return f->second.lock();
    }
}

Cache::Node Cache::operation(Opcode::Opcode op, Cache::Node lhs,
                             Cache::Node rhs, bool simplify)
{
    // These are opcodes that you're not allowed to use here
    assert(op != Opcode::CONST &&
           op != Opcode::INVALID &&
           op != Opcode::LAST_OP);

    // See if we can simplify the expression, either because it's an identity
    // operation (e.g. X + 0) or a commutative expression to be balanced
    if (simplify)
    {
#define CHECK_RETURN(func) { auto t = func(op, lhs, rhs); if (t.get() != nullptr) { return t; }}
        CHECK_RETURN(checkIdentity);
        CHECK_RETURN(checkCommutative);
    }

    Key k(op, lhs.get(), rhs.get());

    auto found = ops.find(k);
    if (found == ops.end())
    {
        // Construct a new operation node
        Node out(new Tree::Tree_ {
            op,

            // Flags
            (uint8_t)
            (((!lhs.get() || (lhs->flags & Tree::FLAG_LOCATION_AGNOSTIC)) &&
              (!rhs.get() || (rhs->flags & Tree::FLAG_LOCATION_AGNOSTIC)) &&
               op != Opcode::VAR_X &&
               op != Opcode::VAR_Y &&
               op != Opcode::VAR_Z)
                  ? Tree::FLAG_LOCATION_AGNOSTIC : 0),

            // Rank
            std::max(lhs.get() ? lhs->rank + 1 : 0,
                     rhs.get() ? rhs->rank + 1 : 0),

            // Value
            std::nanf(""),

            // Arguments
            lhs,
            rhs });

        // Store a weak pointer to this new Node
        ops.insert({k, out});

        // If both sides of the operation are constant, then build up a
        // temporary Evaluator in order to get a constant value out
        // (out will be GC'd immediately when it goes out of scope)
        if ((lhs.get() || rhs.get()) &&
            (!lhs.get() || lhs->op == Opcode::CONST) &&
            (!rhs.get() || rhs->op == Opcode::CONST))
        {
            // Here, we construct a Tree manually to avoid a recursive loop,
            // then pass it immediately into a dummy Evaluator
            PointEvaluator e(std::make_shared<Tape>(Tree(out)));
            return constant(e.eval({0,0,0}));
        }
        else
        {
            return out;
        }
    }
    else
    {
        assert(!found->second.expired());
        return found->second.lock();
    }
}


Cache::Node Cache::var()
{
    return Node(new Tree::Tree_ {
        Opcode::VAR,
        Tree::FLAG_LOCATION_AGNOSTIC,
        0, // rank
        std::nanf(""), // value
        nullptr,
        nullptr});
}

void Cache::del(float v)
{
    auto c = constants.find(v);
    assert(c != constants.end());
    assert(c->second.expired());
    constants.erase(c);
}

void Cache::del(Opcode::Opcode op, Node lhs, Node rhs)
{
    auto o = ops.find(Key(op, lhs.get(), rhs.get()));
    assert(o != ops.end());
    assert(o->second.expired());
    ops.erase(o);
}

Cache::Node Cache::checkIdentity(Opcode::Opcode op, Cache::Node a, Cache::Node b)
{
    if (Opcode::args(op) != 2)
    {
        return Node();
    }

    // Pull op-codes from both branches
    const auto op_a = a->op;
    const auto op_b = b->op;

    // Special cases to handle identity operations
    switch (op)
    {
        case Opcode::ADD:
            if (op_a == Opcode::CONST && a->value == 0)
            {
                return b;
            }
            else if (op_b == Opcode::CONST && b->value == 0)
            {
                return a;
            }
            break;
        case Opcode::SUB:
            if (op_a == Opcode::CONST && a->value == 0)
            {
                return operation(Opcode::NEG, b);
            }
            else if (op_b == Opcode::CONST && b->value == 0)
            {
                return a;
            }
            break;
        case Opcode::MUL:
            if (op_a == Opcode::CONST)
            {
                if (a->value == 0)
                {
                    return a;
                }
                else if (a->value == 1)
                {
                    return b;
                }
            }
            if (op_b == Opcode::CONST)
            {
                if (b->value == 0)
                {
                    return b;
                }
                else if (b->value == 1)
                {
                    return a;
                }
            }
            break;
        case Opcode::POW:   // FALLTHROUGH
        case Opcode::NTH_ROOT:
            if (op_b == Opcode::CONST && b->value == 1)
            {
                return a;
            }
            break;

        default:
            break;
    }
    return Node();
}

Cache::Node Cache::checkCommutative(Opcode::Opcode op, Cache::Node a, Cache::Node b)
{
    if (Opcode::isCommutative(op))
    {
        const auto al = a->lhs ? a->lhs->rank : 0;
        const auto ar = a->rhs ? a->rhs->rank : 0;
        const auto bl = b->lhs ? b->lhs->rank : 0;
        const auto br = b->rhs ? b->rhs->rank : 0;

        if (a->op == op)
        {
            if (al > b->rank)
            {
                return operation(op, a->lhs, operation(op, a->rhs, b));
            }
            else if (ar > b->rank)
            {
                return operation(op, a->rhs, operation(op, a->lhs, b));
            }
        }
        else if (b->op == op)
        {
            if (bl > a->rank)
            {
                return operation(op, b->lhs, operation(op, b->rhs, a));
            }
            else if (br > a->rank)
            {
                return operation(op, b->rhs, operation(op, b->lhs, a));
            }
        }
    }
    return 0;
}

}   // namespace Kernel
