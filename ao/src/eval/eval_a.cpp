#include "ao/eval/eval_a.hpp"

namespace Kernel {

constexpr size_t ArrayEvaluator::N;

ArrayEvaluator::ArrayEvaluator(Tape& t)
    : ArrayEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

ArrayEvaluator::ArrayEvaluator(
        Tape& t, const std::map<Tree::Id, float>& vars)
    : tape(t), f(t.num_clauses + 1, N)
{
    // Unpack variables into result array
    for (auto& v : vars)
    {
        f.row(tape.vars.right.at(v.first)) = v.second;
    }

    // Unpack constants into result array
    for (auto& c : t.constants)
    {
        f.row(c.first) = c.second;
    }
}

float ArrayEvaluator::eval(const Eigen::Vector3f& pt)
{
    set(pt, 0);
    return values(1)(0);
}

float ArrayEvaluator::evalAndPush(const Eigen::Vector3f& pt)
{
    auto out = eval(pt);
    tape.push([&](Opcode::Opcode op, Clause::Id /* id */,
                  Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::MAX)
        {
            if (f(a, 0) > f(b, 0))
            {
                return Tape::KEEP_A;
            }
            else if (f(b, 0) > f(a, 0))
            {
                return Tape::KEEP_B;
            }
        }
        else if (op == Opcode::MIN)
        {
            if (f(a, 0) > f(b, 0))
            {
                return Tape::KEEP_B;
            }
            else if (f(b, 0) > f(a, 0))
            {
                return Tape::KEEP_A;
            }
        }
        return Tape::KEEP_BOTH;
    }, Tape::SPECIALIZED);
    return out;
}

float ArrayEvaluator::baseEval(const Eigen::Vector3f& pt)
{
    return tape.baseEval<ArrayEvaluator, float>(*this, pt);
}

Eigen::Block<decltype(ArrayEvaluator::f), 1, Eigen::Dynamic>
ArrayEvaluator::values(size_t _count)
{
    count = _count;
    return f.block<1, Eigen::Dynamic>(tape.rwalk(
        [=](Opcode::Opcode op, Clause::Id id,
            Clause::Id a, Clause::Id b)
            { evalClause(op, id, a, b); }),
        0, 1, count);
}

////////////////////////////////////////////////////////////////////////////////

bool ArrayEvaluator::setVar(Tree::Id var, float value)
{
    auto v = tape.vars.right.find(var);
    if (v != tape.vars.right.end())
    {
        bool changed = f(v->second, 0) != value;
        f.row(v->second) = value;
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

void ArrayEvaluator::evalClause(Opcode::Opcode op, Clause::Id id,
                                Clause::Id a, Clause::Id b)
{
#define out f.row(id).head(count)
#define a f.row(a).head(count)
#define b f.row(b).head(count)
    switch (op)
    {
        case Opcode::ADD:
            out = a + b;
            break;
        case Opcode::MUL:
            out = a * b;
            break;
        case Opcode::MIN:
            out = a.cwiseMin(b);
            break;
        case Opcode::MAX:
            out = a.cwiseMax(b);
            break;
        case Opcode::SUB:
            out = a - b;
            break;
        case Opcode::DIV:
            out = a / b;
            break;
        case Opcode::ATAN2:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = atan2(a(i), b(i));
            }
            break;
        case Opcode::POW:
            out = a.pow(b);
            break;
        case Opcode::NTH_ROOT:
            out = pow(a, 1.0f/b);
            break;
        case Opcode::MOD:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = std::fmod(a(i), b(i));
                while (out(i) < 0)
                {
                    out(i) += b(i);
                }
            }
            break;
        case Opcode::NANFILL:
            out = a.isNaN().select(b, a);
            break;

        case Opcode::SQUARE:
            out = a * a;
            break;
        case Opcode::SQRT:
            out = sqrt(a);
            break;
        case Opcode::NEG:
            out = -a;
            break;
        case Opcode::SIN:
            out = sin(a);
            break;
        case Opcode::COS:
            out = cos(a);
            break;
        case Opcode::TAN:
            out = tan(a);
            break;
        case Opcode::ASIN:
            out = asin(a);
            break;
        case Opcode::ACOS:
            out = acos(a);
            break;
        case Opcode::ATAN:
            out = atan(a);
            break;
        case Opcode::EXP:
            out = exp(a);
            break;
        case Opcode::ABS:
            out = abs(a);
            break;
        case Opcode::RECIP:
            out = 1 / a;
            break;

        case Opcode::CONST_VAR:
            out = a;
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
#undef out
#undef a
#undef b
}

}   // namespace Kernel

