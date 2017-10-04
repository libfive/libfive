#include "ao/eval/eval_interval.hpp"

namespace Kernel {

IntervalEvaluator::IntervalEvaluator(Tape& t)
    : IntervalEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

IntervalEvaluator::IntervalEvaluator(
        Tape& t, const std::map<Tree::Id, float>& vars)
    : tape(t)
{
    i.resize(t.num_clauses + 1);

    // Unpack variables into result array
    for (auto& v : vars)
    {
        i[tape.vars.right.at(v.first)] = v.second;
    }

    // Unpack constants into result array
    for (auto& c : t.constants)
    {
        i[c.first] = c.second;
    }
}

Interval::I IntervalEvaluator::eval(const Eigen::Vector3f& lower,
                                    const Eigen::Vector3f& upper)
{
    i[tape.X] = {lower.x(), upper.x()};
    i[tape.Y] = {lower.y(), upper.y()};
    i[tape.Z] = {lower.z(), upper.z()};

    return i[tape.rwalk([=](Opcode::Opcode op, Clause::Id id,
                            Clause::Id a, Clause::Id b)
            { evalClause(op, id, a, b); })];
}

Interval::I IntervalEvaluator::evalAndPush(const Eigen::Vector3f& lower,
                                           const Eigen::Vector3f& upper)
{
    auto out = eval(lower, upper);

    tape.push([&](Opcode::Opcode op, Clause::Id /* id */,
                  Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::MAX)
        {
            if (i[a].lower() > i[b].upper())
            {
                return Tape::KEEP_A;
            }
            else if (i[b].lower() > i[a].upper())
            {
                return Tape::KEEP_B;
            }
        }
        else if (op == Opcode::MIN)
        {
            if (i[a].lower() > i[b].upper())
            {
                return Tape::KEEP_B;
            }
            else if (i[b].lower() > i[a].upper())
            {
                return Tape::KEEP_A;
            }
        }
        return Tape::KEEP_BOTH;
    },
        Tape::INTERVAL,
        {{i[tape.X].lower(), i[tape.Y].lower(), i[tape.Z].lower()},
         {i[tape.X].upper(), i[tape.Y].upper(), i[tape.Z].upper()}});
    return out;
}

////////////////////////////////////////////////////////////////////////////////

bool IntervalEvaluator::setVar(Tree::Id var, float value)
{
    auto v = tape.vars.right.find(var);
    if (v != tape.vars.right.end())
    {
        bool changed = i[v->second] != value;
        i[v->second] = value;
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

static Interval::I interval_math_dispatcher(Opcode::Opcode op,
        Interval::I a, Interval::I b)
{
    switch (op) {
        case Opcode::ADD:
            return a + b;
        case Opcode::MUL:
            return a * b;
        case Opcode::MIN:
            return boost::numeric::min(a, b);
        case Opcode::MAX:
            return boost::numeric::max(a, b);
        case Opcode::SUB:
            return a - b;
        case Opcode::DIV:
            return a / b;
        case Opcode::ATAN2:
            return atan2(a, b);
        case Opcode::POW:
            return boost::numeric::pow(a, b.lower());
        case Opcode::NTH_ROOT:
            return boost::numeric::nth_root(a, b.lower());
        case Opcode::MOD:
            return Interval::I(0.0f, b.upper()); // YOLO
        case Opcode::NANFILL:
            return (std::isnan(a.lower()) || std::isnan(a.upper())) ? b : a;

        case Opcode::SQUARE:
            return boost::numeric::square(a);
        case Opcode::SQRT:
            return boost::numeric::sqrt(a);
        case Opcode::NEG:
            return -a;
        case Opcode::SIN:
            return boost::numeric::sin(a);
        case Opcode::COS:
            return boost::numeric::cos(a);
        case Opcode::TAN:
            return boost::numeric::tan(a);
        case Opcode::ASIN:
            return boost::numeric::asin(a);
        case Opcode::ACOS:
            return boost::numeric::acos(a);
        case Opcode::ATAN:
            // If the interval has an infinite bound, then return the largest
            // possible output interval (of +/- pi/2).  This rescues us from
            // situations where we do atan(y / x) and the behavior of the
            // interval changes if you're approaching x = 0 from below versus
            // from above.
            return (std::isinf(a.lower()) || std::isinf(a.upper()))
                ? Interval::I(-M_PI/2, M_PI/2)
                : boost::numeric::atan(a);
        case Opcode::EXP:
            return boost::numeric::exp(a);
        case Opcode::ABS:
            return boost::numeric::abs(a);
        case Opcode::RECIP:
            return Interval::I(1,1) / a;

        case Opcode::CONST_VAR:
            return a;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
    return Interval::I();
}

void IntervalEvaluator::evalClause(Opcode::Opcode op, Clause::Id id,
                                   Clause::Id a, Clause::Id b)
{
    i[id] = interval_math_dispatcher(op, i[a], i[b]);
}

}   // namespace Kernel
