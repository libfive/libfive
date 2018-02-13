/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "libfive/eval/eval_interval_array.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

constexpr size_t IntervalArrayEvaluator::N;

IntervalArrayEvaluator::IntervalArrayEvaluator(std::shared_ptr<Tape> t)
    : IntervalArrayEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

IntervalArrayEvaluator::IntervalArrayEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(t, vars), i(tape->num_clauses + 1, N)
{
    // Unpack variables into result array
    for (auto& v : t->vars.right)
    {
        auto var = vars.find(v.first);
        i.row(v.second) = (var != vars.end())
            ? Interval::I(var->second, var->second)
            : Interval::I(0, 0);
    }

    // Unpack constants into result array
    for (auto& c : tape->constants)
    {
        i.row(c.first) = Interval::I(c.second, c.second);
    }
}

Interval::I IntervalArrayEvaluator::eval(const Eigen::Vector3f& lower,
                                         const Eigen::Vector3f& upper)
{
    // Split the root interval into eight sub-intervals
    // TODO: Only split as many as is necessary (e.g. into 4 in the 2D case)
    const Eigen::Vector3f center = (lower + upper) / 2;
    for (unsigned j=0; j < N; ++j)
    {
        i(tape->X, j) = (j & Axis::X) ? Interval::I(lower.x(),  center.x())
                                      : Interval::I(center.x(), upper.x());
        i(tape->Y, j) = (j & Axis::Y) ? Interval::I(lower.y(),  center.y())
                                      : Interval::I(center.y(), upper.y());
        i(tape->Z, j) = (j & Axis::Z) ? Interval::I(lower.z(),  center.z())
                                      : Interval::I(center.z(), upper.z());
    }

    auto index = tape->rwalk(*this);
    Interval::I out(std::numeric_limits<float>::infinity(),
                   -std::numeric_limits<float>::infinity());

    // Find the union of all of the intervals
    for (unsigned j=0; j < N; ++j)
    {
        out = {fmin(out.lower(), i(index, j).lower()),
               fmax(out.upper(), i(index, j).upper())};
    }
    return out;
}

Interval::I IntervalArrayEvaluator::evalAndPush(const Eigen::Vector3f& lower,
                                                const Eigen::Vector3f& upper)
{
    auto out = eval(lower, upper);
    tape->push([&](Opcode::Opcode op, Clause::Id /* id */,
                  Clause::Id a, Clause::Id b)
    {
        uint8_t result = 0;
        const uint8_t KEEP_A = 1;
        const uint8_t KEEP_B = 2;

        // active if it is decisively above or below the other branch.
        if (op == Opcode::MAX)
        {
            for (unsigned j=0; j < N && result != (KEEP_A | KEEP_B); ++j)
            {
                if (i(a, j).lower() > i(b, j).upper())
                {
                    result |= KEEP_A;
                }
                else if (i(b, j).lower() > i(a, j).upper())
                {
                    result |= KEEP_B;
                }
                else
                {
                    result |= KEEP_A | KEEP_B;
                }
            }
        }
        else if (op == Opcode::MIN)
        {
            for (unsigned j=0; j < N && result != (KEEP_A | KEEP_B); ++j)
            {
                if (i(a, j).lower() > i(b, j).upper())
                {
                    result |= KEEP_B;
                }
                else if (i(b, j).lower() > i(a, j).upper())
                {
                    result |= KEEP_A;
                }
                else
                {
                    result |= KEEP_A | KEEP_B;
                }
            }
        }
        switch (result)
        {
            case 0:         return Tape::KEEP_ALWAYS;
            case KEEP_A:    return Tape::KEEP_A;
            case KEEP_B:    return Tape::KEEP_B;
            default:        return Tape::KEEP_BOTH;
        }
    }, Tape::SPECIALIZED);
    return out;
}

////////////////////////////////////////////////////////////////////////////////

bool IntervalArrayEvaluator::setVar(Tree::Id var, float value)
{
    auto v = tape->vars.right.find(var);
    if (v != tape->vars.right.end())
    {
        bool changed = i(v->second, 0) != value;
        i.row(v->second) = value;
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

void IntervalArrayEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                        Clause::Id a, Clause::Id b)
{
// TODO: Figure out how to make Eigen parallelize these
#define out i(id, j)
#define a i(a, j)
#define b i(b, j)
#define EVAL_LOOP for (unsigned j=0; j < N; ++j)
    switch (op)
    {
        case Opcode::ADD:
            EVAL_LOOP
            out = a + b;
            break;
        case Opcode::MUL:
            EVAL_LOOP
            out = a * b;
            break;
        case Opcode::MIN:
            EVAL_LOOP
            out = boost::numeric::min(a, b);
            break;
        case Opcode::MAX:
            EVAL_LOOP
            out = boost::numeric::max(a, b);
            break;
        case Opcode::SUB:
            EVAL_LOOP
            out = a - b;
            break;
        case Opcode::DIV:
            EVAL_LOOP
            out = a / b;
            break;
        case Opcode::ATAN2:
            EVAL_LOOP
            out = atan2(a, b);
            break;
        case Opcode::POW:
            EVAL_LOOP
            out = boost::numeric::pow(a, b.lower());
            break;
        case Opcode::NTH_ROOT:
            EVAL_LOOP
            out = boost::numeric::nth_root(a, b.lower());
            break;
        case Opcode::MOD:
            EVAL_LOOP
            out = Interval::I(0.0f, b.upper()); // YOLO
            break;
        case Opcode::NANFILL:
            EVAL_LOOP
            out = (std::isnan(a.lower()) || std::isnan(a.upper())) ? b : a;
            break;
        case Opcode::COMPARE:
            EVAL_LOOP {
            if      (a.upper() < b.lower()) out = Interval::I(-1, -1);
            else if (a.lower() > b.upper()) out = Interval::I( 1,  1);
            else                            out = Interval::I(-1,  1);
            }
            break;

        case Opcode::SQUARE:
            EVAL_LOOP
            out = boost::numeric::square(a);
            break;
        case Opcode::SQRT:
            EVAL_LOOP
            out = boost::numeric::sqrt(a);
            break;
        case Opcode::NEG:
            EVAL_LOOP
            out = -a;
            break;
        case Opcode::SIN:
            EVAL_LOOP
            out = boost::numeric::sin(a);
            break;
        case Opcode::COS:
            EVAL_LOOP
            out = boost::numeric::cos(a);
            break;
        case Opcode::TAN:
            EVAL_LOOP
            out = boost::numeric::tan(a);
            break;
        case Opcode::ASIN:
            EVAL_LOOP
            out = boost::numeric::asin(a);
            break;
        case Opcode::ACOS:
            EVAL_LOOP
            out = boost::numeric::acos(a);
            break;
        case Opcode::ATAN:
            // If the interval has an infinite bound, then return the largest
            // possible output interval (of +/- pi/2).  This rescues us from
            // situations where we do atan(y / x) and the behavior of the
            // interval changes if you're approaching x = 0 from below versus
            // from above.
            EVAL_LOOP
            out = (std::isinf(a.lower()) || std::isinf(a.upper()))
                ? Interval::I(-M_PI/2, M_PI/2)
                : boost::numeric::atan(a);
            break;
        case Opcode::EXP:
            EVAL_LOOP
            out = boost::numeric::exp(a);
            break;
        case Opcode::LOG:
            EVAL_LOOP
            out = boost::numeric::log(a);
            break;
        case Opcode::ABS:
            EVAL_LOOP
            out = boost::numeric::abs(a);
            break;
        case Opcode::RECIP:
            EVAL_LOOP
            out = Interval::I(1,1) / a;
            break;

        case Opcode::CONST_VAR:
            EVAL_LOOP
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

