/*
Ao: a CAD kernel for modeling with implicit functions
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
#include "ao/eval/eval_affine.hpp"

namespace Kernel {

AffineEvaluator::AffineEvaluator(std::shared_ptr<Tape> t)
    : AffineEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

AffineEvaluator::AffineEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(t, vars), f(tape->num_clauses + 1, 1),
      e(tape->num_clauses + 1, tape->num_clauses + 1)
{
    e = 0;

    // Unpack variables into result array
    for (auto& v : t->vars.right)
    {
        auto var = vars.find(v.first);
        f(v.second) = (var != vars.end()) ? var->second : 0;
    }

    // Unpack constants into result array
    for (auto& c : tape->constants)
    {
        f(c.first) = c.second;
    }
}

Interval::I AffineEvaluator::eval(const Eigen::Vector3f& lower,
                                  const Eigen::Vector3f& upper)
{
    setAffine(tape->X, {lower.x(), upper.x()});
    setAffine(tape->Y, {lower.y(), upper.y()});
    setAffine(tape->Z, {lower.z(), upper.z()});

    return i(tape->rwalk(*this));
}

Interval::I AffineEvaluator::i(size_t c) const
{
    return {f(c) - e.row(c).sum(), f(c) + e.row(c).sum()};
}

Interval::I AffineEvaluator::evalAndPush(const Eigen::Vector3f& lower,
                                         const Eigen::Vector3f& upper)
{
    auto out = eval(lower, upper);

    tape->push([&](Opcode::Opcode op, Clause::Id /* id */,
                  Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::MAX)
        {
            if (i(a).lower() > i(b).upper())
            {
                return Tape::KEEP_A;
            }
            else if (i(b).lower() > i(a).upper())
            {
                return Tape::KEEP_B;
            }
        }
        else if (op == Opcode::MIN)
        {
            if (i(a).lower() > i(b).upper())
            {
                return Tape::KEEP_B;
            }
            else if (i(b).lower() > i(a).upper())
            {
                return Tape::KEEP_A;
            }
        }
        return Tape::KEEP_BOTH;
    },
        Tape::INTERVAL,
        {{i(tape->X).lower(), i(tape->Y).lower(), i(tape->Z).lower()},
         {i(tape->X).upper(), i(tape->Y).upper(), i(tape->Z).upper()}});
    return out;
}

////////////////////////////////////////////////////////////////////////////////

bool AffineEvaluator::setVar(Tree::Id var, float value)
{
    auto v = tape->vars.right.find(var);
    if (v != tape->vars.right.end())
    {
        bool changed = f(v->second) != value;
        f(v->second) = value;
        return changed;
    }
    else
    {
        return false;
    }
}

void AffineEvaluator::setAffine(Clause::Id id, Interval::I i)
{
    f(id) = (i.upper() + i.lower()) / 2;
    e(id, id) = (i.upper() - i.lower()) / 2;
}

////////////////////////////////////////////////////////////////////////////////

void AffineEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                   Clause::Id a, Clause::Id b)
{
#define ov f(id)
#define oe e.row(id)

#define av f(a)
#define ae e.row(a)
#define ai i(a)

#define bv f(b)
#define be e.row(b)
#define bi i(b)

    switch (op) {
        case Opcode::ADD:
            ov = av + bv;
            oe = ae + be;
            break;
        case Opcode::MUL:
            ov = av * bv;
            oe = av * be + bv * ae;
            {   // Evaluate the quadratic term as an interval
                const float aes = ae.cwiseAbs().sum();
                const float bes = be.cwiseAbs().sum();
                setAffine(id, Interval::I(-aes, aes) * Interval::I(-bes, bes));
            }
            break;
        case Opcode::MIN:
            if (ai.upper() < bi.lower())
            {
                ov = av;
                oe = ae;
            }
            else if (ai.lower() > bi.upper())
            {
                ov = bv;
                oe = be;
            }
            else
            {
                setAffine(id, boost::numeric::min(ai, bi));
            }
            break;
        case Opcode::MAX:
            if (ai.upper() < bi.lower())
            {
                ov = bv;
                oe = be;
            }
            else if (ai.lower() > bi.upper())
            {
                ov = av;
                oe = ae;
            }
            else
            {
                setAffine(id, boost::numeric::max(ai, bi));
            }
            break;
        case Opcode::SUB:
            ov = av - bv;
            oe = ae - be;
            break;
        case Opcode::DIV:
            setAffine(id, ai / bi);
            break;
        case Opcode::ATAN2:
            setAffine(id, atan2(ai, bi));
            break;
        case Opcode::POW:
            setAffine(id, boost::numeric::pow(ai, bv));
            break;
        case Opcode::NTH_ROOT:
            setAffine(id, boost::numeric::nth_root(ai, bv));
            break;
        case Opcode::MOD:
            setAffine(id, Interval::I(0.0f, bi.upper())); // YOLO
            break;
        case Opcode::NANFILL:
            if (std::isnan(ai.lower()) || std::isnan(ai.upper()))
            {
                ov = bv;
                oe = ae;
            }
            else
            {
                ov = av;
                oe = ae;
            }
            break;

        case Opcode::SQUARE:
            ov = av * av;
            oe = 2 * av * ae;
            {   // Evaluate the quadratic term as an interval
                const float aes = ae.cwiseAbs().sum();
                setAffine(id, boost::numeric::square(Interval::I(-aes, aes)));
            }
            break;
        case Opcode::SQRT:
            setAffine(id, boost::numeric::sqrt(ai));
            break;
        case Opcode::NEG:
            ov = -av;
            oe = -ae;
            break;
        case Opcode::SIN:
            setAffine(id, boost::numeric::sin(ai));
            break;
        case Opcode::COS:
            setAffine(id, boost::numeric::cos(ai));
            break;
        case Opcode::TAN:
            setAffine(id, boost::numeric::tan(ai));
            break;
        case Opcode::ASIN:
            setAffine(id, boost::numeric::asin(ai));
            break;
        case Opcode::ACOS:
            setAffine(id, boost::numeric::acos(ai));
            break;
        case Opcode::ATAN:
            // If the interval has an infinite bound, then return the largest
            // possible output interval (of +/- pi/2).  This rescues us from
            // situations where we do atan(y / x) and the behavior of the
            // interval changes if you're approaching x = 0 from below versus
            // from above.
            setAffine(id, (std::isinf(ai.lower()) || std::isinf(ai.upper()))
                ? Interval::I(-M_PI/2, M_PI/2)
                : boost::numeric::atan(ai));
            break;
        case Opcode::EXP:
            setAffine(id, boost::numeric::exp(ai));
            break;
        case Opcode::ABS:
            if (ai.upper() < 0)
            {
                ov = -av;
                oe = -ae;
            }
            else if (ai.lower() > 0)
            {
                ov = av;
                oe = ae;
            }
            else
            {
                setAffine(id, boost::numeric::abs(ai));
            }
            break;
        case Opcode::RECIP:
            setAffine(id, Interval::I(1,1) / ai);
            break;

        case Opcode::CONST_VAR:
            ov = av;
            oe = ae;
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

