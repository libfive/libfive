/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/eval_interval.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/render/brep/region.hpp"

namespace libfive {

IntervalEvaluator::IntervalEvaluator(const Tree& root)
    : IntervalEvaluator(std::make_shared<Deck>(root))
{
    // Nothing to do here
}

IntervalEvaluator::IntervalEvaluator(
        const Tree& root, const std::map<Tree::Id, float>& vars)
    : IntervalEvaluator(std::make_shared<Deck>(root), vars)
{
    // Nothing to do here
}

IntervalEvaluator::IntervalEvaluator(std::shared_ptr<Deck> d)
    : IntervalEvaluator(d, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

IntervalEvaluator::IntervalEvaluator(
        std::shared_ptr<Deck> d, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(d, vars)
{
    i.resize(d->num_clauses + 1);

    // Unpack variables into result array
    for (auto& v : d->vars.right)
    {
        auto var = vars.find(v.first);
        store((var != vars.end()) ? var->second : 0, v.second);
    }

    // Unpack constants into result array
    for (auto& c : d->constants)
    {
        store(c.second, c.first);
    }
}


void IntervalEvaluator::store(float f, size_t index)
{
    const bool is_nan = std::isnan(f);
    i[index] = is_nan ? Interval::I::empty() : f;
}

Interval::I IntervalEvaluator::eval(const Eigen::Vector3f& lower,
                                    const Eigen::Vector3f& upper)
{
    return eval(lower, upper, deck->tape);
}

Interval::I IntervalEvaluator::eval(
        const Eigen::Vector3f& lower,
        const Eigen::Vector3f& upper,
        const Tape::Handle& tape)
{
    assert(!lower.array().isNaN().any()); // A region's bounds should
    assert(!upper.array().isNaN().any()); // never be NaN.

    i[deck->X] = {lower.x(), upper.x()};
    i[deck->Y] = {lower.y(), upper.y()};
    i[deck->Z] = {lower.z(), upper.z()};

    for (auto& o : deck->oracles)
    {
        o->set(lower, upper);
    }

    deck->bindOracles(*tape);
    for (auto itr = tape->rbegin(); itr != tape->rend(); ++itr) {
        (*this)(itr->op, itr->id, itr->a, itr->b);
    }
    deck->unbindOracles();

    auto root = tape->root();
    return i[root];
}

std::pair<Interval::I, Tape::Handle> IntervalEvaluator::intervalAndPush(
        const Eigen::Vector3f& lower,
        const Eigen::Vector3f& upper)
{
    return intervalAndPush(lower, upper, deck->tape);
}

std::pair<Interval::I, Tape::Handle> IntervalEvaluator::intervalAndPush(
        const Eigen::Vector3f& lower,
        const Eigen::Vector3f& upper,
        const Tape::Handle& tape)
{
    auto out = eval(lower, upper, tape);
    return std::make_pair(out, push(tape));
}

Tape::Handle IntervalEvaluator::push()
{
    return push(deck->tape);
}

Tape::Handle IntervalEvaluator::push(const Tape::Handle& tape)
{
    assert(tape.get() != nullptr);

    const Region<3> R(Eigen::Vector3d(i[deck->X].lower(),
                                      i[deck->Y].lower(),
                                      i[deck->Z].lower()),
                      Eigen::Vector3d(i[deck->X].upper(),
                                      i[deck->Y].upper(),
                                      i[deck->Z].upper()));
    return tape->push(*deck,
        [&](Opcode::Opcode op, Clause::Id /* id */,
            Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::OP_MAX)
        {
            if (a == b)
            {
                return Tape::KEEP_A;
            }
            else if (i[a].lower() > i[b].upper())
            {
                return Tape::KEEP_A;
            }
            else if (i[b].lower() > i[a].upper())
            {
                return Tape::KEEP_B;
            }
            return Tape::KEEP_BOTH;
        }
        else if (op == Opcode::OP_MIN)
        {
            if (a == b)
            {
                return Tape::KEEP_A;
            }
            else if (i[a].lower() > i[b].upper())
            {
                return Tape::KEEP_B;
            }
            else if (i[b].lower() > i[a].upper())
            {
                return Tape::KEEP_A;
            }
            return Tape::KEEP_BOTH;
        }
        return Tape::KEEP_ALWAYS;
    },
    Tape::INTERVAL, R);
}

////////////////////////////////////////////////////////////////////////////////

bool IntervalEvaluator::setVar(Tree::Id var, float value)
{
    auto v = deck->vars.right.find(var);
    if (v != deck->vars.right.end())
    {
        const bool changed = (i[v->second] != value);
        store(value, v->second);
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

void IntervalEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                   Clause::Id a_, Clause::Id b_)
{
#define out i[id]
#define a i[a_]
#define b i[b_]
#define SET_UNSAFE(cond) if ((cond)) { out = Interval::I::empty(); }

    if (!Interval::isSafe(a) || !Interval::isSafe(b)) {
        out = Interval::I::empty();
        return;
    }

    switch (op) {
        case Opcode::OP_ADD:
            out = a + b;
            SET_UNSAFE((a.lower() == -INFINITY && b.upper() == INFINITY) ||
                       (b.lower() == -INFINITY && a.upper() == INFINITY));
            break;
        case Opcode::OP_MUL:
            out = a * b;
            SET_UNSAFE(
                ((a.lower() == -INFINITY || a.upper() == INFINITY)
                    && b.lower() <= 0.f && b.upper() >= 0.f) ||
                ((b.lower() == -INFINITY || b.upper() == INFINITY)
                    && a.lower() <= 0.f && a.upper() >= 0.f));
            break;
        case Opcode::OP_MIN:
            out = boost::numeric::min(a, b);
            break;
        case Opcode::OP_MAX:
            out = boost::numeric::max(a, b);
            break;
        case Opcode::OP_SUB:
            out = a - b;
            SET_UNSAFE((a.lower() == -INFINITY && b.lower() == -INFINITY) ||
                       (a.upper() == -INFINITY && b.upper() == -INFINITY));
            break;
        case Opcode::OP_DIV:
            out = a / b;
            if (b.lower() == 0.f && b.upper() == 0.f)
            {
                // In this case, out gives us the empty interval, but point
                // evaluation might give us an infinite value.
                if ((a.lower() <= 0.f && a.upper() >= 0.f) ||
                    std::signbit(b.lower()) != std::signbit(b.upper()))
                {
                    out = { -INFINITY, INFINITY };
                }
                else if (std::signbit(b.lower()) == (a.upper() < 0.f))
                {
                    out = { INFINITY, INFINITY };
                }
                else
                {
                    out = { -INFINITY, -INFINITY };
                }
            }
            SET_UNSAFE(((a.lower() == -INFINITY || a.upper() == INFINITY) &&
                        (b.lower() == -INFINITY || b.upper() == INFINITY)) ||
                        (a.lower() <= 0.f && a.upper() >= 0.f &&
                         b.lower() <= 0.f && b.upper() >= 0.f));
            break;
        case Opcode::OP_ATAN2:
            out = atan2(a, b);
            SET_UNSAFE(a.lower() <= 0.f && a.upper() >= 0.f &&
                       b.lower() <= 0.f && b.upper() >= 0.f);
            break;
        case Opcode::OP_POW:
        {
            auto bPt = b.lower();
            out = boost::numeric::pow(a, bPt);
            // The behavior of raising zero to a negative power is
            // implementation-defined; it may raise a domain error (and thus
            // return NaN) or a pole error (and thus return an infinite value);
            // We can use a static const variable to test for this.

            // Other cases that produce NaN are 0^0, or a negative number to a
            // noninteger; we can use std::pow explicitly to test whether b is
            // considered an integer as far as std::pow is concerned.
            static const auto nanOnZeroToNegative =
                std::isnan(std::pow(0.f, -1.f));

            // Store whether a contains 0, to simplify the expression below.
            const bool a_zero = a.lower() <= 0.f && a.upper() >= 0.f;

            SET_UNSAFE(
               (a_zero && (bPt == 0.f || (bPt < 0.f && nanOnZeroToNegative))) ||
               (a.lower() < 0 && std::isnan(std::pow(-1.f, bPt))));
            break;
        }
        case Opcode::OP_NTH_ROOT:
        {
            int bPt = b.lower();
            out = boost::numeric::nth_root(a, bPt);
            // We can only take multiples-of-two nth roots on negative values
            SET_UNSAFE(a.lower() <= 0.f && !(bPt & 2));
            break;
        }
        case Opcode::OP_MOD:
        {
            out = { fmin(b.lower(), 0.0f), fmax(0.0f, b.upper()) };
#if 0 /* This logic is failing with new mod behavior + fuzzing */
            if (std::isfinite(a.upper()) && std::isfinite(a.lower()))
            {
                // We may be able to do better: Divide into cases, based on whether
                // b is above, below, or crossing 0.
                auto position = (b.upper() >= 0.f) + 2 * (b.lower() <= 0.f);
                switch (position)
                {
                case 1:
                case 2:
                {
                    auto maxMagB = position == 1 ? b.upper() : -b.lower();
                    auto minMagB = position == 1 ? b.lower() : -b.upper();
                    auto highestQuotientB = a.upper() > 0 ? minMagB : maxMagB;
                    auto lowestQuotientB = a.lower() < 0 ? maxMagB : minMagB;
                    // Use std::floor to round to -INFINITY rather than to 0, 
                    // in order to match the point evaluator behavior.
                    auto highestQuotientInt = static_cast<int>
                        (std::floor(a.upper() / highestQuotientB));
                    auto lowestQuotientInt = static_cast<int>
                        (std::floor(a.lower() / lowestQuotientB));
                    if (highestQuotientInt == lowestQuotientInt)
                    {
                      out = { a.lower() - lowestQuotientInt * lowestQuotientB,
                          a.upper() - highestQuotientInt * highestQuotientB };
                    }
                }
                case 3:
                    break;
                case 0: //Can only happen if b is guaranteed NaN.
                    out = Interval::I::empty();
                    break;
                default:
                    assert(false);
                }
            }
#endif
            SET_UNSAFE(b.upper() >= 0.f && b.lower() <= 0.f);
            break;
        }
        case Opcode::OP_NANFILL:
            SET_UNSAFE(true);
            break;
        case Opcode::OP_COMPARE:
            if      (a.upper() < b.lower()) out = Interval::I(-1, -1);
            else if (a.lower() > b.upper()) out = Interval::I( 1,  1);
            else                            out = Interval::I(-1,  1);
            break;

        case Opcode::OP_SQUARE:
            out = boost::numeric::square(a);
            break;
        case Opcode::OP_SQRT:
            out = boost::numeric::sqrt(a);
            SET_UNSAFE(a.lower() < 0.0f);
            break;
        case Opcode::OP_NEG:
            out = -a;
            break;
        case Opcode::OP_SIN:
            out = boost::numeric::sin(a);
            break;
        case Opcode::OP_COS:
            out = boost::numeric::cos(a);
            break;
        case Opcode::OP_TAN:
            out = boost::numeric::tan(a);
            break;
        case Opcode::OP_ASIN:
            out = boost::numeric::asin(a);
            SET_UNSAFE(a.lower() < -1.0f || a.upper() > 1.0f);
            break;
        case Opcode::OP_ACOS:
            out = boost::numeric::acos(a);
            SET_UNSAFE(a.lower() < -1.0f || a.upper() > 1.0f);
            break;
        case Opcode::OP_ATAN:
            // If the interval has an infinite bound, then return the largest
            // possible output interval (of +/- pi/2).  This rescues us from
            // situations where we do atan(y / x) and the behavior of the
            // interval changes if you're approaching x = 0 from below versus
            // from above.
            out = (std::isinf(a.lower()) || std::isinf(a.upper()))
                ? Interval::I(-M_PI/2, M_PI/2)
                : boost::numeric::atan(a);
            break;
        case Opcode::OP_EXP:
            out = boost::numeric::exp(a);
            break;
        case Opcode::OP_LOG:
            out = boost::numeric::log(a);
            SET_UNSAFE(a.lower() < 0.0f); // But exactly 0 is ok (returns -inf)
            break;
        case Opcode::OP_ABS:
            out = boost::numeric::abs(a);
            break;
        case Opcode::OP_RECIP:
            out = Interval::I(1,1) / a;
            // The numerator can't be 0, so a denominator of 0 won't produce NaN
            break;

        case Opcode::CONST_VAR:
            out = a;
            break;

        case Opcode::ORACLE:
            deck->oracles[a_]->evalInterval(out);
            break;

        case Opcode::INVALID:
        case Opcode::CONSTANT:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR_FREE:
        case Opcode::LAST_OP: assert(false);
    }

#undef out
#undef a
#undef b
#undef SET_UNSAFE
}

}   // namespace libfive
