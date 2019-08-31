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
    i[index] = Interval(f, f);
}

Interval IntervalEvaluator::eval(const Eigen::Vector3f& lower,
                                 const Eigen::Vector3f& upper)
{
    return eval(lower, upper, deck->tape);
}

Interval IntervalEvaluator::eval(
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
        evalClause(*itr, tape->getNaryData());
    }
    deck->unbindOracles();

    auto root = tape->root();
    return i[root];
}

std::pair<Interval, Tape::Handle> IntervalEvaluator::intervalAndPush(
        const Eigen::Vector3f& lower,
        const Eigen::Vector3f& upper)
{
    return intervalAndPush(lower, upper, deck->tape);
}

std::pair<Interval, Tape::Handle> IntervalEvaluator::intervalAndPush(
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
        [&](Clause c, const uint32_t* n_ary, uint8_t* n_ary_keep)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (c.op == Opcode::OP_MAX)
        {
            if (c.a == c.b)
            {
                return Tape::KEEP_A;
            }
            else if (i[c.a].lower() > i[c.b].upper())
            {
                return Tape::KEEP_A;
            }
            else if (i[c.b].lower() > i[c.a].upper())
            {
                return Tape::KEEP_B;
            }
            return Tape::KEEP_BOTH;
        }
        else if (c.op == Opcode::OP_MIN)
        {
            if (c.a == c.b)
            {
                return Tape::KEEP_A;
            }
            else if (i[c.a].lower() > i[c.b].upper())
            {
                return Tape::KEEP_B;
            }
            else if (i[c.b].lower() > i[c.a].upper())
            {
                return Tape::KEEP_A;
            }
            return Tape::KEEP_BOTH;
        }
        else if (c.op == Opcode::OP_NARY_MIN)
        {
            for (unsigned j=c.a; j != c.b; ++j) {
                n_ary_keep[j] = (i[n_ary[j]].lower() <= i[c.id].upper());
            }
            return Tape::KEEP_BOTH;
        }
        else if (c.op == Opcode::OP_NARY_MAX)
        {
            for (unsigned j=c.a; j != c.b; ++j) {
                n_ary_keep[j] = (i[n_ary[j]].upper() >= i[c.id].lower());
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
        const bool changed = (i[v->second].lower() != value) ||
                             (i[v->second].upper() != value);
        store(value, v->second);
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

void IntervalEvaluator::evalClause(const Clause& c, const uint32_t* n_ary)
{
#define out i[c.id]
#define a i[c.a]
#define b i[c.b]

    switch (c.op) {
        case Opcode::OP_ADD:
            out = a + b;
            break;
        case Opcode::OP_MUL:
            out = a * b;
            break;
        case Opcode::OP_MIN:
            out = Interval::min(a, b);
            break;
        case Opcode::OP_MAX:
            out = Interval::max(a, b);
            break;
        case Opcode::OP_SUB:
            out = a - b;
            break;
        case Opcode::OP_DIV:
            out = a / b;
            break;
        case Opcode::OP_ATAN2:
            out = Interval::atan2(a, b);
            break;
        case Opcode::OP_POW:
            out = Interval::pow(a, b);
            break;
        case Opcode::OP_NTH_ROOT:
            out = Interval::nth_root(a, b);
            break;
        case Opcode::OP_MOD:
            out = Interval::mod(a, b);
            return;
        case Opcode::OP_NANFILL:
            out = Interval::nanfill(a, b);
            break;
        case Opcode::OP_COMPARE:
            out = Interval::compare(a, b);
            break;

        case Opcode::OP_SQUARE:
            out = Interval::square(a);
            break;
        case Opcode::OP_SQRT:
            out = Interval::sqrt(a);
            break;
        case Opcode::OP_NEG:
            out = -a;
            break;
        case Opcode::OP_SIN:
            out = Interval::sin(a);
            break;
        case Opcode::OP_COS:
            out = Interval::cos(a);
            break;
        case Opcode::OP_TAN:
            out = Interval::tan(a);
            break;
        case Opcode::OP_ASIN:
            out = Interval::asin(a);
            break;
        case Opcode::OP_ACOS:
            out = Interval::acos(a);
            break;
        case Opcode::OP_ATAN:
            out = Interval::atan(a);
            break;
        case Opcode::OP_EXP:
            out = Interval::exp(a);
            break;
        case Opcode::OP_LOG:
            out = Interval::log(a);
            break;
        case Opcode::OP_ABS:
            out = Interval::abs(a);
            break;
        case Opcode::OP_RECIP:
            out = Interval::recip(a);
            break;

        case Opcode::CONST_VAR:
            out = a;
            break;

#undef out
#undef a
#undef b
        case Opcode::ORACLE:
            deck->oracles[c.a]->evalInterval(i[c.id]);
            break;

        case Opcode::OP_NARY_MIN:
            i[c.id] = i[n_ary[c.a]];
            for (unsigned j=c.a + 1; j < c.b; ++j) {
                i[c.id] = Interval::min(i[c.id], i[n_ary[j]]);
            }
            break;

        case Opcode::OP_NARY_MAX:
            i[c.id] = i[n_ary[c.a]];
            for (unsigned j=c.a + 1; j < c.b; ++j) {
                i[c.id] = Interval::max(i[c.id], i[n_ary[j]]);
            }
            break;

        case Opcode::OP_NARY_ADD:
            i[c.id] = i[n_ary[c.a]];
            for (unsigned j=c.a + 1; j < c.b; ++j) {
                i[c.id] = i[c.id] + i[n_ary[j]];
            }
            break;

        case Opcode::OP_NARY_MUL:
            i[c.id] = i[n_ary[c.a]];
            for (unsigned j=c.a + 1; j < c.b; ++j) {
                i[c.id] = i[c.id] * i[n_ary[j]];
            }
            break;

        case Opcode::INVALID:
        case Opcode::CONSTANT:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR_FREE:
        case Opcode::LAST_OP: assert(false);
    }
}

}   // namespace libfive
