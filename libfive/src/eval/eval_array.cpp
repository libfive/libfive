/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/eval_array.hpp"
#include "libfive/eval/tape.hpp"
#include "libfive/eval/deck.hpp"

namespace Kernel {

constexpr size_t ArrayEvaluator::N;

ArrayEvaluator::ArrayEvaluator(const Tree& root)
    : ArrayEvaluator(std::make_shared<Deck>(root))
{
    // Nothing to do here
}

ArrayEvaluator::ArrayEvaluator(
        const Tree& root, const std::map<Tree::Id, float>& vars)
    : ArrayEvaluator(std::make_shared<Deck>(root), vars)
{
    // Nothing to do here
}

ArrayEvaluator::ArrayEvaluator(std::shared_ptr<Deck> d)
    : ArrayEvaluator(d, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

ArrayEvaluator::ArrayEvaluator(
        std::shared_ptr<Deck> d, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(d, vars), v(deck->num_clauses + 1, N), ambig(false)
{
    // Initialize the whole data array as zero, to prevent Valgrind warnings.
    v.array() = 0;

    // Unpack variables into result array
    for (auto& var_ : deck->vars.right)
    {
        auto var = vars.find(var_.first);
        v.row(var_.second) = (var != vars.end()) ? var->second : 0;
    }

    // Unpack constants into result array
    for (auto& c : deck->constants)
    {
        v.row(c.first) = c.second;
    }
}

float ArrayEvaluator::value(const Eigen::Vector3f& pt) {
    return value(pt, deck->tape);
}

float ArrayEvaluator::value(const Eigen::Vector3f& pt,
                            std::shared_ptr<Tape> tape)
{
    set(pt, 0);
    return values(1, tape)(0);
}


Eigen::Block<decltype(ArrayEvaluator::v), 1, Eigen::Dynamic>
ArrayEvaluator::values(size_t count)
{
    return values(count, deck->tape);
}

void ArrayEvaluator::setCount(size_t count)
{
    count_actual = count;

#if defined EIGEN_VECTORIZE_AVX512
    #define LIBFIVE_SIMD_SIZE 16
#elif defined EIGEN_VECTORIZE_AVX
    #define LIBFIVE_SIMD_SIZE 8
#elif defined EIGEN_VECTORIZE_SSE
    #define LIBFIVE_SIMD_SIZE 4
#elif defined EIGEN_VECTORIZE
    #warning "EIGEN_VECTORIZE is set but no vectorization flag is found"
    #define LIBFIVE_SIMD_SIZE 0
#else
    #warning "No SIMD flags detected"
    #define LIBFIVE_SIMD_SIZE 0
#endif
    // If we have SIMD instructions, then round the evaluation size up
    // to the nearest block, to avoid issues where Eigen's SIMD and
    // non-SIMD paths produce different results.
    if (LIBFIVE_SIMD_SIZE) {
        count_simd = ((count + LIBFIVE_SIMD_SIZE - 1) / LIBFIVE_SIMD_SIZE)
                * LIBFIVE_SIMD_SIZE;
    } else {
        count_simd = count;
    }
}

Eigen::Block<decltype(ArrayEvaluator::v), 1, Eigen::Dynamic>
ArrayEvaluator::values(size_t count, Tape::Handle tape)
{
    setCount(count);

    deck->bindOracles(tape);
    auto index = tape->rwalk(*this);
    deck->unbindOracles();

    return v.block<1, Eigen::Dynamic>(index, 0, 1, count);
}


std::pair<float, Tape::Handle> ArrayEvaluator::valueAndPush(
        const Eigen::Vector3f& pt, Tape::Handle tape)
{
    if (tape == nullptr) {
        tape = deck->tape;
    }
    auto out = value(pt, tape);
    auto p = Tape::push(tape, *deck,
        [&](Opcode::Opcode op, Clause::Id /* id */,
            Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::OP_MAX)
        {
            if (v(a, 0) > v(b, 0))
            {
                return Tape::KEEP_A;
            }
            else if (v(b, 0) > v(a, 0))
            {
                return Tape::KEEP_B;
            }
            else
            {
                return Tape::KEEP_BOTH;
            }
        }
        else if (op == Opcode::OP_MIN)
        {
            if (v(a, 0) > v(b, 0))
            {
                return Tape::KEEP_B;
            }
            else if (v(b, 0) > v(a, 0))
            {
                return Tape::KEEP_A;
            }
            else
            {
                return Tape::KEEP_BOTH;
            }
        }
        return Tape::KEEP_ALWAYS;
    }, Tape::SPECIALIZED);
    return std::make_pair(out, std::move(p));
}

////////////////////////////////////////////////////////////////////////////////

bool ArrayEvaluator::setVar(Tree::Id var_, float value)
{
    auto var = deck->vars.right.find(var_);
    if (var != deck->vars.right.end())
    {
        bool changed = v(var->second, 0) != value;
        v.row(var->second) = value;
        return changed;
    }
    else
    {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////

Eigen::Block<decltype(ArrayEvaluator::ambig), 1, Eigen::Dynamic>
ArrayEvaluator::getAmbiguous(size_t i)
{
    return getAmbiguous(i, deck->tape);
}

Eigen::Block<decltype(ArrayEvaluator::ambig), 1, Eigen::Dynamic>
ArrayEvaluator::getAmbiguous(size_t i, Tape::Handle tape)
{
    // Reset the ambiguous array to all false
    ambig = false;

    bool abort = false;
    tape->walk(
        [&](Opcode::Opcode op, Clause::Id /* id */, Clause::Id a, Clause::Id b)
        {
            if (op == Opcode::ORACLE)
            {
                deck->oracles[a]->checkAmbiguous(ambig.head(i));
            }
            else if (op == Opcode::OP_MIN || op == Opcode::OP_MAX)
            {
                ambig.head(i) = ambig.head(i) ||
                    (v.block(a, 0, 1, i) ==
                     v.block(b, 0, 1, i));
            }
        }, abort);

    return ambig.head(i);
}

////////////////////////////////////////////////////////////////////////////////

void ArrayEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                Clause::Id a_, Clause::Id b_)
{
#define out v.block<1, Eigen::Dynamic>(id, 0, 1, count_simd)
#define a v.row(a_).head(count_simd)
#define b v.row(b_).head(count_simd)
    switch (op)
    {
        case Opcode::OP_ADD:
            out = a + b;
            break;
        case Opcode::OP_MUL:
            out = a * b;
            break;
        case Opcode::OP_MIN:
            out = a.cwiseMin(b);
            break;
        case Opcode::OP_MAX:
            out = a.cwiseMax(b);
            break;
        case Opcode::OP_SUB:
            out = a - b;
            break;
        case Opcode::OP_DIV:
            out = a / b;
            break;
        case Opcode::OP_ATAN2:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = atan2(a(i), b(i));
            }
            break;
        case Opcode::OP_POW:
            out = a.pow(b);
            break;
        case Opcode::OP_NTH_ROOT:
            for (auto i=0; i < a.size(); ++i)
            {
                // Work around a limitation in pow by using boost's nth-root
                // function on a single-point interval
                if (a(i) < 0)
                    out(i) = boost::numeric::nth_root(
                            Interval::I(a(i), a(i)), b(i)).lower();
                else
                    out(i) = pow(a(i), 1.0f/b(i));
            }
            break;
        case Opcode::OP_MOD:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = std::fmod(a(i), b(i));
                while (out(i) < 0)
                {
                    out(i) += b(i);
                }
            }
            break;
        case Opcode::OP_NANFILL:
            out = a.isNaN().select(b, a);
            break;
        case Opcode::OP_COMPARE:
            for (auto i=0; i < a.size(); ++i)
            {
                if      (a(i) < b(i))   out(i) = -1;
                else if (a(i) > b(i))   out(i) =  1;
                else                    out(i) =  0;
            }
            break;

        case Opcode::OP_SQUARE:
            out = a * a;
            break;
        case Opcode::OP_SQRT:
            out = sqrt(a);
            break;
        case Opcode::OP_NEG:
            out = -a;
            break;
        case Opcode::OP_SIN:
            out = sin(a);
            break;
        case Opcode::OP_COS:
            out = cos(a);
            break;
        case Opcode::OP_TAN:
            out = tan(a);
            break;
        case Opcode::OP_ASIN:
            out = asin(a);
            break;
        case Opcode::OP_ACOS:
            out = acos(a);
            break;
        case Opcode::OP_ATAN:
            out = atan(a);
            break;
        case Opcode::OP_LOG:
            out = log(a);
            break;
        case Opcode::OP_EXP:
            out = exp(a);
            break;
        case Opcode::OP_ABS:
            out = abs(a);
            break;
        case Opcode::OP_RECIP:
            out = 1 / a;
            break;

        case Opcode::CONST_VAR:
            out = a;
            break;

        case Opcode::ORACLE:
            deck->oracles[a_]->evalArray(
                    v.block<1, Eigen::Dynamic>(id, 0, 1, count_actual));
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
}

}   // namespace Kernel

