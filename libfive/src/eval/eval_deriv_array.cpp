/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/eval_deriv_array.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"

namespace libfive {

DerivArrayEvaluator::DerivArrayEvaluator(const Tree& root)
    : DerivArrayEvaluator(std::make_shared<Deck>(root))
{
    // Nothing to do here
}

DerivArrayEvaluator::DerivArrayEvaluator(
        const Tree& root, const std::map<Tree::Id, float>& vars)
    : DerivArrayEvaluator(std::make_shared<Deck>(root), vars)
{
    // Nothing to do here
}

DerivArrayEvaluator::DerivArrayEvaluator(std::shared_ptr<Deck> d)
    : DerivArrayEvaluator(d, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

DerivArrayEvaluator::DerivArrayEvaluator(
        std::shared_ptr<Deck> deck, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(deck, vars), ArrayEvaluator(deck, vars),
      d(deck->num_clauses + 1, 1)
{
    // Initialize all derivatives to zero
    for (Eigen::Index i=0; i < d.rows(); ++i)
    {
        d(i) = 0;
    }

    // Load immutable derivatives for X, Y, Z
    d(deck->X).row(0) = 1;
    d(deck->Y).row(1) = 1;
    d(deck->Z).row(2) = 1;
}

Eigen::Block<decltype(DerivArrayEvaluator::ambig), 1, Eigen::Dynamic>
DerivArrayEvaluator::getAmbiguousDerivs(size_t i)
{
    return getAmbiguousDerivs(i, *deck->tape);
}

Eigen::Block<decltype(DerivArrayEvaluator::ambig), 1, Eigen::Dynamic>
DerivArrayEvaluator::getAmbiguousDerivs(size_t i, const Tape& tape)
{
    // Reset the ambiguous array to all false
    ambig = false;

    for (auto itr = tape.rbegin(); itr != tape.rend(); ++itr) {
        if (itr->op == Opcode::ORACLE)
        {
            deck->oracles[itr->a]->checkAmbiguous(ambig.head(i));
        }
        else if (itr->op == Opcode::OP_MIN || itr->op == Opcode::OP_MAX)
        {
            ambig.head(i) = ambig.head(i) ||
                ((v.block(itr->a, 0, 1, i) ==
                  v.block(itr->b, 0, 1, i)) &&
                  (d(itr->a).leftCols(i) != d(itr->b).leftCols(i))
                    .colwise().sum());
        }
    }

    return ambig.head(i);

}

Eigen::Vector4f DerivArrayEvaluator::deriv(const Eigen::Vector3f& pt)
{
    return deriv(pt, *deck->tape);
}

Eigen::Vector4f DerivArrayEvaluator::deriv(const Eigen::Vector3f& pt,
                                           const Tape& tape)
{
    set(pt, 0);
    return derivs(1, tape).col(0);
}

Eigen::Block<decltype(DerivArrayEvaluator::out), 4, Eigen::Dynamic>
DerivArrayEvaluator::derivs(size_t count)
{
    return derivs(count, *deck->tape);
}

Eigen::Block<decltype(DerivArrayEvaluator::out), 4, Eigen::Dynamic>
DerivArrayEvaluator::derivs(size_t count, const Tape& tape)
{
    // Perform value evaluation, copying results into the 4th row of out
    out.row(3).head(count) = values(count, tape);

    // Perform derivative evaluation, copying results into the out array
    deck->bindOracles(tape);
    for (auto itr = tape.rbegin(); itr != tape.rend(); ++itr) {
        (*this)(itr->op, itr->id, itr->a, itr->b);
    }
    deck->unbindOracles();

    // Return a block of valid results from the out array
    out.topLeftCorner(3, count) = d(tape.root()).leftCols(count);
    return out.block<4, Eigen::Dynamic>(0, 0, 4, count);
}

void DerivArrayEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                     Clause::Id a_, Clause::Id b_)
{
#define ov v.row(id).head(count_simd)
#define od d(id).leftCols(count_simd)

#define av v.row(a_).head(count_simd)
#define ad d(a_).leftCols(count_simd)

#define bv v.row(b_).head(count_simd)
#define bd d(b_).leftCols(count_simd)

    switch (op) {
        case Opcode::OP_ADD:
            od = ad + bd;
            break;
        case Opcode::OP_MUL:
            // Product rule
            od = bd.rowwise()*av + ad.rowwise()*bv;
            break;
        case Opcode::OP_MIN:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i) = (av < bv).select(ad.row(i), bd.row(i));
            break;
        case Opcode::OP_MAX:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i) = (av < bv).select(bd.row(i), ad.row(i));
            break;
        case Opcode::OP_SUB:
            od = ad - bd;
            break;
        case Opcode::OP_DIV:
            od = (ad.rowwise()*bv - bd.rowwise()*av).rowwise() /
                 bv.pow(2);
            break;
        case Opcode::OP_ATAN2:
            od = (ad.rowwise()*bv - bd.rowwise()*av).rowwise() /
                 (av.pow(2) + bv.pow(2));
            break;
        case Opcode::OP_POW:
            // The full form of the derivative is
            // od = m * (bv * ad + av * log(av) * bd))
            // However, log(av) is often NaN and bd is always zero,
            // (since it must be CONST), so we skip that part.
            od = ad.rowwise() * (bv * av.pow(bv - 1));
            break;

        case Opcode::OP_NTH_ROOT:
            for (Eigen::Index i=0; i < od.cols(); ++i)
                od.col(i) = (ad.col(i) == 0)
                    .select(0, ad.col(i) * (pow(av(i), 1.0f / bv(i) - 1) / bv(i)));
            break;
        case Opcode::OP_MOD:
            od = ad;
            break;
        case Opcode::OP_NANFILL:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i) = av.isNaN().select(bd.row(i), ad.row(i));
            break;
        case Opcode::OP_COMPARE:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i).setZero();
            break;

        case Opcode::OP_SQUARE:
            od = ad.rowwise() * av * 2;
            break;
        case Opcode::OP_SQRT:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i) = (av < 0 || ad.row(i) == 0).select(
                    Eigen::Array<float, 1, Eigen::Dynamic>::Zero(1, count_simd),
                    ad.row(i) / (2 * ov));
            break;
        case Opcode::OP_NEG:
            od = -ad;
            break;
        case Opcode::OP_SIN:
            od = ad.rowwise() * cos(av);
            break;
        case Opcode::OP_COS:
            od = ad.rowwise() * -sin(av);
            break;
        case Opcode::OP_TAN:
            od = ad.rowwise() * pow(1/cos(av), 2);
            break;
        case Opcode::OP_ASIN:
            od = ad.rowwise() / sqrt(1 - pow(av, 2));
            break;
        case Opcode::OP_ACOS:
            od = ad.rowwise() / -sqrt(1 - pow(av, 2));
            break;
        case Opcode::OP_ATAN:
            od = ad.rowwise() / (pow(av, 2) + 1);
            break;
        case Opcode::OP_LOG:
            od = ad.rowwise() / av;
            break;
        case Opcode::OP_EXP:
            od = ad.rowwise() * exp(av);
            break;
        case Opcode::OP_ABS:
            for (Eigen::Index i=0; i < od.rows(); ++i)
                od.row(i) = (av > 0).select(ad.row(i), -ad.row(i));
            break;
        case Opcode::OP_RECIP:
            od = ad.rowwise() / -av.pow(2);
            break;

        case Opcode::CONST_VAR:
            if (clear_vars) {
                od = 0.0;
            } else {
                od = ad;
            }
            break;

        case Opcode::ORACLE:
            deck->oracles[a_]->evalDerivArray(d(id).leftCols(count_actual));
            break;

        case Opcode::INVALID:
        case Opcode::CONSTANT:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR_FREE:
        case Opcode::LAST_OP: assert(false);
    }
#undef ov
#undef od

#undef av
#undef ad

#undef bv
#undef bd

}

}   // namespace libfive
