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
#include "libfive/eval/eval_deriv_array.hpp"

namespace Kernel {

DerivArrayEvaluator::DerivArrayEvaluator(std::shared_ptr<Tape> t)
    : DerivArrayEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

DerivArrayEvaluator::DerivArrayEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : ArrayEvaluator(t, vars), d(tape->num_clauses + 1, 1)
{
    // Initialize all derivatives to zero
    for (Eigen::Index i=0; i < d.rows(); ++i)
    {
        d(i) = 0;
    }

    // Load immutable derivatives for X, Y, Z
    d(tape->X).row(0) = 1;
    d(tape->Y).row(1) = 1;
    d(tape->Z).row(2) = 1;
}

Eigen::Block<decltype(DerivArrayEvaluator::ambig), 1, Eigen::Dynamic>
DerivArrayEvaluator::getAmbiguousDerivs(size_t i)
{
    // Reset the ambiguous array to all false
    ambig = false;

    bool abort = false;
    tape->walk(
        [&](Opcode::Opcode op, Clause::Id /* id */, Clause::Id a, Clause::Id b)
        {
            if (op == Opcode::ORACLE)
            {
                tape->oracles[a]->checkAmbiguous(ambig.head(i));
            }
            else if (op == Opcode::OP_MIN || op == Opcode::OP_MAX)
            {
                ambig.head(i) = ambig.head(i) ||
                    ((f.block(a, 0, 1, i) ==
                      f.block(b, 0, 1, i)) &&
                      (d(a).leftCols(i) != d(b).leftCols(i)).colwise().sum());
            }
        }, abort);

    return ambig.head(i);

}

Eigen::Block<decltype(DerivArrayEvaluator::out), 4, Eigen::Dynamic>
DerivArrayEvaluator::derivs(size_t count)
{
    // Perform value evaluation, copying results into the 4th row of out
    out.row(3).head(count) = values(count);

    // Perform derivative evaluation, copying results into the out array
    out.topLeftCorner(3, count) = d(tape->rwalk(*this)).leftCols(count);

    // Return a block of valid results from the out array
    return out.block<4, Eigen::Dynamic>(0, 0, 4, count);
}

void DerivArrayEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                     Clause::Id a_, Clause::Id b_)
{
#define ov f.row(id).head(count)
#define od d(id).leftCols(count)

#define av f.row(a_).head(count)
#define ad d(a_).leftCols(count)

#define bv f.row(b_).head(count)
#define bd d(b_).leftCols(count)

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
                    Eigen::Array<float, 1, Eigen::Dynamic>::Zero(1, count),
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
            od = ad;
            break;

        case Opcode::ORACLE:
            tape->oracles[a_]->evalDerivArray(od);
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

}   // namespace Kernel
