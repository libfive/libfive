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
#include "libfive/eval/eval_deriv.hpp"

namespace Kernel {

DerivEvaluator::DerivEvaluator(std::shared_ptr<Tape> t)
    : DerivEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

DerivEvaluator::DerivEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : PointEvaluator(t, vars), d(3, tape->num_clauses + 1)
{
    // Initialize all derivatives to zero
    d = 0;

    // Load immutable derivatives for X, Y, Z
    d(0, tape->X) = 1;
    d(1, tape->Y) = 1;
    d(2, tape->Z) = 1;
}

Eigen::Vector4f DerivEvaluator::deriv(const Eigen::Vector3f& pt)
{
    // Perform value evaluation, saving results
    auto w = eval(pt);
    auto xyz = d.col(tape->rwalk(*this));

    Eigen::Vector4f out;
    out << xyz, w;
    return out;
}

void DerivEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                Clause::Id a_, Clause::Id b_)
{
#define ov f(id)
#define od d.col(id)

#define av f(a_)
#define ad d.col(a_)

#define bv f(b_)
#define bd d.col(b_)

    switch (op) {
        case Opcode::OP_ADD:
            od = ad + bd;
            break;
        case Opcode::OP_MUL:
            // Product rule
            od = bd*av + ad*bv;
            break;
        case Opcode::OP_MIN:
            od = (av < bv) ? ad : bd;
            break;
        case Opcode::OP_MAX:
            od = (av < bv) ? bd : ad;
            break;
        case Opcode::OP_SUB:
            od = ad - bd;
            break;
        case Opcode::OP_DIV:
            od = (ad*bv - bd*av) / pow(bv, 2);
            break;
        case Opcode::OP_ATAN2:
            od = (ad*bv - bd*av) / (pow(av, 2) + pow(bv, 2));
            break;
        case Opcode::OP_POW:
            // The full form of the derivative is
            // od = m * (bv * ad + av * log(av) * bd))
            // However, log(av) is often NaN and bd is always zero,
            // (since it must be CONST), so we skip that part.
            od = ad * bv * pow(av, bv - 1);
            break;

        case Opcode::OP_NTH_ROOT:
            od = (ad == 0).select(0, ad * pow(av, 1.0f / bv - 1) / bv);
            break;
        case Opcode::OP_MOD:
            od = ad;
            break;
        case Opcode::OP_NANFILL:
            od = std::isnan(av) ? bd : ad;
            break;
        case Opcode::OP_COMPARE:
            od.setZero();
            break;

        case Opcode::OP_SQUARE:
            od = ad * av * 2;
            break;
        case Opcode::OP_SQRT:
            // This is not technically correct (the derivative goes to
            // infinity as ad goes to 0), but saves us from NaNs.
            od = av < 0 ? Eigen::Vector3f::Zero().eval()
                        : (ad == 0).select(Eigen::Vector3f::Zero(),
                                           (ad / (2 * ov)));
            break;
        case Opcode::OP_NEG:
            od = -ad;
            break;
        case Opcode::OP_SIN:
            od = ad * cos(av);
            break;
        case Opcode::OP_COS:
            od = ad * -sin(av);
            break;
        case Opcode::OP_TAN:
            od = ad * pow(1/cos(av), 2);
            break;
        case Opcode::OP_ASIN:
            od = ad / sqrt(1 - pow(av, 2));
            break;
        case Opcode::OP_ACOS:
            od = ad / -sqrt(1 - pow(av, 2));
            break;
        case Opcode::OP_ATAN:
            od = ad / (pow(av, 2) + 1);
            break;
        case Opcode::OP_LOG:
            od = ad / av;
            break;
        case Opcode::OP_EXP:
            od = ad * exp(av);
            break;
        case Opcode::OP_ABS:
            od = av > 0 ? ad : (-ad).eval();
            break;
        case Opcode::OP_RECIP:
            od = ad / -pow(av, 2);
            break;

        case Opcode::CONST_VAR:
            od = ad;
            break;

        case Opcode::ORACLE:
          // evalBase might be set during derivative evaluation by a derived
          // class; if so, we want to evaluate the derivatives properly as 
          // well.
            if (evalBase) tape->oracles[a_]->baseEvalDerivs(od);
            else tape->oracles[a_]->evalDerivs(od);
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

