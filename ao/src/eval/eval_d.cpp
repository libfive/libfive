#include "ao/eval/eval_d.hpp"

namespace Kernel {

DerivArrayEvaluator::DerivArrayEvaluator(Tape& t)
    : DerivArrayEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

DerivArrayEvaluator::DerivArrayEvaluator(
        Tape& t, const std::map<Tree::Id, float>& vars)
    : ArrayEvaluator(t, vars), d(t.num_clauses, 1)
{
    // Load immutable derivatives for X / Y / Z
    d(tape.X).row(0) = 1;
    d(tape.Y).row(1) = 1;
    d(tape.Z).row(2) = 1;
}

Eigen::Vector4f DerivArrayEvaluator::deriv(const Eigen::Vector3f& pt)
{
    set(pt, 0);
    return derivs(1).col(0);
}

Eigen::Block<DerivArrayEvaluator::ResultArray, 4, Eigen::Dynamic>
DerivArrayEvaluator::derivs(size_t count)
{
    // Perform value evaluation and store to the result array (f)
    values(count);

    // For every clause, copy over the result into the bottom slot
    // of the 4-row derivatives array.  This lets us keep results
    // together and makes evaluation cheaper (maybe)
    for (long i=0; i < d.rows(); ++i)
    {
        d(i).row(3) = f.row(i);
    }

    return d(tape.walk(*this)).block<4, Eigen::Dynamic>(0, 0, 4, count);
}

void DerivArrayEvaluator::evalClause(Opcode::Opcode op, Clause::Id id,
                                     Clause::Id a, Clause::Id b)
{
#define ov d(id).row(3).head(count)
#define od d(id).leftCols(count)

#define av d(a).row(3).head(count)
#define ad d(a).leftCols(count)

#define bv d(b).row(3).head(count)
#define bd d(b).leftCols(count)

    switch (op) {
        case Opcode::ADD:
            od = ad + bd;
            break;
        case Opcode::MUL:
            // Product rule
            od = bd.rowwise()*av + ad.rowwise()*bv;
            break;
        case Opcode::MIN:
            for (unsigned i=0; i < od.rows(); ++i)
                od.row(i) = (av < bv).select(ad.row(i), bd.row(i));
            break;
        case Opcode::MAX:
            for (unsigned i=0; i < od.rows(); ++i)
                od.row(i) = (av < bv).select(bd.row(i), ad.row(i));
            break;
        case Opcode::SUB:
            od = ad - bd;
            break;
        case Opcode::DIV:
            od = (ad.rowwise()*bv - bd.rowwise()*av).rowwise() /
                 bv.pow(2);
            break;
        case Opcode::ATAN2:
            od = (ad.rowwise()*bv - bd.rowwise()*av).rowwise() /
                 (av.pow(2) + bv.pow(2));
            break;
        case Opcode::POW:
            // The full form of the derivative is
            // od = m * (bv * ad + av * log(av) * bd))
            // However, log(av) is often NaN and bd is always zero,
            // (since it must be CONST), so we skip that part.
            od = ad.rowwise() * (bv * av.pow(bv - 1));
            break;

        case Opcode::NTH_ROOT:
            od = ad.rowwise() * (av.pow(1.0f / bv - 1) / bv);
            break;
        case Opcode::MOD:
            od = ad;
            break;
        case Opcode::NANFILL:
            for (unsigned i=0; i < od.rows(); ++i)
                od.row(i) = av.isNaN().select(bd.row(i), ad.row(i));
            break;

        case Opcode::SQUARE:
            od = ad.rowwise() * av * 2;
            break;
        case Opcode::SQRT:
            for (unsigned i=0; i < od.rows(); ++i)
                od.row(i) = (av < 0).select(
                    Eigen::Array<float, 1, Eigen::Dynamic>::Zero(1, count),
                    ad.row(i) / (2 * ov));
            break;
        case Opcode::NEG:
            od = -ad;
            break;
        case Opcode::SIN:
            od = ad.rowwise() * cos(av);
            break;
        case Opcode::COS:
            od = ad.rowwise() * -sin(av);
            break;
        case Opcode::TAN:
            od = ad.rowwise() * pow(1/cos(av), 2);
            break;
        case Opcode::ASIN:
            od = ad.rowwise() / sqrt(1 - pow(av, 2));
            break;
        case Opcode::ACOS:
            od = ad.rowwise() / -sqrt(1 - pow(av, 2));
            break;
        case Opcode::ATAN:
            od = ad.rowwise() / (pow(av, 2) + 1);
            break;
        case Opcode::EXP:
            od = ad.rowwise() * exp(av);
            break;
        case Opcode::ABS:
            for (unsigned i=0; i < od.rows(); ++i)
                od.row(i) = (av > 0).select(ad.row(i), -ad.row(i));
            break;
        case Opcode::RECIP:
            od = ad.rowwise() / -av.pow(2);
            break;

        case Opcode::CONST_VAR:
            od = ad;
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
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
