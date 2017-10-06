#include "ao/eval/eval_jacobian.hpp"

namespace Kernel {

JacobianEvaluator::JacobianEvaluator(std::shared_ptr<Tape> t)
    : JacobianEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

JacobianEvaluator::JacobianEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : DerivEvaluator(t, vars), j(tape->num_clauses + 1, tape->vars.size())
{
    // Initialize Jacobian array to all zeros
    j = 0;

    // Then drop a 1 at each var's position
    size_t index = 0;
    for (auto& v : tape->vars.left)
    {
        j(v.first, index++) = 1;
    }
}

std::map<Tree::Id, float> JacobianEvaluator::gradient(const Eigen::Vector3f& p)
{
    // Perform value evaluation, to make sure the f array is correct
    eval(p);

    // Everybody do the tape walk!
    auto ti = tape->rwalk(
        [=](Opcode::Opcode op, Clause::Id id,
            Clause::Id a, Clause::Id b)
            { evalClause(op, id, a, b); });

    // Unpack from flat array into map
    // (to allow correlating back to VARs in Tree)
    std::map<Tree::Id, float> out;
    size_t index = 0;
    for (auto v : tape->vars.left)
    {
        out[v.second] = j(ti, index++);
    }
    return out;
}

void JacobianEvaluator::evalClause(Opcode::Opcode op, Clause::Id id,
                                   Clause::Id a, Clause::Id b)
{
#define ov f(id)
#define oj j.row(id)

#define av f(a)
#define aj j.row(a)

#define bv f(b)
#define bj j.row(b)

        switch (op) {
            case Opcode::ADD:
                oj = aj + bj;
                break;
            case Opcode::MUL:
                oj = av * bj + bv * aj;
                break;
            case Opcode::MIN:
                oj = (av < bv) ? aj : bj;
                break;
            case Opcode::MAX:
                oj = (av < bv) ? bj : aj;
                break;
            case Opcode::SUB:
                oj = aj - bj;
                break;
            case Opcode::DIV:
                oj = (bv*aj - av*bj) / pow(bv, 2);
                break;
            case Opcode::ATAN2:
                oj = (aj*bv - av*bj) / (pow(av, 2) + pow(bv, 2));
                break;
            case Opcode::POW:
                // The full form of the derivative is
                // oj = m * (bv * aj + av * log(av) * bj))
                // However, log(av) is often NaN and bj is always zero,
                // (since it must be CONST), so we skip that part.
                oj = pow(av, bv - 1) * (bv * aj);
                break;
            case Opcode::NTH_ROOT:
                oj = pow(av, 1.0f/bv - 1) * (1.0f/bv * aj);
                break;
            case Opcode::MOD:
                // This isn't quite how partial derivatives of mod work,
                // but close enough normals rendering.
                oj = aj;
                break;
            case Opcode::NANFILL:
                oj = std::isnan(av) ? bj : aj;
                break;

            case Opcode::SQUARE:
                oj = 2 * av * aj;
                break;
            case Opcode::SQRT:
                if (av < 0) oj = 0.0;
                else        oj = (aj / (2 * sqrt(av)));
                break;
            case Opcode::NEG:
                oj = -aj;
                break;
            case Opcode::SIN:
                oj = aj * cos(av);
                break;
            case Opcode::COS:
                oj = aj * -sin(av);
                break;
            case Opcode::TAN:
                oj = aj * pow(1/cos(av), 2);
                break;
            case Opcode::ASIN:
                oj = aj / sqrt(1 - pow(av, 2));
                break;
            case Opcode::ACOS:
                oj = aj / -sqrt(1 - pow(av, 2));
                break;
            case Opcode::ATAN:
                oj = aj / (pow(av, 2) + 1);
                break;
            case Opcode::EXP:
                oj = exp(av) * aj;
                break;
            case Opcode::ABS:
                oj = (av > 0 ? 1 : -1) * aj;
                break;
            case Opcode::RECIP:
                oj = -aj / pow(av, 2);
                break;

            case Opcode::CONST_VAR:
                oj = 0;
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
#undef oj

#undef av
#undef aj

#undef bv
#undef bj
}

}   // namespace Kernel


