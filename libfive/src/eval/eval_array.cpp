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
#include "libfive/eval/eval_array.hpp"

namespace Kernel {

constexpr size_t ArrayEvaluator::N;

ArrayEvaluator::ArrayEvaluator(std::shared_ptr<Tape> t)
    : ArrayEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

ArrayEvaluator::ArrayEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vs)
    : BaseEvaluator(t, vs), f(tape->num_clauses + 1, N)
{
    // Nothing to do here
}

Eigen::Block<decltype(ArrayEvaluator::f), 1, Eigen::Dynamic>
ArrayEvaluator::values(size_t _count)
{
    count = _count;
    return f.block<1, Eigen::Dynamic>(tape->rwalk(*this), 0, 1, count);
}

////////////////////////////////////////////////////////////////////////////////

Eigen::Block<decltype(ArrayEvaluator::ambig), 1, Eigen::Dynamic>
ArrayEvaluator::getAmbiguous(size_t i)
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
                    (f.block(a, 0, 1, i) ==
                     f.block(b, 0, 1, i));
            }
        }, abort);

    return ambig.head(i);
}

////////////////////////////////////////////////////////////////////////////////

void ArrayEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                Clause::Id a_, Clause::Id b_)
{
#define out f.block<1, Eigen::Dynamic>(id, 0, 1, count)
#define a f.row(a_).head(count)
#define b f.row(b_).head(count)
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
            tape->oracles[a_]->evalArray(out);
            break;

        case Opcode::CONSTANT:
            out = tape->constants.at(a_);
            break;

        case Opcode::VAR_X:
            out = x.head(count);
            break;

        case Opcode::VAR_Y:
            out = y.head(count);
            break;

        case Opcode::VAR_Z:
            out = z.head(count);
            break;

        case Opcode::VAR_FREE:
            out = vars.at(a_);
            break;

        case Opcode::OP_COPY:
            out = a;
            break;

        case Opcode::INVALID:
        case Opcode::LAST_OP: assert(false);
    }
#undef out
#undef a
#undef b
}

}   // namespace Kernel

