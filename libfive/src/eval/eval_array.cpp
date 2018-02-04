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
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(t, vars), f(tape->num_clauses + 1, N)
{
    // Unpack variables into result array
    for (auto& v : t->vars.right)
    {
        auto var = vars.find(v.first);
        f.row(v.second) = (var != vars.end()) ? var->second : 0;
    }

    // Unpack constants into result array
    for (auto& c : tape->constants)
    {
        f.row(c.first) = c.second;
    }
}

float ArrayEvaluator::eval(const Eigen::Vector3f& pt)
{
    set(pt, 0);
    return values(1)(0);
}

float ArrayEvaluator::evalAndPush(const Eigen::Vector3f& pt)
{
    auto out = eval(pt);
    tape->push([&](Opcode::Opcode op, Clause::Id /* id */,
                  Clause::Id a, Clause::Id b)
    {
        // For min and max operations, we may only need to keep one branch
        // active if it is decisively above or below the other branch.
        if (op == Opcode::MAX)
        {
            if (f(a, 0) > f(b, 0))
            {
                return Tape::KEEP_A;
            }
            else if (f(b, 0) > f(a, 0))
            {
                return Tape::KEEP_B;
            }
            else
            {
                return Tape::KEEP_BOTH;
            }
        }
        else if (op == Opcode::MIN)
        {
            if (f(a, 0) > f(b, 0))
            {
                return Tape::KEEP_B;
            }
            else if (f(b, 0) > f(a, 0))
            {
                return Tape::KEEP_A;
            }
            else
            {
                return Tape::KEEP_BOTH;
            }
        }
        else if (op == Opcode::ADD)
        {
            if (f(a, 0) == 0)
            {
                return Tape::KEEP_B;
            }
            else if (f(b, 0) == 0)
            {
                return Tape::KEEP_A;
            }
            else
            {
                return Tape::KEEP_BOTH;
            }
        }
        else if (op == Opcode::MUL)
        {
            if (f(a, 0) == 1)
            {
                return Tape::KEEP_B;
            }
            else if (f(b, 0) == 1)
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
    return out;
}

Eigen::Block<decltype(ArrayEvaluator::f), 1, Eigen::Dynamic>
ArrayEvaluator::values(size_t _count)
{
    count = _count;
    return f.block<1, Eigen::Dynamic>(tape->rwalk(*this), 0, 1, count);
}

////////////////////////////////////////////////////////////////////////////////

bool ArrayEvaluator::setVar(Tree::Id var, float value)
{
    auto v = tape->vars.right.find(var);
    if (v != tape->vars.right.end())
    {
        bool changed = f(v->second, 0) != value;
        f.row(v->second) = value;
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
    // Reset the ambiguous array to all false
    ambig = false;

    bool abort = false;
    tape->walk(
        [&](Opcode::Opcode op, Clause::Id /* id */, Clause::Id a, Clause::Id b)
        {
            if (op == Opcode::MIN || op == Opcode::MAX)
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
                                Clause::Id a, Clause::Id b)
{
#define out f.row(id).head(count)
#define a f.row(a).head(count)
#define b f.row(b).head(count)
    switch (op)
    {
        case Opcode::ADD:
            out = a + b;
            break;
        case Opcode::MUL:
            out = a * b;
            break;
        case Opcode::MIN:
            out = a.cwiseMin(b);
            break;
        case Opcode::MAX:
            out = a.cwiseMax(b);
            break;
        case Opcode::SUB:
            out = a - b;
            break;
        case Opcode::DIV:
            out = a / b;
            break;
        case Opcode::ATAN2:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = atan2(a(i), b(i));
            }
            break;
        case Opcode::POW:
            out = a.pow(b);
            break;
        case Opcode::NTH_ROOT:
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
        case Opcode::MOD:
            for (auto i=0; i < a.size(); ++i)
            {
                out(i) = std::fmod(a(i), b(i));
                while (out(i) < 0)
                {
                    out(i) += b(i);
                }
            }
            break;
        case Opcode::NANFILL:
            out = a.isNaN().select(b, a);
            break;
        case Opcode::COMPARE:
            for (auto i=0; i < a.size(); ++i)
            {
                if      (a(i) < b(i))   out(i) = -1;
                else if (a(i) > b(i))   out(i) =  1;
                else                    out(i) =  0;
            }
            break;

        case Opcode::SQUARE:
            out = a * a;
            break;
        case Opcode::SQRT:
            out = sqrt(a);
            break;
        case Opcode::NEG:
            out = -a;
            break;
        case Opcode::SIN:
            out = sin(a);
            break;
        case Opcode::COS:
            out = cos(a);
            break;
        case Opcode::TAN:
            out = tan(a);
            break;
        case Opcode::ASIN:
            out = asin(a);
            break;
        case Opcode::ACOS:
            out = acos(a);
            break;
        case Opcode::ATAN:
            out = atan(a);
            break;
        case Opcode::LOG:
            out = log(a);
            break;
        case Opcode::EXP:
            out = exp(a);
            break;
        case Opcode::ABS:
            out = abs(a);
            break;
        case Opcode::RECIP:
            out = 1 / a;
            break;

        case Opcode::CONST_VAR:
            out = a;
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

