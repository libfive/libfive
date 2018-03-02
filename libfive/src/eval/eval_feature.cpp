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
#include "libfive/eval/eval_feature.hpp"

namespace Kernel {

FeatureEvaluator::FeatureEvaluator(std::shared_ptr<Tape> t)
    : FeatureEvaluator(t, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

FeatureEvaluator::FeatureEvaluator(
        std::shared_ptr<Tape> t, const std::map<Tree::Id, float>& vars)
    : PointEvaluator(t, vars), d(1, tape->num_clauses + 1)
{
    // Load the default derivatives
    d(tape->X).push_back(Feature(Eigen::Vector3f(1, 0, 0)));
    d(tape->Y).push_back(Feature(Eigen::Vector3f(0, 1, 0)));
    d(tape->Z).push_back(Feature(Eigen::Vector3f(0, 0, 1)));

    // Set variables to have a single all-zero derivative
    for (auto& v : t->vars.right)
    {
        d(v.second).push_back(Feature(Eigen::Vector3f::Zero()));
    }

    // Set constants to have a single all-zero derivative
    for (auto& c : tape->constants)
    {
        d(c.first).push_back(Feature(Eigen::Vector3f::Zero()));
    }
}

bool FeatureEvaluator::isInside(const Eigen::Vector3f& p)
{
    auto v = eval(p);

    // Unambiguous cases
    if (v < 0)
    {
        return true;
    }
    else if (v > 0)
    {
        return false;
    }

    // Otherwise, we need to handle the zero-crossing case!

    // First, we extract all of the features
    auto fs = features_(p);

    // If there's only a single feature, we can get both positive and negative
    // values out if it's got a non-zero gradient
    if (fs.size() == 1)
    {
        return fs.front().deriv.norm() > 0;
    }

    // Otherwise, check each feature
    // The only case where we're outside the model is if all features
    // and their normals are all positive (i.e. for every epsilon that
    // we move from (x,y,z), epsilon . deriv > 0)
    bool pos = false;
    bool neg = false;
    for (auto& f : fs)
    {
        pos |= f.check(f.deriv);
        neg |= f.check(-f.deriv);
    }
    return !(pos && !neg);

}

const boost::container::small_vector<Feature, 4>&
    FeatureEvaluator::features_(const Eigen::Vector3f& p)
{
    // Load the location into the results slot and evaluate point-wise
    evalAndPush(p);

    // Evaluate feature-wise
    auto index = tape->rwalk(*this);

    // Pop out of point-wise specialization
    pop();

    return d(index);
}

std::list<Eigen::Vector3f> FeatureEvaluator::features(const Eigen::Vector3f& p)
{
    // Deduplicate and return the result
    std::list<Eigen::Vector3f> out;
    for (auto& o : features_(p))
    {
        if (std::find(out.begin(), out.end(), o.deriv) == out.end())
        {
            out.push_back(o.deriv);
        }
    }
    return out;
}

void FeatureEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                  Clause::Id a, Clause::Id b, Clause::Id cond)
{
#define ov f(id)
#define od d(id)

#define av f(a)
#define _ads d(a)
#define ad _ad.deriv

#define bv f(b)
#define _bds d(b)
#define bd _bd.deriv

#define cond f(cond)


#define LOOP2 \
    for (auto& _ad : _ads) \
        for (auto& _bd : _bds)

#define STORE2(expr) LOOP2 \
    if (_ad.check(_bd)) \
        od.push_back(Feature(expr, _ad, _bd));

#define STORE1(expr) \
    for (auto& _ad : _ads) \
        od.push_back(Feature(expr, _ad));

    od.clear();

    switch (op) {
        case Opcode::ADD:
            STORE2(ad + bd);
            break;
        case Opcode::MUL:
            // Product rule
            STORE2(bd*av + ad*bv);
            break;
        case Opcode::MIN:
            if (av < bv || a == b)
            {
                od = _ads;
            }
            else if (av > bv)
            {
                od = _bds;
            }
            else LOOP2
            {
                Eigen::Vector3f epsilon = bd - ad;
                if (epsilon.norm() == 0)
                {
                    od.push_back(_ad);
                }
                else
                {
                    auto fa = _ad;
                    if (fa.push(epsilon))
                    {
                        od.push_back(fa);
                    }
                    auto fb = _bd;
                    if (fb.push(-epsilon))
                    {
                        od.push_back(fb);
                    }
                }
            }
            break;
        case Opcode::MAX:
            if (av < bv || a == b)
            {
                od = _bds;
            }
            else if (av > bv)
            {
                od = _ads;
            }
            else if (a == b)
            {
                od = _ads;
            }
            else LOOP2
            {
                Eigen::Vector3f epsilon = ad - bd;
                if (epsilon.norm() == 0)
                {
                    od.push_back(_ad);
                }
                else
                {
                    auto fa = _ad;
                    if (fa.push(epsilon))
                    {
                        od.push_back(fa);
                    }
                    auto fb = _bd;
                    if (fb.push(-epsilon))
                    {
                        od.push_back(fb);
                    }
                }
            }
            break;
        case Opcode::SUB:
            STORE2(ad - bd);
            break;
        case Opcode::DIV:
            STORE2((ad*bv - bd*av) / pow(bv, 2));
            break;
        case Opcode::ATAN2:
            STORE2((ad*bv - bd*av) / (pow(av, 2) + pow(bv, 2)));
            break;
        case Opcode::POW:
            // The full form of the derivative is
            // od = m * (bv * ad + av * log(av) * bd))
            // However, log(av) is often NaN and bd is always zero,
            // (since it must be CONST), so we skip that part.
            STORE2(ad * bv * pow(av, bv - 1));
            break;

        case Opcode::NTH_ROOT:
            STORE2((ad.array() == 0)
                    .select(0, ad * pow(av, 1.0f / bv - 1) / bv));
            break;
        case Opcode::MOD:
            od = _ads;
            break;
        case Opcode::NANFILL:
            od = std::isnan(av) ? _bds : _ads;
            break;
        case Opcode::COMPARE:
            od.push_back(Feature(Eigen::Vector3f::Zero()));
            break;

        case Opcode::SQUARE:
            STORE1(ad * av * 2);
            break;
        case Opcode::SQRT:
            STORE1(av < 0 ? Eigen::Vector3f::Zero().eval() : (ad / (2 * ov)));
            break;
        case Opcode::NEG:
            STORE1(-ad);
            break;
        case Opcode::SIN:
            STORE1(ad * cos(av));
            break;
        case Opcode::COS:
            STORE1(ad * -sin(av));
            break;
        case Opcode::TAN:
            STORE1(ad * pow(1/cos(av), 2));
            break;
        case Opcode::ASIN:
            STORE1(ad / sqrt(1 - pow(av, 2)));
            break;
        case Opcode::ACOS:
            STORE1(ad / -sqrt(1 - pow(av, 2)));
            break;
        case Opcode::ATAN:
            STORE1(ad / (pow(av, 2) + 1));
            break;
        case Opcode::LOG:
            STORE1(ad / av);
            break;
        case Opcode::EXP:
            STORE1(ad * exp(av));
            break;
        case Opcode::ABS:
            STORE1(av > 0 ? ad : (-ad).eval());
            break;
        case Opcode::RECIP:
            STORE1(ad / -pow(av, 2));
            break;

        case Opcode::CONST_VAR:
            od = _ads;
            break;

        case Opcode::CHOOSE:
            if (cond == 1)
            {
                od = _ads;
            }
            else if (cond == 0)
            {
                od = _bds;
            }
            else LOOP2
            {
                Eigen::Vector3f epsilon = ad - bd;
                if (epsilon.norm() == 0)
                {
                    od.push_back(_ad);
                }
                else
                {
                    auto fa = _ad;
                    if (fa.push(epsilon))
                    {
                        od.push_back(fa);
                    }
                    auto fb = _bd;
                    if (fb.push(-epsilon))
                    {
                        od.push_back(fb);
                    }
                }
            }
            break;

        case Opcode::ORACLE:
            tape->oracles[a]->evalFeatures(od);
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
#undef _ads
#undef ad

#undef bv
#undef _bds
#undef bd

#undef STORE1
#undef STORE2
}

}   // namespace Kernel
