/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/eval_feature.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"

namespace libfive {

FeatureEvaluator::FeatureEvaluator(const Tree& root)
    : FeatureEvaluator(std::make_shared<Deck>(root))
{
    // Nothing to do here
}

FeatureEvaluator::FeatureEvaluator(std::shared_ptr<Deck> d)
    : FeatureEvaluator(d, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

FeatureEvaluator::FeatureEvaluator(
        std::shared_ptr<Deck> t, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(t, vars), DerivArrayEvaluator(t, vars),
      f(1, deck->num_clauses + 1), filled(1, deck->num_clauses + 1)
{
    // Load the default derivatives
    f(deck->X).push_back(Feature(Eigen::Vector3f(1, 0, 0)));
    f(deck->Y).push_back(Feature(Eigen::Vector3f(0, 1, 0)));
    f(deck->Z).push_back(Feature(Eigen::Vector3f(0, 0, 1)));

    // Set variables to have a single all-zero derivative
    for (auto& v : t->vars.right)
    {
        f(v.second).push_back(Feature(Eigen::Vector3f::Zero()));
    }

    // Set constants to have a single all-zero derivative
    for (auto& c : deck->constants)
    {
        f(c.first).push_back(Feature(Eigen::Vector3f::Zero()));
    }
}

bool FeatureEvaluator::isInside(const Eigen::Vector3f& p)
{
    return isInside(p, deck->tape);
}

bool FeatureEvaluator::isInside(const Eigen::Vector3f& p,
                                const Tape::Handle& tape)
{
    auto handle = valueAndPush(p, tape);

    // Unambiguous cases
    if (handle.first < 0) {
        if (handle.second != tape) {
            deck->claim(std::move(handle.second));
        }
        return true;
    } else if (handle.first > 0) {
        if (handle.second != tape) {
            deck->claim(std::move(handle.second));
        }
        return false;
    }

    // Otherwise, we need to handle the zero-crossing case!

    // Mark that only the first slot is valid
    filled = 1;

    // First, we evaluate and extract all of the features, saving
    // time by re-using the shortened tape from valueAndPush
    for (auto itr = handle.second->rbegin();
         itr != handle.second->rend();
         ++itr)
    {
        (*this)(itr->op, itr->id, itr->a, itr->b);
    }
    auto fs = f(handle.second->root());

    // If this is a freshly allocated tape, then release it to the Deck
    // so that it can be reused later.
    if (handle.second != tape) {
        deck->claim(std::move(handle.second));
    }

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
    const bool outside = pos && !neg;

    return !outside;
}

const boost::container::small_vector<Feature, 4>&
    FeatureEvaluator::features_(const Eigen::Vector3f& p)
{
    return features_(p, deck->tape);
}

const boost::container::small_vector<Feature, 4>&
    FeatureEvaluator::features_(const Eigen::Vector3f& p,
                                const Tape::Handle& tape)
{
    // Load the location into the results slot and evaluate point-wise
    auto handle = valueAndPush(p, tape);
    filled = 1;

    // Evaluate feature-wise
    deck->bindOracles(*handle.second);
    for (auto itr = handle.second->rbegin(); itr != handle.second->rend(); ++itr) {
        (*this)(itr->op, itr->id, itr->a, itr->b);
    }
    deck->unbindOracles();

    const auto root = handle.second->root();

    // If this is a freshly allocated tape, then release it to the Deck
    // so that it can be reused later.
    if (handle.second != tape) {
        deck->claim(std::move(handle.second));
    }

    return f(root);
}

std::list<Eigen::Vector3f> FeatureEvaluator::features(const Eigen::Vector3f& p)
{
    return features(p, deck->tape);
}

std::list<Eigen::Vector3f> FeatureEvaluator::features(
        const Eigen::Vector3f& p,
        const Tape::Handle& tape)
{
    // Deduplicate and return the result
    std::list<Eigen::Vector3f> out;
    for (auto& o : features_(p, tape))
    {
        if (std::find(out.begin(), out.end(), o.deriv) == out.end())
        {
            out.push_back(o.deriv);
        }
    }
    return out;
}

void FeatureEvaluator::operator()(Opcode::Opcode op, Clause::Id id,
                                  Clause::Id a, Clause::Id b)
{
#define of f(id)

#define av v(a, 0)
#define _ads f(a)
#define ad _ad.deriv

#define bv v(b, 0)
#define _bds f(b)
#define bd _bd.deriv

#define LOOP2 \
    for (auto& _ad : _ads) \
        for (auto& _bd : _bds)

    of.clear();

    if (op == Opcode::OP_MIN) {
        if (av < bv || a == b) {
            of = _ads;
        } else if (av > bv) {
            of = _bds;
        }
        else LOOP2 {
            const Eigen::Vector3f epsilon = bd - ad;
            if (epsilon.norm() == 0) {
                if (_ad.hasEpsilons()) {
                    of.push_back(_ad);
                }
                if (_bd.hasEpsilons()) {
                    of.push_back(_bd);
                }
                if (!_ad.hasEpsilons() && !_bd.hasEpsilons()) {
                    of.push_back(_ad);
                }
            } else {
                // The new feature must be compatible with the epsilons
                // from both of the source features, plus the new epsilon
                // to select a particular branch of the max.
                auto combined = _ad;
                if (combined.push(_bd)) {
                    auto fa = combined;
                    fa.deriv = _ad.deriv;
                    if (fa.push(epsilon)) {
                        of.push_back(fa);
                    }

                    auto fb = combined;
                    fb.deriv = _bd.deriv;
                    if (fb.push(-epsilon)) {
                        of.push_back(fb);
                    }
                }
            }
        }
    } else if (op == Opcode::OP_MAX) {
        if (av < bv || a == b) {
            of = _bds;
        } else if (av > bv) {
            of = _ads;
        } else LOOP2 {
            const Eigen::Vector3f epsilon = ad - bd;
            if (epsilon.norm() == 0) {
                if (_ad.hasEpsilons()) {
                    of.push_back(_ad);
                }
                if (_bd.hasEpsilons()) {
                    of.push_back(_bd);
                }
                if (!_ad.hasEpsilons() && !_bd.hasEpsilons()) {
                    of.push_back(_ad);
                }
            } else {
                // The new feature must be compatible with the epsilons
                // from both of the source features, plus the new epsilon
                // to select a particular branch of the max.
                auto combined = _ad;
                if (combined.push(_bd)) {
                    auto fa = combined;
                    fa.deriv = _ad.deriv;
                    if (fa.push(epsilon)) {
                        of.push_back(fa);
                    }

                    auto fb = combined;
                    fb.deriv = _bd.deriv;
                    if (fb.push(-epsilon)) {
                        of.push_back(fb);
                    }
                }
            }
        }
    } else if (op == Opcode::ORACLE) {
        deck->oracles[a]->evalFeatures(f(id));
    } else if (Opcode::args(op) == 1) {
        unsigned count = 0;
        auto run = [&]() {
            if (count) {
                // Make sure that the values arrays are correctly filled up,
                // because we'll be doing an array-wise evaluation, but
                // previous only solved for the value in slot 0 (so filled(a)
                // is 1 to start).
                if (count > filled(a)) {
                    v.row(a).leftCols(count)  = v(a, 0);
                    filled(a) = count;
                }
                setCount(count);
                DerivArrayEvaluator::operator()(op, id, a, b);
                for (unsigned i=0; i < count; ++i) {
                    of.push_back(Feature(d(id).col(i), _ads[i]));
                }
            }
            count = 0;
        };

        for (auto& _ad : _ads) {
            d(a).col(count++) = _ad.deriv;
            if (count == N) {
                run();
            }
        }
        run();
    } else if (Opcode::args(op) == 2) {
        unsigned count = 0;
        auto run = [&]() {
            if (count) {
                // Same logic as above
                if (count > filled(a)) {
                    v.row(a).leftCols(count)  = v(a, 0);
                    filled(a) = count;
                }
                if (count > filled(b)) {
                    v.row(b).leftCols(count)  = v(b, 0);
                    filled(b) = count;
                }
                DerivArrayEvaluator::operator()(op, id, a, b);
                for (unsigned i=0; i < count; ++i) {
                    of.push_back(Feature(d(id).col(i), _ads[i / _bds.size()],
                                                       _bds[i % _bds.size()]));
                }
            }
            count = 0;
        };

        for (auto& _ad : _ads) {
            for (auto& _bd : _bds) {
                d(a).col(count) = _ad.deriv;
                d(b).col(count) = _bd.deriv;
                if (++count == N) {
                    run();
                }
            }
        }
        run();
    }

    // Now to deduplicate.
    if (of.size() > 1)
    {
        std::sort(of.begin(), of.end());
        // Now we walk through and remove any that are essentially the
        // same as the last one we kept.  This may occasionally miss 
        // near-duplicates despite sorting (e.g. 0,0,0 is followed by 1,0,0, 
        // followed by 0, 1e-8, 0), but that should be rare enough to not be
        // an issue.
        auto newEnd = std::unique(of.begin(), of.end(),
                                  [](const Feature& f1, const Feature& f2)
        {
            // Not an equivalence relation, so behavior of std::unique is 
            // technically undefined.  Reasonable implementations should avoid
            // problems for any remotely plausible feature lists, but consider
            // replacing with an explicit implementation to make sure.
            auto derivDiff = f1.deriv - f2.deriv;
            return (derivDiff.dot(derivDiff) <= 1e-10 &&
                    f1.hasSameEpsilons(f2));
        });
        of.erase(newEnd, of.end());
        auto& firstDeriv = of.front().deriv;
        if (std::all_of(of.begin(), of.end(), [&firstDeriv](const Feature& f)
        {
            auto derivDiff = f.deriv - firstDeriv;
            return derivDiff.dot(derivDiff) < 1e-10;
        }))
        {
            // Collapse into a single feature with no epsilons.
            of = { firstDeriv };
        }
    }
#undef of

#undef av
#undef _ads
#undef ad

#undef bv
#undef _bds
#undef bd

#undef LOOP2
}

}   // namespace libfive
