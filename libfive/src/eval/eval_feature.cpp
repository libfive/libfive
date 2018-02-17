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
    : DerivEvaluator(t, vars), dOrAll(tape->num_clauses + 1)
{
    // Nothing to do here
}

Eigen::Vector4f FeatureEvaluator::deriv(const Eigen::Vector3f& pt)
{
    // Load gradients of oracles; only use the first gradient of each here
    // (following precedent for min and max evaluation).
    for (auto o : tape->oracles)
    {
        // Other than this line, it's the same as the DerivEvaluator version.
        dOrAll(o.first) = o.second.first->getGradients(pt);
        d.col(o.first) = o.second.first->getGradients(pt).begin()->first;
    }
    // Perform value evaluation, saving results
    auto w = eval(pt);
    auto xyz = d.col(tape->rwalk(*this));

    Eigen::Vector4f out;
    out << xyz, w;
    return out;
}

Eigen::Vector4f FeatureEvaluator::deriv(
    const Eigen::Vector3f& pt, const Feature& feature)
{
    if (feature.getOracleChoices().empty())
    {
        return deriv(pt);
    }
    // Load gradients of ambiguous primitives (non-ambiguous ones are
    // assumed to have already been loaded).
    for (auto o : feature.getOracleChoices())
    {
        d.col(o.id) = o.choice;
    }

    // Perform derivative evaluation, saving results
    // (value evaluation is assumed to have already been done.)

    auto w = eval(pt);
    auto xyz = d.col(tape->rwalk(*this));

    Eigen::Vector4f out;
    out << xyz, w;
    return out;
}

Feature FeatureEvaluator::push(const Feature& feature)
{
    Feature out;
    out.deriv = feature.deriv;

    const auto& choices = feature.getChoices();
    auto itr = choices.begin();

    tape->push([&](Opcode::Opcode op, Clause::Id id, Clause::Id a, Clause::Id b)
    {
        // First, check whether this is an ambiguous operation
        // If it is, then there may be a choice for it in the feature
        if (op == Opcode::MAX || op == Opcode::MIN)
        {
            if (f(a, 0) == f(b, 0) || a == b)
            {
                // Walk the iterator forwards until we find a match by id
                // or hit the end of the feature
                while (itr != choices.end() && itr->id < id) { itr++; }

                // Push either the choice + epsilon or the bare choice to the
                // output feature, effectively pruning it to contain only the
                // choices that are actually significant in this subtree.
                if (itr != choices.end() && itr->id == id)
                {
                    if (feature.hasEpsilon(id))
                    {
                        out.pushRaw(*itr, feature.getEpsilon(id));
                    }
                    else
                    {
                        out.pushChoice(*itr);
                    }

                    return (itr->choice == 0) ? Tape::KEEP_A : Tape::KEEP_B;
                }
            }
            return Tape::KEEP_BOTH;
        }
        else
        {
            return Tape::KEEP_ALWAYS;
        }
    }, Tape::FEATURE);

    return out;
}

bool FeatureEvaluator::isInside(const Eigen::Vector3f& p)
{
    auto ds = deriv(p);

    // Unambiguous cases
    if (ds.w() < 0)
    {
        return true;
    }
    else if (ds.w() > 0)
    {
        return false;
    }

    // Special case to save time on non-ambiguous features: we can get both
    // positive and negative values out if there's a non-zero gradient
    // (same as single-feature case below).
    {
        bool ambig = false;
        tape->walk(
            [&](Opcode::Opcode op, Clause::Id id, Clause::Id a, Clause::Id b)
            {
                ambig |= (op == Opcode::MIN || op == Opcode::MAX) &&
                         (f(a) == f(b));
                ambig |= (op == Opcode::ORACLE && dOrAll(id).size() > 1);
            }, ambig);

        if (!ambig)
        {
            return (ds.col(0).template head<3>().array() != 0).any();
        }
    }

    // Otherwise, we need to handle the zero-crossing case!

    // First, we extract all of the features
    auto fs = featuresAt(p);

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
        pos |= f.isCompatible(f.deriv);
        neg |= f.isCompatible(-f.deriv);
    }
    return !(pos && !neg);

}

std::list<Feature> FeatureEvaluator::featuresAt(const Eigen::Vector3f& p)
{
    // The initial feature doesn't know any ambiguities
    Feature feature;
    std::list<Feature> todo = {feature};
    std::list<Feature> done;
    std::set<std::pair<std::set<Feature::Choice>,
             std::set<Feature::OracleChoice>>> seen;
    bool derivsGathered = false; //Set to true when deriv is first called.

    // Load the location into the first results slot and evaluate
    evalAndPush(p);

    while (todo.size())
    {
        // Take the most recent feature and scan for ambiguous min/max nodes
        // (from the bottom up).  If we find such an ambiguous node, then push
        // both versions to the feature (if compatible) and re-insert the
        // augmented feature in the todo list; otherwise, move the feature
        // to the done list.
        auto feature = todo.front();
        todo.pop_front();

        // Then, push into this feature
        // (storing a minimized version of the feature)
        auto f_ = push(feature);

        // Evaluate the derivatives at this point for this feature. 
        // The value will be the same throughout, but derivatives may change
        // depending on which feature we're in.

        // The first call has a blank feature, so it populates non-ambiguous 
        // oracles and gets and stores all gradients for all oracles.

        const Eigen::Vector4f ds = derivsGathered
            ? deriv(p, feature) 
            : deriv(p);
        derivsGathered = true;

        bool ambiguous = false;

        //Any time there is a branching choice, at least one choice should have
        //an epsilon compatible with all previous epsilons; this variable
        //is used in an assert to ensure this is actually the case.
        bool completeFeature = true;

        tape->rwalk(
            [&](Opcode::Opcode op, Clause::Id id, Clause::Id a, Clause::Id b)
            {
                if (op == Opcode::ORACLE && dOrAll(id).size() > 1)
                {
                    const auto& choices = feature.getChoices();
                    const auto& location = std::find_if(
                        choices.begin(), choices.end(),
                        [id](Feature::Choice choice)
                            {return choice.id == id; });
                    if (location == choices.end())
                    {
                        for (auto choice : dOrAll(id))
                        {
                            auto rejected = false;
                            auto fNew = f_;
                            for (auto choice2 : dOrAll(id))
                            {
                                if (choice != choice2)
                                {
                                    completeFeature = false;
                                    Eigen::Vector3f epsilon =
                                        choice.second - choice2.second;
                                    if (!fNew.push(
                                        epsilon.template cast<double>(),
                                        { id, choice.first }))
                                    {
                                        rejected = true;
                                        break;
                                    }
                                }
                            }
                            if (!rejected) {
                                ambiguous = true;
                                todo.push_back(fNew);
                            }
                        }
                    }
                }
                else if ((op == Opcode::MIN || op == Opcode::MAX))
                {
                    // If we've ended up with a non-selection, then collapse
                    // it to a single choice
                    if (a == b)
                    {
                        auto fa = f_;
                        fa.pushChoice({id, 0});
                        todo.push_back(fa);
                        ambiguous = true;
                    }
                    // Check for ambiguity here
                    else if (f(a, 0) == f(b, 0))
                    {
                        // Check both branches of the ambiguity
                        const Eigen::Vector3d rhs(
                                d.col(b).template cast<double>());
                        const Eigen::Vector3d lhs(
                                d.col(a).template cast<double>());
                        const auto epsilon = (op == Opcode::MIN) ? (rhs - lhs)
                                                                      : (lhs - rhs);

                        auto fa = f_;
                        completeFeature = false;
                        if (fa.push(epsilon, {id, 0}))
                        {
                            ambiguous = true;
                            todo.push_back(fa);
                        }

                        auto fb = f_;
                        if (fb.push(-epsilon, {id, 1}))
                        {
                            ambiguous = true;
                            todo.push_back(fb);
                        }
                    }
                }
            }, ambiguous);
        if (!ambiguous)
        {
            assert(completeFeature);
            f_.deriv = ds.col(0).template head<3>().template cast<double>();
            if (seen.find({ f_.getChoices(), f_.getOracleChoices() }) == seen.end())
            {
                seen.insert({ f_.getChoices(), f_.getOracleChoices() });
                done.push_back(f_);
            }
        }
        pop(); // push(Feature)
    }
    pop(); // specialization

    assert(done.size() > 0);
    return done;

}

}   // namespace Kernel
