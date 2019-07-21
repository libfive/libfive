/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/eval_deriv_array.hpp"
#include "libfive/eval/feature.hpp"
#include "libfive/render/brep/region.hpp"

namespace libfive {

/*
 *  FeatureEvaluator is an enhanced version of DerivEvaluator.
 *
 *  Instead of recording one derivative in ambiguous cases
 *  (e.g. min(x, y) at 0,0), it stores and returns all of them.
 *
 *  This is more expensive, but also more awesome.
 */
class FeatureEvaluator : public DerivArrayEvaluator
{
public:
    FeatureEvaluator(const Tree& root);
    FeatureEvaluator(std::shared_ptr<Deck> d);
    FeatureEvaluator(std::shared_ptr<Deck> d,
                     const std::map<Tree::Id, float>& vars);

    /*
     *  Checks to see if the given point is inside the solid body.
     *  There are three cases
     *      eval(x, y, z) < 0  => true
     *      eval(x, y, z) > 0  => false
     *      eval(x, y, z) == 0 => further checking is performed
     */
    bool isInside(const Eigen::Vector3f& p);
    bool isInside(const Eigen::Vector3f& p, const std::shared_ptr<Tape>& tape);

    /*
     *  Helper function to reduce boilerplate
     */
    template <unsigned N>
    bool isInside(const Eigen::Matrix<double, N, 1>& p, const Region<N>& region,
                  const std::shared_ptr<Tape>& tape)
    {
        Eigen::Vector3f v;
        v << p.template cast<float>(), region.perp.template cast<float>();
        return isInside(v, tape);
    }

    /*
     *  Checks for features at the given position, returning a list
     *  of unique feature normals.
     */
    std::list<Eigen::Vector3f> features(const Eigen::Vector3f& p);
    std::list<Eigen::Vector3f> features(const Eigen::Vector3f& p,
                                        const std::shared_ptr<Tape>& tape);
    template <unsigned N>
    std::list<Eigen::Vector3f> features(const Eigen::Matrix<double, N, 1>& p,
                                        const Region<N>& region,
                                        const std::shared_ptr<Tape>& tape)
    {
        Eigen::Vector3f v;
        v << p.template cast<float>(), region.perp.template cast<float>();
        return features(v, tape);
    }

    /*
     *  Checks for features at the given position, returning a list
     *  of the raw features themselves
     */
    const boost::container::small_vector<Feature, 4>&
        features_(const Eigen::Vector3f& p);
    const boost::container::small_vector<Feature, 4>&
        features_(const Eigen::Vector3f& p, const std::shared_ptr<Tape>& tape);

protected:
    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*  Raw feature data */
    Eigen::Array<boost::container::small_vector<Feature, 4>,
                 1, Eigen::Dynamic> f;

    /*  filled(id) represents how many slots of v.row(id) have been filled
     *  with v(id, 0).  This is needed because we do point-wise evaluation
     *  on a single slot (to get values), then need to copy that value into
     *  multiple slots to have the correct value for array-wise derivative
     *  evaluations. */
    Eigen::Array<unsigned, 1, Eigen::Dynamic> filled;
};

}   // namespace libfive
