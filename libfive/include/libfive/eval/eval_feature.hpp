/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/eval_deriv.hpp"
#include "libfive/eval/feature.hpp"

namespace Kernel {

/*
 *  FeatureEvaluator is an enhanced version of DerivEvaluator.
 *
 *  Instead of recording one derivative in ambiguous cases
 *  (e.g. min(x, y) at 0,0), it stores and returns all of them.
 *
 *  This is more expensive, but also more awesome.
 */
class FeatureEvaluator : public PointEvaluator
{
public:
    FeatureEvaluator(std::shared_ptr<Deck> d);
    FeatureEvaluator(std::shared_ptr<Deck> d,
                     const std::map<Tree::Id, double>& vars);

    /*
     *  Checks to see if the given point is inside the solid body.
     *  There are three cases
     *      eval(x, y, z) < 0  => true
     *      eval(x, y, z) > 0  => false
     *      eval(x, y, z) == 0 => further checking is performed
     */
    bool isInside(const Eigen::Vector3d& p);
    bool isInside(const Eigen::Vector3d& p,
                  std::shared_ptr<Tape> tape);

    /*
     *  Checks for features at the given position, returning a list
     *  of unique feature normals.
     */
    std::list<Eigen::Vector3d> features(const Eigen::Vector3d& p);
    std::list<Eigen::Vector3d> features(const Eigen::Vector3d& p,
                                        std::shared_ptr<Tape> tape);

    /*
     *  Checks for features at the given position, returning a list
     *  of the raw features themselves
     */
    const boost::container::small_vector<Feature, 4>&
        features_(const Eigen::Vector3d& p);
    const boost::container::small_vector<Feature, 4>&
        features_(const Eigen::Vector3d& p, std::shared_ptr<Tape> tape);

protected:
    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    Eigen::Array<boost::container::small_vector<Feature, 4>,
                 1, Eigen::Dynamic> d;

    friend class Tape; // for rwalk<DerivEvaluator>
};

}   // namespace Kernel
