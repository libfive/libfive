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

#pragma once

#include "libfive/tree/oracle.hpp"
#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/eval_xtree.hpp"



namespace Kernel {

class TransformedOracle : public Oracle {
public:

    // TransformedOracle may require deduplication if the same transformation
    // is applied to the same oracle on two separate occasions, so this is 
    // handled via a static member of the class.
    static std::shared_ptr<TransformedOracle> transform(
        const std::shared_ptr<const Oracle> underlying, 
        Tree X_, Tree Y_, Tree Z_);


    Interval::I getRange(Region<2> region, int threadNo) const override;
    Interval::I getRange(Region<3> region, int threadNo) const override;
    Oracle::GradientsWithEpsilons getGradients(
        Eigen::Vector3f point, int threadNo) const override;
    bool isAmbiguous(Eigen::Vector3f point, int threadNo) const override;
    float getValue(Eigen::Vector3f point, int threadNo) const override;

    //Custom destructor to remove from transformedOracles.
    ~TransformedOracle();

private:

    class JacobianMatrixWithEpsilons
    {
    public:
        JacobianMatrixWithEpsilons(std::array<FeatureEvaluator*, 3> evaluators,
            Eigen::Vector3f point);
        Oracle::GradientsWithEpsilons
            operator*(const Oracle::GradientsWithEpsilons& base) const;
        const boost::container::small_vector<
            std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
            1>& getColumn(int i) const
        {
            assert(i < 3);
            return columns[i];
        }
    private:
        std::array<boost::container::small_vector<
            std::pair<Eigen::Vector3d/*gradient*/,
            std::vector<Eigen::Vector3d>/*epsilons*/>,
            1>, 3> columns;

    };

    //Adds a transformation constructor.
    class GradientsWithEpsilons : public Oracle::GradientsWithEpsilons
    {
    private:
        GradientsWithEpsilons(const JacobianMatrixWithEpsilons& transform,
            const Oracle::GradientsWithEpsilons& base)
            :Oracle::GradientsWithEpsilons(
                getTransformationConstructorData(transform, base)) { ; }
        static boost::container::small_vector<
            std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
            1> getTransformationConstructorData(
                const JacobianMatrixWithEpsilons& transform,
                const Oracle::GradientsWithEpsilons& base);
        friend Oracle::GradientsWithEpsilons 
            JacobianMatrixWithEpsilons::operator*(
                const Oracle::GradientsWithEpsilons& base) const;
    };

    //Now for members of TransformedOracle itself.
    TransformedOracle(const std::shared_ptr<const Oracle> underlying, 
        Tree X_, Tree Y_, Tree Z_) :
        underlying(underlying), X_(X_), Y_(Y_), Z_(Z_),
        xEvaluators(evaluatorsFromTree(X_)),
        yEvaluators(evaluatorsFromTree(Y_)),
        zEvaluators(evaluatorsFromTree(Z_))
    { ; }

    //For deduplication purposes:
    typedef std::pair<std::array<Tree::Id, 3>, const Oracle*> Key;
    static std::map<Key, std::weak_ptr<TransformedOracle>> transformedOracles;

    //Helper function for the constructor.
    static std::array<XTreeEvaluator, 8> evaluatorsFromTree(Tree t)
    {
        return { XTreeEvaluator{t, 0}, {t, 1}, {t, 2}, {t, 3},
                { t, 4 },{t, 5}, {t, 6}, {t, 7} };
        
    }

    const std::shared_ptr<const Oracle> underlying;
    Tree X_;
    Tree Y_;
    Tree Z_;
    mutable std::array<XTreeEvaluator, 8> xEvaluators; 
    mutable std::array<XTreeEvaluator, 8> yEvaluators;
    mutable std::array<XTreeEvaluator, 8> zEvaluators;
    // XTreeEvaluators are used because they contain both interval
    // evaluators and feature evaluators.

};

inline Oracle::GradientsWithEpsilons
TransformedOracle::JacobianMatrixWithEpsilons::operator*(
    const Oracle::GradientsWithEpsilons& base) const
{
    return GradientsWithEpsilons(*this, base);
}

}
