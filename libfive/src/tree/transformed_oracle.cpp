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

#include "libfive\tree\transformed_oracle.hpp"

namespace Kernel {

TransformedOracle::JacobianMatrixWithEpsilons::JacobianMatrixWithEpsilons(
    std::array<FeatureEvaluator*, 3> evaluators, Eigen::Vector3f point) 
{
    for (auto i = 0; i < 3; ++i) 
    {
        auto features = evaluators[i]->featuresAt(point);
        for (auto feature : features)
        {
            auto epsilonsList = feature.getEpsilons();
            std::vector<Eigen::Vector3d> epsilonsVector(
                epsilonsList.begin(), epsilonsList.end());
            columns[i].push_back({ feature.deriv, epsilonsVector });
        }
    }
}

boost::container::small_vector<
    std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
    1> TransformedOracle::GradientsWithEpsilons::
    getTransformationConstructorData(
    const JacobianMatrixWithEpsilons& transform,
    const Oracle::GradientsWithEpsilons& base) 
{
    boost::container::small_vector<
        std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
        1> out;

    class RecursivePush { // because c++ doesn't like recursive lambdas, 
                          // and std::function has overhead.
    public:
        //usedColumns[j] is irrelevant for j <= i.
        void operator()(int i, std::array<Eigen::Vector3d, 3> usedColumns,
            const Feature& f)
        {
            if (i < 3)
            {
                for (auto gradientChoice : transform.getColumn(i))
                {
                    auto f_ = f;
                    auto valid = true;
                    for (auto epsilon : gradientChoice.second)
                    {
                        if (!f_.push(epsilon, { 0, 0 }))
                        {
                            valid = false;
                            break;
                        }
                    }
                    if (valid)
                    {
                        usedColumns[i] = gradientChoice.first;
                        (*this)(i + 1, usedColumns, f_);
                    }
                }
            }
            else
            {
                Eigen::Matrix3d singleJacobian;
                singleJacobian <<
                    usedColumns[0], usedColumns[1], usedColumns[2];
                for (auto gradientChoice : base.getData())
                {
                    auto f_ = f;
                    auto valid = true;
                    for (auto epsilon : gradientChoice.second)
                    {
                        auto transformedEpsilon = singleJacobian * epsilon;
                        // Now we push it, provided it is not 0; if it's the 0
                        // vector, that just means that it represents the
                        // boundary between two regions whose gradients are 
                        // equal after transformation; they may overlap or not,
                        // and may be combinable or not, but regardless they 
                        // can be left as-is.
                        if ((transformedEpsilon.x() != 0. ||
                             transformedEpsilon.y() != 0. ||
                             transformedEpsilon.y() != 0.) &&
                            !f_.push(transformedEpsilon, { 0, 0 }))
                        {
                            valid = false;
                            break;
                        }
                    }
                    if (valid)
                    {
                        auto epsilonsList = f_.getEpsilons();
                        std::vector<Eigen::Vector3d> epsilonsVector(
                            epsilonsList.begin(), epsilonsList.end());
                        out.push_back({ singleJacobian * gradientChoice.first,
                            epsilonsVector });
                    }
                }
            }
        };

        RecursivePush(const JacobianMatrixWithEpsilons& transform,
            const Oracle::GradientsWithEpsilons& base,
            boost::container::small_vector<
            std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
            1>& out) : transform(transform), base(base), out(out) { ; }
    private:
        const JacobianMatrixWithEpsilons& transform;
        const Oracle::GradientsWithEpsilons& base;
        boost::container::small_vector<
            std::pair<Eigen::Vector3d, std::vector<Eigen::Vector3d>>,
            1>& out;
    };

    RecursivePush(transform, base, out)(
        0, std::array<Eigen::Vector3d, 3>(), Feature());

    return out;
}

std::shared_ptr<TransformedOracle> TransformedOracle::transform(
    const std::shared_ptr<const Oracle> underlying, Tree X_, Tree Y_, Tree Z_)
{
    auto underlyingAsTransformed = 
        dynamic_cast<const TransformedOracle*>(underlying.get());
    if (underlyingAsTransformed != nullptr) //We can simplify.
    {
        return transform(underlyingAsTransformed->underlying,
            X_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_), 
            Y_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_), 
            Z_.remap(underlyingAsTransformed->X_, underlyingAsTransformed->Y_,
                underlyingAsTransformed->Z_));
    }
    else {
        Key k{ {X_.id(), Y_.id(), Z_.id()}, underlying.get() };
        auto found = transformedOracles.find(k);
        if (found == transformedOracles.end())
        {
            auto transformed = new TransformedOracle (underlying, X_, Y_, Z_);
            auto out = std::unique_ptr<TransformedOracle>(transformed);
            return out;
        }
        else
        {
            assert(!found->second.expired());
            return found->second.lock();
        }
    }   
}

std::map<TransformedOracle::Key, std::weak_ptr<TransformedOracle>> 
    TransformedOracle::transformedOracles;

TransformedOracle::~TransformedOracle()
{
    auto success = transformedOracles.erase(
        { { X_.id(), Y_.id(), Z_.id() }, underlying.get() });
    assert(success); //It was in the map before we erased it.
}

Interval::I TransformedOracle::getRange(Region<2> region, int threadNo) const
{
    auto xRange = xEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    auto yRange = yEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    auto zRange = zEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    return underlying->getRange(
        Region<3>({ xRange.lower(), yRange.lower(), zRange.lower() }, 
            { xRange.upper(), yRange.upper(), zRange.upper() }), threadNo);
}

Interval::I TransformedOracle::getRange(Region<3> region, int threadNo) const 
{
    auto xRange = xEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    auto yRange = yEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    auto zRange = zEvaluators[threadNo].interval.eval(
        region.lower3().template cast<float>(), 
        region.upper3().template cast<float>());
    return underlying->getRange(Region<3>(
        { xRange.lower(), yRange.lower(), zRange.lower() }, 
        { xRange.upper(), yRange.upper(), zRange.upper() }), threadNo);
}

Oracle::GradientsWithEpsilons 
TransformedOracle::getGradients(Eigen::Vector3f point, int threadNo) const 
{
    JacobianMatrixWithEpsilons jac(
        { &xEvaluators[threadNo].feature, 
          &yEvaluators[threadNo].feature, 
          &zEvaluators[threadNo].feature },
        point);
    Eigen::Vector3f transformedPoint{ 
        xEvaluators[threadNo].feature.eval(point), 
        yEvaluators[threadNo].feature.eval(point),
        zEvaluators[threadNo].feature.eval(point) };
    return jac * underlying->getGradients(transformedPoint, threadNo);
}

float TransformedOracle::getValue(Eigen::Vector3f point, int threadNo) const 
{
    Eigen::Vector3f transformedPoint{ 
        xEvaluators[threadNo].feature.eval(point), 
        yEvaluators[threadNo].feature.eval(point),
        zEvaluators[threadNo].feature.eval(point) };
    return underlying->getValue(transformedPoint, threadNo);
}

bool TransformedOracle::isAmbiguous(Eigen::Vector3f point, int threadNo) const
{
    Eigen::Vector3f transformedPoint{
        xEvaluators[threadNo].feature.eval(point),
        yEvaluators[threadNo].feature.eval(point),
        zEvaluators[threadNo].feature.eval(point) };
    xEvaluators[threadNo].array.ArrayEvaluator::set(point, 0);
    yEvaluators[threadNo].array.ArrayEvaluator::set(point, 0);
    zEvaluators[threadNo].array.ArrayEvaluator::set(point, 0);
    return xEvaluators[threadNo].array.ArrayEvaluator::getAmbiguous(1)(0) ||
           yEvaluators[threadNo].array.ArrayEvaluator::getAmbiguous(1)(0) ||
           zEvaluators[threadNo].array.ArrayEvaluator::getAmbiguous(1)(0) ||
           underlying->isAmbiguous(transformedPoint, threadNo);
}

} //namespace Kernel