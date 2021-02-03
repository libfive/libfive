/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/oracle/borrowing_transformed_oracle.hpp"
#include "libfive/eval/evaluator.hpp"

namespace libfive {

BorrowingTransformedOracle::BorrowingTransformedOracle(
    std::unique_ptr<Oracle> underlying,
    Clause::Id X_, Clause::Id Y_, Clause::Id Z_)
        : underlying(std::move(underlying)), X(X_), Y(Y_), Z(Z_)
{
    //nothing more to do here.
}

void BorrowingTransformedOracle::evalInterval(Interval& out)
{
  auto ctx = dynamic_cast<Context*>(context.get());
  assert(context == nullptr || ctx != nullptr);
  auto intervalEvaluator = dynamic_cast<const IntervalEvaluator*>(evaluator);
  if (!intervalEvaluator) {
    assert(false);
    out = {-INFINITY, INFINITY, true};
    return;
  }
  auto [x, y, z] = getRemappedXYZ();
  auto xRange    = intervalEvaluator->clause(x);
  auto yRange    = intervalEvaluator->clause(y);
  auto zRange    = intervalEvaluator->clause(z);

  Eigen::Vector3f rangeLower {xRange.lower(), yRange.lower(), zRange.lower()};
  Eigen::Vector3f rangeUpper {xRange.upper(), yRange.upper(), zRange.upper()};

  underlying->bind(ctx ? ctx->u : nullptr, nullptr);
  underlying->set(rangeLower, rangeUpper);
  underlying->evalInterval(out);
  underlying->unbind();
}

void BorrowingTransformedOracle::evalPoint(float& out, size_t index)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);
    auto arrayEvaluator = dynamic_cast<const ArrayEvaluator*>(evaluator);
    if (!arrayEvaluator) {
        assert(false);
        out = NAN;
        return;
    }
    auto [xr, yr, zr] = getRemappedXYZ();
    auto x = arrayEvaluator->clause(xr, index + 1)(index);
    auto y = arrayEvaluator->clause(yr, index + 1)(index);
    auto z = arrayEvaluator->clause(zr, index + 1)(index);

    Eigen::Vector3f transformedPoint(x, y, z);

    underlying->set(transformedPoint, index);

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    underlying->evalPoint(out, index);
    underlying->unbind();
}

void BorrowingTransformedOracle::evalArray(
    Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                 LIBFIVE_EVAL_ARRAY_SIZE, Eigen::RowMajor>, 1, Eigen::Dynamic> out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);
    const unsigned count = out.cols();

    auto arrayEvaluator = dynamic_cast<const ArrayEvaluator*>(evaluator);
    if (!arrayEvaluator) {
        assert(false);
        out = NAN;
        return;
    }
    auto [x, y, z] = getRemappedXYZ();
    auto xPoints = arrayEvaluator->clause(x, count);
    auto yPoints = arrayEvaluator->clause(y, count);
    auto zPoints = arrayEvaluator->clause(z, count);

    for (unsigned i = 0; i < count; ++i)
    {
        underlying->set({ xPoints(i), yPoints(i), zPoints(i) }, i);
    }

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    underlying->evalArray(out);
    underlying->unbind();
}

void BorrowingTransformedOracle::checkAmbiguous(
    Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                 1, Eigen::Dynamic> out)
{
    const unsigned count = out.cols();
    underlying->checkAmbiguous(out);
}

void BorrowingTransformedOracle::evalDerivs(
    Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                 3, 1, true> out, size_t index)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    auto derivEvaluator = dynamic_cast<const DerivArrayEvaluator*>(evaluator);
    if (!derivEvaluator) {
        assert(false);
        out = NAN; 
        return;
    }
    auto [x, y, z] = getRemappedXYZ();
    auto xGradient = derivEvaluator->clauseDerivs(x).col(index);
    auto yGradient = derivEvaluator->clauseDerivs(y).col(index);
    auto zGradient = derivEvaluator->clauseDerivs(z).col(index);

    auto xPoint = derivEvaluator->clause(x, index + 1)(index);
    auto yPoint = derivEvaluator->clause(y, index + 1)(index);
    auto zPoint = derivEvaluator->clause(z, index + 1)(index);

    Eigen::Matrix3f Jacobian;
    Jacobian << xGradient, yGradient, zGradient;    
    
    underlying->set({ xPoint, yPoint, zPoint }, index);

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    underlying->evalDerivs(out, index);
    underlying->unbind();

    out = Jacobian * out.matrix();
}

void BorrowingTransformedOracle::evalDerivArray(
    Eigen::Block<Eigen::Array<float, 3, LIBFIVE_EVAL_ARRAY_SIZE>,
                 3, Eigen::Dynamic, true> out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);
    const unsigned count = out.cols();

    auto derivEvaluator = dynamic_cast<const DerivArrayEvaluator*>(evaluator);
    if (!derivEvaluator) {
        assert(false);
        out = NAN;
        return;
    }
    auto [x, y, z] = getRemappedXYZ();
    const auto& xGradients = derivEvaluator->clauseDerivs(x);
    const auto& yGradients = derivEvaluator->clauseDerivs(y);
    const auto& zGradients = derivEvaluator->clauseDerivs(z);

    auto xPoints = derivEvaluator->clause(x, count);
    auto yPoints = derivEvaluator->clause(y, count);
    auto zPoints = derivEvaluator->clause(z, count);

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    for (unsigned i = 0; i < count; ++i)
    {
        underlying->set({xPoints(i), yPoints(i), zPoints(i)});
        underlying->evalDerivArray(out);
        Eigen::Matrix3f Jacobian;
        Jacobian << xGradients.col(i), yGradients.col(i), zGradients.col(i);
        out.col(i) = Jacobian * out.col(i).matrix();
    }
    underlying->unbind();
}

void BorrowingTransformedOracle::evalFeatures(
    boost::container::small_vector<Feature, 4>& out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    out.clear();

    auto featureEvaluator = dynamic_cast<const FeatureEvaluator*>(evaluator);
    if (!featureEvaluator) {
      assert(false);
      return;
    }

    auto [xr, yr, zr] = getRemappedXYZ();
    auto x = featureEvaluator->clause(xr, 1)(0);
    auto y = featureEvaluator->clause(yr, 1)(0);
    auto z = featureEvaluator->clause(zr, 1)(0);

    boost::container::small_vector<Feature, 4> underlyingOut;
    underlying->set({ x, y, z });

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    underlying->evalFeatures(underlyingOut);
    underlying->unbind();

    auto xFeatures = featureEvaluator->clauseFeatures(xr);
    auto yFeatures = featureEvaluator->clauseFeatures(yr);
    auto zFeatures = featureEvaluator->clauseFeatures(zr);


    /*  This O(n^4) loop should almost never have large values for all input
     *  sizes, barring intentionally pathological cases.
     */
    for (auto f1 : xFeatures)
    {
        for (auto f2 : yFeatures)
        {
            if (f1.check(f2))
            {
                Feature f12({ 0.f, 0.f, 0.f }, f1, f2);
                for (auto f3 : zFeatures)
                {
                    if (f3.check(f12))
                    {
                        Feature f123({ 0.f, 0.f, 0.f }, f12, f3);
                        Eigen::Matrix3f Jacobian;
                        Jacobian << f1.deriv, f2.deriv, f3.deriv;
                        for (auto f4 : underlyingOut)
                        {
                            Feature transformed(f4, Jacobian);
                            if (transformed.check(f123))
                            {
                                out.emplace_back(
                                    transformed.deriv, transformed, f123);
                            }
                        }
                    }
                }
            }
        }
    }
}

std::shared_ptr<OracleContext> BorrowingTransformedOracle::push(Tape::Type t)
{
    if (t != Tape::INTERVAL)
    {
        return nullptr;
    }

    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    auto out = std::shared_ptr<Context>(new Context);

    underlying->bind(ctx ? ctx->u : nullptr, nullptr);
    out->u = underlying->push(t);
    underlying->unbind();
    auto [x, y, z] = getRemappedXYZ();
    out->remappedX = x;
    out->remappedY = y;
    out->remappedZ = z;
    return out;
}

std::array<Clause::Id, 3> BorrowingTransformedOracle::getRemappedXYZ() const
{
  auto c = dynamic_cast<Context*>(context.get());
  if (!c) {
    assert(!context);
    return { X, Y, Z };
  }
  else {
    return { c->remappedX, c->remappedY, c->remappedZ };
  }
}

void BorrowingTransformedOracle::Context::applyRemaps(
      const std::vector<Clause::Id>& remaps)
{
  for (auto rptr : { &remappedX, &remappedY, &remappedZ }) {
    while (remaps[*rptr]) {
      *rptr = remaps[*rptr];
    }
  }
}

} //Namespace Kernel

