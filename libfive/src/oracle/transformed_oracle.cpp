/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/oracle/transformed_oracle.hpp"

namespace Kernel {

TransformedOracle::TransformedOracle(
        std::unique_ptr<Oracle> underlying, Tree X_, Tree Y_, Tree Z_)
    : underlying(std::move(underlying)),
      xEvaluator(X_), yEvaluator(Y_), zEvaluator(Z_)
{
    //nothing more to do here.
}

void TransformedOracle::set(const Eigen::Vector3f& p, size_t index)
{
    OracleStorage::set(p, index);
    xEvaluator.array.set(p, index);
    yEvaluator.array.set(p, index);
    zEvaluator.array.set(p, index);
}

void TransformedOracle::setCount(Eigen::Index count)
{
    OracleStorage::setCount(count);
    underlying->setCount(count);
}

void TransformedOracle::evalInterval(Interval::I& out)
{
    auto xRange = xEvaluator.interval.eval(lower, upper);
    auto yRange = yEvaluator.interval.eval(lower, upper);
    auto zRange = zEvaluator.interval.eval(lower, upper);

    Eigen::Vector3f rangeLower{
        xRange.lower(), yRange.lower(), zRange.lower() };
    Eigen::Vector3f rangeUpper{
        xRange.upper(), yRange.upper(), zRange.upper() };

    underlying->set(rangeLower, rangeUpper);
    underlying->evalInterval(out);
}

void TransformedOracle::evalPoint(float& out, size_t index)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    Eigen::Vector3f transformedPoint = ctx
        ? Eigen::Vector3f(xEvaluator.feature.eval(points.col(index), ctx->tx),
                          yEvaluator.feature.eval(points.col(index), ctx->ty),
                          zEvaluator.feature.eval(points.col(index), ctx->tz))
        : Eigen::Vector3f(xEvaluator.feature.eval(points.col(index)),
                          yEvaluator.feature.eval(points.col(index)),
                          zEvaluator.feature.eval(points.col(index)));

    underlying->set(transformedPoint, index);

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalPoint(out, index);
    underlying->unbind();
}

void TransformedOracle::evalArray(
    Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                 LIBFIVE_EVAL_ARRAY_SIZE, Eigen::RowMajor>, 1, Eigen::Dynamic> out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    auto xPoints = ctx ? xEvaluator.array.values(count(), ctx->tx)
                       : xEvaluator.array.values(count());
    auto yPoints = ctx ? yEvaluator.array.values(count(), ctx->ty)
                       : yEvaluator.array.values(count());
    auto zPoints = ctx ? zEvaluator.array.values(count(), ctx->tz)
                       : zEvaluator.array.values(count());

    for (auto i = 0; i < count(); ++i)
    {
        underlying->set({ xPoints(i), yPoints(i), zPoints(i) }, i);
    }

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalArray(out);
    underlying->unbind();
}

void TransformedOracle::checkAmbiguous(
    Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                 1, Eigen::Dynamic> out)
{
    underlying->checkAmbiguous(out);
    out = out || xEvaluator.array.getAmbiguous(count())
              || yEvaluator.array.getAmbiguous(count())
              || zEvaluator.array.getAmbiguous(count());
}

void TransformedOracle::evalDerivs(
    Eigen::Block<Eigen::Array<float, 3, Eigen::Dynamic>,
                 3, 1, true> out, size_t index)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    Eigen::Matrix3f Jacobian;
    Jacobian <<
        (ctx
            ? xEvaluator.deriv.deriv(points.col(index), ctx->tx)
            : xEvaluator.deriv.deriv(points.col(index))).template head<3>(),
        (ctx
            ? yEvaluator.deriv.deriv(points.col(index), ctx->ty)
            : yEvaluator.deriv.deriv(points.col(index))).template head<3>(),
        (ctx
            ? zEvaluator.deriv.deriv(points.col(index), ctx->tz)
            : zEvaluator.deriv.deriv(points.col(index))).template head<3>();

    Eigen::Vector3f transformedPoint{
        xEvaluator.deriv.eval(points.col(index)),
        yEvaluator.deriv.eval(points.col(index)),
        zEvaluator.deriv.eval(points.col(index))};

    underlying->set(transformedPoint, index);

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalDerivs(out, index);
    underlying->unbind();

    out = Jacobian * out.matrix();
}

void TransformedOracle::evalDerivArray(
    Eigen::Block<Eigen::Array<float, 3, LIBFIVE_EVAL_ARRAY_SIZE>,
                 3, Eigen::Dynamic, true> out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    auto xDerivs = ctx ? xEvaluator.array.derivs(count(), ctx->tx)
                       : xEvaluator.array.derivs(count());
    auto yDerivs = ctx ? yEvaluator.array.derivs(count(), ctx->ty)
                       : yEvaluator.array.derivs(count());
    auto zDerivs = ctx ? zEvaluator.array.derivs(count(), ctx->tz)
                       : zEvaluator.array.derivs(count());

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalDerivArray(out);
    underlying->unbind();

    for (auto i = 0; i < count(); ++i)
    {
        Eigen::Matrix3f Jacobian;
        Jacobian << xDerivs.col(i).template head<3>(),
                    yDerivs.col(i).template head<3>(),
                    zDerivs.col(i).template head<3>();
        out.col(i) = Jacobian * out.col(i).matrix();
    }
}

void TransformedOracle::evalFeatures(
    boost::container::small_vector<Feature, 4>& out)
{
    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    out.clear();
    auto pt = points.col(0);
    Eigen::Vector3f transformedPoint = ctx
        ? Eigen::Vector3f(xEvaluator.feature.eval(pt, ctx->tx),
                          yEvaluator.feature.eval(pt, ctx->ty),
                          zEvaluator.feature.eval(pt, ctx->tz))
        : Eigen::Vector3f(xEvaluator.feature.eval(pt),
                          yEvaluator.feature.eval(pt),
                          zEvaluator.feature.eval(pt));

    auto xFeatures = xEvaluator.feature.features_(pt);
    auto yFeatures = yEvaluator.feature.features_(pt);
    auto zFeatures = zEvaluator.feature.features_(pt);

    boost::container::small_vector<Feature, 4> underlyingOut;
    underlying->set(transformedPoint);

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalFeatures(underlyingOut);
    underlying->unbind();

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

std::shared_ptr<OracleContext> TransformedOracle::push(Tape::Type t)
{
    if (t != Tape::INTERVAL)
    {
        return nullptr;
    }

    auto ctx = dynamic_cast<Context*>(context.get());
    assert(context == nullptr || ctx != nullptr);

    auto out = std::shared_ptr<Context>(new Context);

    out->tx = ctx ? xEvaluator.interval.push(ctx->tx)
                  : xEvaluator.interval.push();
    out->ty = ctx ? yEvaluator.interval.push(ctx->ty)
                  : yEvaluator.interval.push();
    out->tz = ctx ? zEvaluator.interval.push(ctx->tz)
                  : zEvaluator.interval.push();

    underlying->bind(ctx ? ctx->u : nullptr);
    out->u = underlying->push(t);
    underlying->unbind();

    return out;
}


bool TransformedOracle::Context::isTerminal()
{
    return tx->isTerminal() &&
           ty->isTerminal() &&
           tz->isTerminal() &&
           u->isTerminal();
}

} //Namespace Kernel

