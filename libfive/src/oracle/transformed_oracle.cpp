/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/oracle/transformed_oracle.hpp"

namespace libfive {

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
    xEvaluator.set(p, index);
    yEvaluator.set(p, index);
    zEvaluator.set(p, index);
}

void TransformedOracle::evalInterval(Interval& out)
{
    auto xRange = xEvaluator.eval(lower, upper);
    auto yRange = yEvaluator.eval(lower, upper);
    auto zRange = zEvaluator.eval(lower, upper);

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
        ? Eigen::Vector3f(xEvaluator.value(points.col(index), *ctx->tx),
                          yEvaluator.value(points.col(index), *ctx->ty),
                          zEvaluator.value(points.col(index), *ctx->tz))
        : Eigen::Vector3f(xEvaluator.value(points.col(index)),
                          yEvaluator.value(points.col(index)),
                          zEvaluator.value(points.col(index)));

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
    const unsigned count = out.cols();

    auto xPoints = ctx ? xEvaluator.values(count, *ctx->tx)
                       : xEvaluator.values(count);
    auto yPoints = ctx ? yEvaluator.values(count, *ctx->ty)
                       : yEvaluator.values(count);
    auto zPoints = ctx ? zEvaluator.values(count, *ctx->tz)
                       : zEvaluator.values(count);

    for (unsigned i = 0; i < count; ++i)
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
    const unsigned count = out.cols();
    underlying->checkAmbiguous(out);
    out = out || xEvaluator.getAmbiguous(count)
              || yEvaluator.getAmbiguous(count)
              || zEvaluator.getAmbiguous(count);
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
            ? xEvaluator.deriv(points.col(index), *ctx->tx)
            : xEvaluator.deriv(points.col(index))).template head<3>(),
        (ctx
            ? yEvaluator.deriv(points.col(index), *ctx->ty)
            : yEvaluator.deriv(points.col(index))).template head<3>(),
        (ctx
            ? zEvaluator.deriv(points.col(index), *ctx->tz)
            : zEvaluator.deriv(points.col(index))).template head<3>();

    Eigen::Vector3f transformedPoint{
        xEvaluator.value(points.col(index)),
        yEvaluator.value(points.col(index)),
        zEvaluator.value(points.col(index))};

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
    const unsigned count = out.cols();
    assert(context == nullptr || ctx != nullptr);

    auto xDerivs = ctx ? xEvaluator.derivs(count, *ctx->tx)
                       : xEvaluator.derivs(count);
    auto yDerivs = ctx ? yEvaluator.derivs(count, *ctx->ty)
                       : yEvaluator.derivs(count);
    auto zDerivs = ctx ? zEvaluator.derivs(count, *ctx->tz)
                       : zEvaluator.derivs(count);

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalDerivArray(out);
    underlying->unbind();

    for (unsigned i = 0; i < count; ++i)
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
        ? Eigen::Vector3f(xEvaluator.value(pt, *ctx->tx),
                          yEvaluator.value(pt, *ctx->ty),
                          zEvaluator.value(pt, *ctx->tz))
        : Eigen::Vector3f(xEvaluator.value(pt),
                          yEvaluator.value(pt),
                          zEvaluator.value(pt));

    auto xFeatures = xEvaluator.features_(pt);
    auto yFeatures = yEvaluator.features_(pt);
    auto zFeatures = zEvaluator.features_(pt);

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

    out->tx = ctx ? xEvaluator.push(ctx->tx)
                  : xEvaluator.push();
    out->ty = ctx ? yEvaluator.push(ctx->ty)
                  : yEvaluator.push();
    out->tz = ctx ? zEvaluator.push(ctx->tz)
                  : zEvaluator.push();

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
           (!u || u->isTerminal());
}

} //Namespace Kernel

