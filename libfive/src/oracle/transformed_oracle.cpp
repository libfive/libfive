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
    Eigen::Vector3f transformedPoint{
        xEvaluator.feature.eval(points.col(index)),
        yEvaluator.feature.eval(points.col(index)),
        zEvaluator.feature.eval(points.col(index)) };

    underlying->set(transformedPoint, index);
    underlying->evalPoint(out, index);
}

void TransformedOracle::evalArray(
    Eigen::Block<Eigen::Array<float, Eigen::Dynamic,
                 LIBFIVE_EVAL_ARRAY_SIZE, Eigen::RowMajor>, 1, Eigen::Dynamic> out)
{
    setUnderlyingArrayValues(out.cols());
    underlying->evalArray(out);
}

void TransformedOracle::checkAmbiguous(
    Eigen::Block<Eigen::Array<bool, 1, LIBFIVE_EVAL_ARRAY_SIZE>,
                 1, Eigen::Dynamic> out)
{
    setUnderlyingArrayValues(out.cols());
    underlying->checkAmbiguous(out);
    out = out || xEvaluator.array.getAmbiguous(out.cols())
              || yEvaluator.array.getAmbiguous(out.cols())
              || zEvaluator.array.getAmbiguous(out.cols());
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

    TransformedOracle::setUnderlyingArrayValues(out.cols());

    auto xDerivs = ctx ? xEvaluator.array.derivs(out.cols(), ctx->tx)
                           : xEvaluator.array.derivs(out.cols());
    auto yDerivs = ctx ? yEvaluator.array.derivs(out.cols(), ctx->ty)
                           : yEvaluator.array.derivs(out.cols());
    auto zDerivs = ctx ? zEvaluator.array.derivs(out.cols(), ctx->tz)
                           : zEvaluator.array.derivs(out.cols());

    underlying->bind(ctx ? ctx->u : nullptr);
    underlying->evalDerivArray(out);
    underlying->unbind();

    for (auto i = 0; i < out.cols(); ++i)
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
    Eigen::Vector3f transformedPoint = (context == nullptr)
        ? Eigen::Vector3f(xEvaluator.feature.eval(pt),
                          yEvaluator.feature.eval(pt),
                          zEvaluator.feature.eval(pt))
        : Eigen::Vector3f(xEvaluator.feature.eval(pt, ctx->tx),
                          yEvaluator.feature.eval(pt, ctx->ty),
                          zEvaluator.feature.eval(pt, ctx->tz));

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


void TransformedOracle::setUnderlyingArrayValues(int count)
{
    auto xPoints = xEvaluator.array.values(count);
    auto yPoints = yEvaluator.array.values(count);
    auto zPoints = zEvaluator.array.values(count);
    for (auto i = 0; i < count; ++i)
    {
        underlying->set({ xPoints(i), yPoints(i), zPoints(i) }, i);
    }
}

bool TransformedOracle::Context::isTerminal()
{
    return tx->isTerminal() &&
           ty->isTerminal() &&
           tz->isTerminal() &&
           u->isTerminal();
}

} //Namespace Kernel

