/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/eval/eval_jacobian.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/tape.hpp"

namespace libfive {

JacobianEvaluator::JacobianEvaluator(const Tree& root)
    : JacobianEvaluator(std::make_shared<Deck>(root))
{
    // Nothing to do here
}

JacobianEvaluator::JacobianEvaluator(
        const Tree& root, const std::map<Tree::Id, float>& vars)
    : JacobianEvaluator(std::make_shared<Deck>(root), vars)
{
    // Nothing to do here
}

JacobianEvaluator::JacobianEvaluator(std::shared_ptr<Deck> d)
    : JacobianEvaluator(d, std::map<Tree::Id, float>())
{
    // Nothing to do here
}

JacobianEvaluator::JacobianEvaluator(
        std::shared_ptr<Deck> d, const std::map<Tree::Id, float>& vars)
    : BaseEvaluator(d, vars), FeatureEvaluator(d, vars),
      j(deck->vars.size())
{
    // Nothing to do here
}

std::map<Tree::Id, float> JacobianEvaluator::gradient(
        const Eigen::Vector3f& p)
{
    return gradient(p, *deck->tape);
}

std::map<Tree::Id, float> JacobianEvaluator::gradient(
        const Eigen::Vector3f& p, const Tape& tape)
{
    // Load the XYZ values into the whole array, rather than
    // checking and loading a subset of the array.
    v.row(deck->X) = p.x();
    v.row(deck->Y) = p.y();
    v.row(deck->Z) = p.z();

    // Turn on a flag which modifies the DerivArrayEvaluator
    // behavior for the CONST_VAR opcode.
    clear_vars = true;

    unsigned count = 0;

    // remap[i] tells us which variable is at ds(i % 3, i / 3)
    std::array<unsigned, 3 * N> remap;

    auto run = [&]() {
        if (count) {
            auto ds = derivs((count + 2) / 3, tape);
            for (unsigned i=0; i < count; ++i) {
                j[remap[i]] = ds(i % 3, i / 3);
            }
            count = 0;
        }
        // Clear the derivative arrays
        for (long i=0; i < d.rows(); ++i) {
            d(i) = 0;
        }
    };

    // Dummy call to reset the derivative array
    run();

    // We're going to use the deriv array evaluator
    // to evaluate the Jacobian instead.
    unsigned index = 0;
    for (auto& v : deck->vars.left) {
        d(v.first)(count % 3, count / 3) = 1.0;
        remap[count++] = index++;
        if (count == N * 3) {
            run();
        }
    }

    // Run once more to flush the evaluation and clear the deriv array
    run();

    // Reload immutable derivatives for X, Y, Z
    d(deck->X).row(0) = 1;
    d(deck->Y).row(1) = 1;
    d(deck->Z).row(2) = 1;

    // Unpack from flat array into map
    // (to allow correlating back to VARs in Tree)
    std::map<Tree::Id, float> out;
    index = 0;
    for (auto v : deck->vars.left)
    {
        out[v.second] = j(index++);
    }

    // Reset our special-behavior-on-CONST_VAR flag to the default.
    clear_vars = false;
    return out;
}

}   // namespace libfive
