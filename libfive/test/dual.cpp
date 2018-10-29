/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/dc/dc_pool.hpp"
#include "libfive/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    // Copied from contours.cpp (Segment class)
    template <Axis::Axis A>
    void load(const std::array<const DCTree<2>*, 2>& ts)
    {
        // TODO: consolidate this code into once place

        // Exit immediately if we can prove that there will be no
        // face produced by this edge.
        if (std::any_of(ts.begin(), ts.end(),
            [](const DCTree<2>* t){ return t->type != Interval::AMBIGUOUS; }))
        {
            return;
        }

        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const DCTree<2>* a, const DCTree<2>* b)
                { return a->leaf->level < b->leaf->level; }) - ts.begin();

        constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;
        constexpr std::array<uint8_t, 2> corners = {{perp, 0}};

        // If there is a sign change across the relevant edge, then call the
        // watcher with the segment corners (with proper winding order)
        auto a = ts[index]->cornerState(corners[index]);
        auto b = ts[index]->cornerState(corners[index] | A);
        if (a != b)
        {
            // Use either forward or reversed segment building
            if ((a == Interval::FILLED && A == Axis::Y) ||
                (b == Interval::FILLED && A == Axis::X))
            {
                load<A, 0>(ts);
            }
            else
            {
                load<A, 1>(ts);
            }
        }
    }

    // Check winding of contours
    template<Axis::Axis A, bool D>
    void load(const std::array<const DCTree<2>*, 2>& ts)
    {
        auto a = ts[!D];
        auto b = ts[D];

        auto norm = Eigen::Vector2d(a->vert().y() - b->vert().y(),
                                    b->vert().x() - a->vert().x())
            .normalized();
        Eigen::Vector2d center = (a->vert() + b->vert()).normalized();
        auto dot_ = -center.dot(norm);
        neg += (dot_ < 0);
        pos += (dot_ > 0);
        dot = fmin(dot, dot_);

        for (auto t : ts)
        {
            auto n = t->vert().norm();
            min_norm = fmin(n, min_norm);
            max_norm = fmax(n, max_norm);
        }
    }
    int pos = 0;
    int neg = 0;
    float dot = 2;
    float min_norm = 2;
    float max_norm = 0;
};

struct Walker3
{
    template <Axis::Axis A>
    void load(const std::array<const DCTree<3>*, 4>& a)
    {
        // TODO: consolidate this with DCMesher

        // Exit immediately if we can prove that there will be no
        // face produced by this edge.
        if (std::any_of(a.begin(), a.end(),
            [](const DCTree<3>* t){ return t->type != Interval::AMBIGUOUS; }))
        {
            return;
        }

        for (auto t : a)
        {
            auto n = t->vert().norm();
            min_norm = fmin(n, min_norm);
            max_norm = fmax(n, max_norm);
        }
    }
    float min_norm = 2;
    float max_norm = 0;
};

#if 0
// TODO
TEST_CASE("Dual<2>::walk")
{
    auto ta = DCPool<2>::build(circle(0.5), Region<2>({-1, -1}, {1, 1}));

    Walker2 c;
    Dual<2>::walk(ta.get(), c);
    REQUIRE(c.min_norm > 0.49);
    REQUIRE(c.max_norm < 0.51);
    CAPTURE(c.neg);
    CAPTURE(c.pos);
    REQUIRE(c.dot > 0.9);
}

TEST_CASE("Dual<3>::walk")
{
    auto ta = DCPool<3>::build(sphere(0.5), Region<3>({-1, -1, -1}, {1, 1, 1}));

    Walker3 c;
    Dual<3>::walk(ta.get(), c);
    REQUIRE(c.min_norm > 0.49);
    REQUIRE(c.max_norm < 0.51);
}
#endif
