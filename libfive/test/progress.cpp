/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/dc/dc_pool.hpp"
#include "libfive/render/brep/mesh.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("DCPool::build (progress callback)")
{
    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    for (auto res: {0.02, 0.03, 0.05, 0.1, 0.11, 0.125})
    {
        std::vector<float> ps;
        auto callback = [&](float f) {
            ps.push_back(f);
        };

        DCPool<3>::build(sponge, r, res, 1e-8, 8, callback);

        CAPTURE(ps.size());
        CAPTURE(ps);
        CAPTURE(res);

        REQUIRE(ps.size() >= 2);
        REQUIRE(ps[0] == 0.0f);
        REQUIRE(ps[ps.size() - 1] == 1.0f);

        // Check that the values are monotonically increasing
        float prev = -1;
        for (auto& p : ps)
        {
            REQUIRE(p > prev);
            prev = p;
        }

        if (ps.size() > 2)
        {
            REQUIRE(ps[ps.size() - 2] >= 0.5f);
        }
        else
        {
            WARN("Callbacks not triggered (this is expected in debug builds)");
        }
    }
}

TEST_CASE("Mesh::render (progress callback)")
{
    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    for (auto res: {0.02, 0.03, 0.05, 0.1, 0.11, 0.125})
    {
        std::vector<float> ps;
        auto callback = [&](float f) {
            ps.push_back(f);
        };

        Mesh::render(sponge, r, res, 1e-8, true, callback);

        CAPTURE(ps.size());
        CAPTURE(ps);
        CAPTURE(res);

        REQUIRE(ps.size() >= 2);
        REQUIRE(ps[0] == 0.0f);
        REQUIRE(ps[ps.size() - 1] == 3.0f);

        // Check that the values are monotonically increasing
        float prev = -1;
        for (auto& p : ps)
        {
            REQUIRE(p > prev);
            prev = p;
        }

        if (ps.size() <= 4)
        {
            WARN("Callbacks not triggered (this is expected in debug builds)");
        }
    }
}
