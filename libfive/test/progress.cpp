/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/eval/evaluator.hpp"

#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/dc/dc_pool.hpp"
#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/settings.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

class TestProgressHandler : public ProgressHandler {
public:
    void progress(double d) override {
        ps.push_back(d);
    }
    std::vector<float> ps;
};

TEST_CASE("DCPool::build (progress callback)")
{
    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    for (auto res: {0.02, 0.03, 0.05, 0.1, 0.11, 0.125})
    {
        BRepSettings settings;
        settings.min_feature = res;
        TestProgressHandler handler;
        handler.start({1});
        settings.progress_handler = &handler;

        DCPool<3>::build(sponge, r, settings);
        handler.finish();

        CAPTURE(handler.ps.size());
        CAPTURE(handler.ps);
        CAPTURE(res);

        REQUIRE(handler.ps.size() >= 2);
        REQUIRE(handler.ps[0] == 0.0f);

        // Check that the values are monotonically increasing
        float prev = -1;
        for (auto& p : handler.ps)
        {
            REQUIRE(p > prev);
            prev = p;
        }

        if (handler.ps.size() > 2)
        {
            REQUIRE(handler.ps[handler.ps.size() - 2] > 0.0f);
            REQUIRE(handler.ps[handler.ps.size() - 2] < 1.0f);
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

    BRepSettings settings;

    for (auto res: {0.02, 0.03, 0.05, 0.1, 0.11, 0.125})
    {
        TestProgressHandler progress;
        settings.progress_handler = &progress;

        settings.min_feature = res;
        Mesh::render(sponge, r, settings);

        CAPTURE(progress.ps.size());
        CAPTURE(progress.ps);
        CAPTURE(res);

        REQUIRE(progress.ps.size() >= 2);
        REQUIRE(progress.ps[0] == 0.0f);
        REQUIRE(progress.ps[progress.ps.size() - 1] > 0.6666f);

        // Check that the values are monotonically increasing
        float prev = -1;
        for (auto& p : progress.ps)
        {
            REQUIRE(p > prev);
            prev = p;
        }
    }
}
