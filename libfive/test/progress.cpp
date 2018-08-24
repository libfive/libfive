/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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
#include "catch.hpp"

#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/xtree_pool.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("XTreePool::build (progress callback)")
{
    Tree sponge = max(menger(2), -sphere(1, {1.5, 1.5, 1.5}));
    Region<3> r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5});

    for (auto res: {0.02, 0.03, 0.05, 0.1, 0.11, 0.125})
    {
        std::vector<float> ps;
        auto callback = [&](float f) {
            ps.push_back(f);
            return true;
        };

        XTreePool<3>::build(sponge, r, res, 1e-8, 8, callback);

        CAPTURE(ps.size());

        REQUIRE(ps.size() >= 2);
        REQUIRE(ps[0] == 0.0f);
        REQUIRE(ps[ps.size() - 1] == 1.0f);
        CAPTURE(ps);
        CAPTURE(res);

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

}
