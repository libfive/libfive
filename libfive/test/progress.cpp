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

    float progress = 0.0f;
    float max_progress = 0.0f;
    int cb_count = 0;
    auto callback = [&](float f) {
        progress = f;
        max_progress = fmax(progress, max_progress);
        cb_count++;
        return true;
    };

    XTreePool<3>::build(sponge, r, 0.02, 1e-8, 8, callback);

    CAPTURE(cb_count);
    CAPTURE(max_progress);
    CAPTURE(progress);

    if (cb_count > 2)
    {
        REQUIRE(progress == Approx(1.0f));
        REQUIRE(max_progress == Approx(1.0f));
    }
    else
    {
        WARN("Callbacks not triggered (this is expected in debug builds)");
    }
}
