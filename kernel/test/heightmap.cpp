/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <catch/catch.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/kernel/render/heightmap.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

#define EPSILON 1e-6

// Helper function to make rendering a single call
static std::pair<DepthImage, NormalImage> Render(
        Tree* t, const Region& r, glm::mat4 M=glm::mat4())
{
    std::atomic_bool abort(false);

    auto out = Heightmap::Render(t, r, abort, M);
    return std::make_pair(out.first.transpose(),
                          out.second.transpose());
}

TEST_CASE("2D interval Z values")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 0);

    auto out = Render(&t, r).first;
    CAPTURE(out);
    REQUIRE((out == 0 ||
             out == -std::numeric_limits<float>::infinity()).all());
}

TEST_CASE("3D interval Z values")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 25);

    auto out = Render(&t, r).first;
    CAPTURE(out);
    REQUIRE((out == r.Z.values.back() ||
             out == -std::numeric_limits<float>::infinity()).all());
}

TEST_CASE("2D rendering of a circle ")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    DepthImage comp(10, 10);
    float inf = std::numeric_limits<float>::infinity();
    comp <<
        -inf,-inf,-inf,   0,   0,   0,   0,-inf,-inf,-inf,
        -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
        -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
           0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
           0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
           0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
           0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
        -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
        -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
        -inf,-inf,-inf,   0,   0,   0,   0,-inf,-inf,-inf;

    SECTION("Empty Z")
    {
        Region r({-1, 1}, {-1, 1}, {0, 0}, 5);
        auto out = Render(&t, r).first;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }

    SECTION("Zero-resolution Z")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 5, 5, 0);
        auto out = Render(&t, r).first;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }
}

TEST_CASE("2D circle rendering at non-zero Z ")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    Region r({-1, 1}, {-1, 1}, {1, 1}, 5);
    auto out = Render(&t, r).first;
    CAPTURE(out);

    DepthImage comp(10, 10);
    float inf = std::numeric_limits<float>::infinity();
    comp <<
        -inf,-inf,-inf,   1,   1,   1,   1,-inf,-inf,-inf,
        -inf,   1,   1,   1,   1,   1,   1,   1,   1,-inf,
        -inf,   1,   1,   1,   1,   1,   1,   1,   1,-inf,
           1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
           1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
           1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
           1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
        -inf,   1,   1,   1,   1,   1,   1,   1,   1,-inf,
        -inf,   1,   1,   1,   1,   1,   1,   1,   1,-inf,
        -inf,-inf,-inf,   1,   1,   1,   1,-inf,-inf,-inf;
    REQUIRE((comp == out).all());
}

TEST_CASE("Render orientation ")
{
    Store s;
    Region r({-1, 1}, {-1, 1}, {0, 0}, 5);

    SECTION("Y")
    {
        Tree t(&s, s.operation(OP_MAX,
                   s.operation(OP_SUB,
                   s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                       s.operation(OP_MUL, s.Y(), s.Y())),
                   s.constant(1)), s.Y()));

        auto out = Render(&t, r).first;

        DepthImage comp(10, 10);
        float inf = std::numeric_limits<float>::infinity();
        comp <<
            -inf,-inf,-inf,   0,   0,   0,   0,-inf,-inf,-inf,
            -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
            -inf,   0,   0,   0,   0,   0,   0,   0,   0,-inf,
               0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
               0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
            -inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,
            -inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,
            -inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,
            -inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,
            -inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf;

        CAPTURE(out);
        REQUIRE((comp == out).all());
    }

    SECTION("X")
    {
        Tree t(&s, s.operation(OP_MAX,
                   s.operation(OP_SUB,
                   s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                       s.operation(OP_MUL, s.Y(), s.Y())),
                   s.constant(1)), s.X()));

        auto out = Render(&t, r).first;

        DepthImage comp(10, 10);
        float inf = std::numeric_limits<float>::infinity();
        comp <<
            -inf,-inf,-inf,   0,   0,-inf,-inf,-inf,-inf,-inf,
            -inf,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
            -inf,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
               0,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
               0,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
               0,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
               0,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
            -inf,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
            -inf,   0,   0,   0,   0,-inf,-inf,-inf,-inf,-inf,
            -inf,-inf,-inf,   0,   0,-inf,-inf,-inf,-inf,-inf;

        CAPTURE(out);
        REQUIRE((comp == out).all());
    }
}

TEST_CASE("Render shape ")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    SECTION("X")
    {
        Region r({0, 1}, {-1, 1}, {0, 0}, 5);
        auto out = Render(&t, r).first;
        REQUIRE(out.rows() == 10);
        REQUIRE(out.cols() == 5);
    }
    SECTION("Y")
    {
        Region r({-1, 1}, {0, 1}, {0, 0}, 5);
        auto out = Render(&t, r).first;
        REQUIRE(out.rows() == 5);
        REQUIRE(out.cols() == 10);
    }
}

TEST_CASE("3D rendering of a sphere ")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
                                   s.operation(OP_MUL, s.Z(), s.Z())),
               s.constant(1)));

    SECTION("Values")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 5);
        auto out = Render(&t, r).first;

        DepthImage comp(10, 10);
        float inf = std::numeric_limits<float>::infinity();
        comp <<
            -inf,-inf,-inf, 0.3, 0.3, 0.3, 0.3,-inf,-inf,-inf,
            -inf, 0.1, 0.5, 0.5, 0.7, 0.7, 0.5, 0.5, 0.1,-inf,
            -inf, 0.5, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.5,-inf,
             0.3, 0.5, 0.7, 0.9, 0.9, 0.9, 0.9, 0.7, 0.5, 0.3,
             0.3, 0.7, 0.7, 0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.3,
             0.3, 0.7, 0.7, 0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.3,
             0.3, 0.5, 0.7, 0.9, 0.9, 0.9, 0.9, 0.7, 0.5, 0.3,
            -inf, 0.5, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.5,-inf,
            -inf, 0.1, 0.5, 0.5, 0.7, 0.7, 0.5, 0.5, 0.1,-inf,
            -inf,-inf,-inf, 0.3, 0.3, 0.3, 0.3,-inf,-inf,-inf;

        auto diff = comp - out;
        CAPTURE(out);
        CAPTURE(diff);
        REQUIRE((diff.abs() < EPSILON || diff != diff).all());
    }

}

TEST_CASE("2D rendering with normals ")
{
    Store s;

    Region r({-1, 1}, {-1, 1}, {-2,2}, 5);

    SECTION("X")
    {
        Tree t(&s, s.operation(OP_ADD, s.X(), s.Z()));
        auto norm = Render(&t, r).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd97fd9).all());
    }

    SECTION("-X")
    {
        Tree t(&s, s.operation(OP_ADD, s.Z(), s.operation(OP_NEG, s.X())));
        auto norm = Render(&t, r).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd97f25 ||
                 norm == 0xffda7f25).all());
    }

    SECTION("Y")
    {
        Tree t(&s, s.operation(OP_ADD, s.Y(), s.Z()));
        auto norm = Render(&t, r).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd9d97f ||
                 norm == 0xffdada7f).all());
    }
}

TEST_CASE("Normal clipping ")
{
    Store s;
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 5);
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    auto norm = Render(&t, r).second;

    CAPTURE(norm);
    REQUIRE((norm == 0xffff7f7f || norm == 0).all());
}

TEST_CASE("Performance")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;
    Store s;

    std::string log;

    {   // Build and render sphere
        Tree t(&s, s.operation(OP_SUB,
                   s.operation(OP_ADD,
                   s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                       s.operation(OP_MUL, s.Y(), s.Y())),
                                       s.operation(OP_MUL, s.Z(), s.Z())),
                   s.constant(1)));

        Region r({-1, 1}, {-1, 1}, {-1, 1}, 500);

        start = std::chrono::system_clock::now();
        auto out = Render(&t, r).first;
        end = std::chrono::system_clock::now();

        elapsed = end - start;

        log += "Rendered sphere in " + std::to_string(elapsed.count()) + " sec";
    }

    {   // Build and render Menger sponge (this is much prettier in Scheme)
        auto rectangle =
            [&s](float xmin, float xmax, float ymin, float ymax, glm::mat4 M)
            {
                auto x = s.affine(M[0][0], M[0][1], M[0][2], M[0][3]);
                auto y = s.affine(M[1][0], M[1][1], M[1][2], M[1][3]);
                auto z = s.affine(M[2][0], M[2][1], M[2][2], M[2][3]);

                return s.operation(OP_MAX,
                       s.operation(OP_MAX, s.operation(OP_SUB, s.constant(xmin), x),
                                           s.operation(OP_SUB, x, s.constant(xmax))),
                       s.operation(OP_MAX, s.operation(OP_SUB, s.constant(ymin), y),
                                           s.operation(OP_SUB, y, s.constant(ymax))));
            };

        std::function<Token*(float, float, float, glm::mat4, int)> recurse =
            [&](float x, float y, float scale, glm::mat4 M, int i)
            {
                auto base = rectangle(x - scale/2, x + scale/2,
                                      y - scale/2, y + scale/2, M);

                if (i == 0)
                {
                    return base;
                }
                else
                {
                    auto j = i - 1;
                    auto t = scale / 3;

                    return s.operation(OP_MIN, base,
                           s.operation(OP_MIN, recurse(x + scale, y, t, M, j),
                           s.operation(OP_MIN, recurse(x - scale, y, t, M, j),
                           s.operation(OP_MIN, recurse(x, y + scale, t, M, j),
                           s.operation(OP_MIN, recurse(x, y - scale, t, M, j),
                           s.operation(OP_MIN, recurse(x + scale, y + scale, t, M, j),
                           s.operation(OP_MIN, recurse(x + scale, y - scale, t, M, j),
                           s.operation(OP_MIN, recurse(x - scale, y + scale, t, M, j),
                                               recurse(x - scale, y - scale, t, M, j)
                           ))))))));
                }
            };

        auto M = glm::mat4();
        Token* a = recurse(0, 0, 1, M, 2);

        M = glm::rotate(M, float(M_PI/2), {1, 0, 0});
        Token* b = recurse(0, 0, 1, M, 2);

        M = glm::rotate(M, float(M_PI/2), {0, 1, 0});
        Token* c = recurse(0, 0, 1, M, 2);

        auto cube = s.operation(OP_MAX,
                    s.operation(OP_MAX,
                       s.operation(OP_MAX, s.affine(-1,  0,  0, -1.5),
                                           s.affine( 1,  0,  0, -1.5)),
                       s.operation(OP_MAX, s.affine( 0, -1,  0, -1.5),
                                           s.affine( 0,  1,  0, -1.5))),
                       s.operation(OP_MAX, s.affine( 0,  0, -1, -1.5),
                                           s.affine( 0,  0,  1, -1.5)));

        Token* cutout = s.operation(OP_NEG,
                        s.operation(OP_MIN, s.operation(OP_MIN, a, b), c));

        Tree sponge(&s, s.operation(OP_MAX, cube, cutout));

        Region r({-2.5, 2.5}, {-2.5, 2.5}, {-2.5, 2.5}, 250);
        auto rot = glm::rotate(glm::rotate(glm::mat4(), float(M_PI/4), {0, 1, 0}),
                               float(atan(1/sqrt(2))), {1, 0, 0});

        // Begin timekeeping
        start = std::chrono::system_clock::now();
        auto heightmap = Render(&sponge, r, rot).first;
        end = std::chrono::system_clock::now();

        elapsed = end - start;

        auto elapsed_ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

        log += "\nRendered sponge in " +
               std::to_string(elapsed.count()) + " sec";
    }

    WARN(log);
}
