#include <iostream>
#include <chrono>

#include <catch/catch.hpp>

#include "ao/render/heightmap.hpp"
#include "ao/core/tree.hpp"
#include "ao/core/store.hpp"

TEST_CASE("2D rendering of a circle (CPU)")
{
    std::atomic<bool> abort(false);

    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    DepthImage comp(10, 10);
    double inf = std::numeric_limits<double>::infinity();
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
        auto out = Heightmap::Render(&t, r, abort).first;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }

    SECTION("Zero-resolution Z")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 5, 5, 0);
        auto out = Heightmap::Render(&t, r, abort).first;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }
}

TEST_CASE("2D interval Z values")
{
    std::atomic<bool> abort(false);

    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 0);

    auto out = Heightmap::Render(&t, r, abort).first;
    CAPTURE(out);
    REQUIRE((out == 0 ||
             out == -std::numeric_limits<double>::infinity()).all());
}

TEST_CASE("3D interval Z values")
{
    std::atomic<bool> abort(false);

    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 25);

    auto out = Heightmap::Render(&t, r, abort).first;
    CAPTURE(out);
    REQUIRE((out == r.Z.pos(r.Z.size - 1) ||
             out == -std::numeric_limits<double>::infinity()).all());
}

TEST_CASE("Render orientation (CPU)")
{
    std::atomic<bool> abort(false);

    Store s;
    Region r({-1, 1}, {-1, 1}, {0, 0}, 5);

    SECTION("Y")
    {
        Tree t(&s, s.operation(OP_MAX,
                   s.operation(OP_SUB,
                   s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                       s.operation(OP_MUL, s.Y(), s.Y())),
                   s.constant(1)), s.Y()));

        auto out = Heightmap::Render(&t, r, abort).first;

        DepthImage comp(10, 10);
        double inf = std::numeric_limits<double>::infinity();
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

        auto out = Heightmap::Render(&t, r, abort).first;

        DepthImage comp(10, 10);
        double inf = std::numeric_limits<double>::infinity();
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

TEST_CASE("Render shape (CPU)")
{
    std::atomic<bool> abort(false);

    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    SECTION("X")
    {
        Region r({0, 1}, {-1, 1}, {0, 0}, 5);
        auto out = Heightmap::Render(&t, r, abort).first;
        REQUIRE(out.rows() == 10);
        REQUIRE(out.cols() == 5);
    }
    SECTION("Y")
    {
        Region r({-1, 1}, {0, 1}, {0, 0}, 5);
        auto out = Heightmap::Render(&t, r, abort).first;
        REQUIRE(out.rows() == 5);
        REQUIRE(out.cols() == 10);
    }
}

TEST_CASE("3D rendering of a sphere (CPU)")
{
    std::atomic<bool> abort(false);

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
        auto out = Heightmap::Render(&t, r, abort).first;

        DepthImage comp(10, 10);
        double inf = std::numeric_limits<double>::infinity();
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
        REQUIRE((diff.abs() < 1e-10 || diff != diff).all());
    }

    SECTION("Performance")
    {
        std::chrono::time_point<std::chrono::system_clock> start, end;
        start = std::chrono::system_clock::now();

        Region r({-1, 1}, {-1, 1}, {-1, 1}, 100);
        auto out = Heightmap::Render(&t, r, abort).first;

        end = std::chrono::system_clock::now();
        std::chrono::duration<double> elapsed = end - start;

        auto elapsed_ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

        auto description = "Rendered sphere in " +
                           std::to_string(elapsed.count()) + " sec";

        // Check for major regressions in render performance
#ifdef RELEASE
        if (elapsed_ms.count() > 50)
#else
        if (elapsed_ms.count() > 500)
#endif
        {
            WARN(description);
        }
    }
}

TEST_CASE("2D rendering with normals (CPU)")
{
    std::atomic<bool> abort(false);

    Store s;

    Region r({-1, 1}, {-1, 1}, {0}, 5);

    SECTION("X")
    {
        Tree t(&s, s.X());
        auto norm = Heightmap::Render(&t, r, abort, false).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xff7f7fff || norm == 0).all());
    }

    SECTION("-X")
    {
        Tree t(&s, s.operation(OP_NEG, s.X()));
        auto norm = Heightmap::Render(&t, r, abort, false).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xff7f7f00 || norm == 0).all());
    }

    SECTION("Y")
    {
        Tree t(&s, s.Y());
        auto norm = Heightmap::Render(&t, r, abort, false).second;

        CAPTURE(norm);
        REQUIRE((norm == 0xff7fff7f || norm == 0).all());
    }

}

TEST_CASE("Normal clipping")
{
    std::atomic<bool> abort(false);

    Store s;
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 5);
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    auto norm = Heightmap::Render(&t, r, abort).second;

    CAPTURE(norm);
    REQUIRE((norm == 0xffff7f7f || norm == 0).all());
}
