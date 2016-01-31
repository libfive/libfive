#include <catch/catch.hpp>

#include "ao/kernel/render/heightmap.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

#define EPSILON 1e-6

// Run the standard render test suite with this render function
static std::pair<DepthImage, NormalImage> Render(Tree* t, const Region& r)
{
    std::atomic_bool abort(false);

    return Heightmap::Render(t, r, abort);
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

    SECTION("Performance")
    {
        std::chrono::time_point<std::chrono::system_clock> start, end;
        start = std::chrono::system_clock::now();

        Region r({-1, 1}, {-1, 1}, {-1, 1}, 500);
        auto out = Render(&t, r).first;

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
        if (elapsed_ms.count() > 1000)
#endif
        {
            WARN(description);
        }
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
