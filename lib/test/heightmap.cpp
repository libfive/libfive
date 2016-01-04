#include <iostream>
#include <chrono>

#include <catch/catch.hpp>

#include "ao/render/heightmap.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

TEST_CASE("2D interval Z values")
{
    std::atomic<bool> abort(false);

    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Evaluator e(&t);
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 0);

    auto out = Heightmap::Render(&e, r, abort).first;
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
    Evaluator e(&t);
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 25);

    auto out = Heightmap::Render(&e, r, abort).first;
    CAPTURE(out);
    REQUIRE((out == r.Z.pos(r.Z.size - 1) ||
             out == -std::numeric_limits<double>::infinity()).all());
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
    Evaluator e(&t);

    auto norm = Heightmap::Render(&e, r, abort).second;

    CAPTURE(norm);
    REQUIRE((norm == 0xffff7f7f || norm == 0).all());
}

// Run the standard render test suite with this render function
static std::pair<DepthImage, NormalImage> RENDER(Tree* t, const Region& r)
{
    std::atomic<bool> abort(false);

    Evaluator e(t);
    return Heightmap::Render(&e, r, abort);
}

#define DESCRIPTION "(CPU)"
#define EPSILON 1e-10
#include "render.ipp"
