#include <catch/catch.hpp>

#include "ao/render/heightmap.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

// Run the standard render test suite with this render function
static std::pair<DepthImage, NormalImage> RENDER(Tree* t, const Region& r)
{
    std::atomic<bool> abort(false);

    Evaluator e(t);
    return Heightmap::Render(&e, r, abort);
}

TEST_CASE("2D interval Z values")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 0);

    auto out = RENDER(&t, r).first;
    CAPTURE(out);
    REQUIRE((out == 0 ||
             out == -std::numeric_limits<double>::infinity()).all());
}

TEST_CASE("3D interval Z values")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 25, 25, 25);

    auto out = RENDER(&t, r).first;
    CAPTURE(out);
    REQUIRE((out == r.Z.pos(r.Z.size - 1) ||
             out == -std::numeric_limits<double>::infinity()).all());
}

#define DESCRIPTION "(CPU)"
#define EPSILON 1e-10
#include "render.ipp"
