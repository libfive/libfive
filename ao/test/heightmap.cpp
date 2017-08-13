#include <chrono>

#include "catch.hpp"

#include "ao/render/discrete/heightmap.hpp"
#include "ao/eval/evaluator.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

#define EPSILON 1e-6

// Helper function to make rendering a single call
static std::unique_ptr<Heightmap> render(Tree t, const Voxels& r)
{
    std::atomic_bool abort(false);

    return Heightmap::render(t, r, abort);
}

TEST_CASE("Heightmap::render: 2D interval Z values")
{
    Tree t = circle(1);
    Voxels r({-1, -1, -1}, {1, 1, 1}, {25, 25, 0});

    auto out = render(t, r)->depth;
    CAPTURE(out);
    REQUIRE((out == 0 ||
             out == -std::numeric_limits<float>::infinity()).all());
}

TEST_CASE("Heightmap::render: 3D interval Z values")
{
    Tree t = circle(1);
    Voxels r({-1, -1, -1}, {1, 1, 1}, {25, 25, 25});

    auto out = render(t, r)->depth;
    CAPTURE(out);
    REQUIRE((out == r.pts[2].back() ||
             out == -std::numeric_limits<float>::infinity()).all());
}

TEST_CASE("Heightmap::render: 2D circle ")
{
    Tree t = circle(1);

    Heightmap::Depth comp(10, 10);
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
        Voxels r({-1, -1, 0}, {1, 1, 0}, {5, 5, 0});
        auto out = render(t, r)->depth;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }

    SECTION("Zero-resolution Z")
    {
        Voxels r({-1, -1, -1}, {1, 1, 1}, {5, 5, 0});
        auto out = render(t, r)->depth;
        CAPTURE(out);
        REQUIRE((comp == out).all());
    }
}

TEST_CASE("Heightmap::render: 2D circle at non-zero Z ")
{
    Tree t = circle(1);

    Voxels r({-1, -1, 1}, {1, 1, 1}, 5);
    auto out = render(t, r)->depth;
    CAPTURE(out);

    Heightmap::Depth comp(10, 10);
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

TEST_CASE("Heightmap::render: orientation")
{

    Voxels r({-1, -1, 0}, {1, 1, 0}, 5);

    SECTION("Y")
    {
        Tree t = max(circle(1), Tree::Y());
        auto out = render(t, r)->depth;

        Heightmap::Depth comp(10, 10);
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
        Tree t = max(circle(1), Tree::X());
        auto out = render(t, r)->depth;

        Heightmap::Depth comp(10, 10);
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

TEST_CASE("Heightmap::render: image shape")
{
    Tree t = circle(1);

    SECTION("X")
    {
        Voxels r({0, -1, 0}, {1, 1, 0}, 5);
        auto out = render(t, r)->depth;
        REQUIRE(out.rows() == 10);
        REQUIRE(out.cols() == 5);
    }
    SECTION("Y")
    {
        Voxels r({-1, 0, 0}, {1, 1, 0}, 5);
        auto out = render(t, r)->depth;
        REQUIRE(out.rows() == 5);
        REQUIRE(out.cols() == 10);
    }
}

TEST_CASE("Heightmap::render: 3D sphere")
{
    Tree t = sphere(1);

    SECTION("Values")
    {
        Voxels r({-1, -1, -1}, {1, 1, 1}, 5);
        auto out = render(t, r)->depth;

        Heightmap::Depth comp(10, 10);
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

TEST_CASE("Heightmap::render: 2D normals")
{
    Voxels r({-1, -1, -2}, {1, 1, 2}, 5);

    SECTION("X")
    {
        Tree t = Tree::X() + Tree::Z();
        auto norm = render(t, r)->norm;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd97fd9).all());
    }

    SECTION("-X")
    {
        Tree t = Tree::Z() + (-Tree::X());
        auto norm = render(t, r)->norm;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd97f25 ||
                 norm == 0xffda7f25).all());
    }

    SECTION("Y")
    {
        Tree t = Tree::Y() + Tree::Z();
        auto norm = render(t, r)->norm;

        CAPTURE(norm);
        REQUIRE((norm == 0xffd9d97f ||
                 norm == 0xffdada7f).all());
    }
}

TEST_CASE("Heightmap::render: Normal clipping ")
{
    Tree t = circle(1);
    Voxels r({-1, -1, -1}, {1, 1, 1}, 5);

    auto norm = render(t, r)->norm;

    CAPTURE(norm);
    REQUIRE((norm == 0xffff7f7f || norm == 0).all());
}

TEST_CASE("Heightmap::render: Performance")
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    std::string log;

    {   // Build and render sphere
        Tree t = sphere(1);

        Voxels r({-1, -1, -1}, {1, 1, 1}, 500);

        start = std::chrono::system_clock::now();
        auto out = render(t, r)->depth;
        end = std::chrono::system_clock::now();

        elapsed = end - start;

        log += "Rendered sphere in " + std::to_string(elapsed.count()) + " sec";
    }

    {   // Build and render Menger sponge
        Tree sponge = menger(2);

        Voxels r({-2.5, -2.5, -2.5}, {2.5, 2.5, 2.5}, 250);

        Eigen::Matrix3f m;
        m = Eigen::AngleAxisf(float(M_PI/4), Eigen::Vector3f::UnitY()) *
            Eigen::AngleAxisf(float(atan(1/sqrt(2))), Eigen::Vector3f::UnitX());

        auto sponge_ = sponge.remap(
            m(0,0)*Tree::X() + m(0,1)*Tree::Y() + m(0,2)*Tree::Z(),
            m(1,0)*Tree::X() + m(1,1)*Tree::Y() + m(1,2)*Tree::Z(),
            m(2,0)*Tree::X() + m(2,1)*Tree::Y() + m(2,2)*Tree::Z());

        // Begin timekeeping
        start = std::chrono::system_clock::now();
        auto heightmap = render(sponge_, r);
        end = std::chrono::system_clock::now();

        elapsed = end - start;

        auto elapsed_ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(elapsed);

        log += "\nRendered sponge in " +
               std::to_string(elapsed.count()) + " sec";
    }

    WARN(log);
}
