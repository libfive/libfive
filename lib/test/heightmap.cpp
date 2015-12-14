#include <iostream>
#include <chrono>

#include <catch/catch.hpp>

#include "heightmap.hpp"
#include "tree.hpp"
#include "store.hpp"

TEST_CASE("2D rendering of a circle")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    SECTION("Without recursion")
    {
        Region r({-1, 1}, {-1, 1}, {0, 0}, 5);
        auto out = Heightmap::Render(&t, r);

        Eigen::ArrayXXd comp(10, 10);
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

        REQUIRE((comp == out).all());
    }

    SECTION("With recursion")
    {
        Region r({-1, 1}, {-1, 1}, {0, 0}, 100);
        auto out = Heightmap::Render(&t, r);
    }
}

TEST_CASE("3D rendering of a sphere")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
                                   s.operation(OP_MUL, s.Z(), s.Z())),
               s.constant(1)));

    SECTION("Low resolution")
    {
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 5);
        auto out = Heightmap::Render(&t, r);

        Eigen::ArrayXXd comp(10, 10);
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

        REQUIRE((comp == out).all());
    }

    SECTION("High resolution")
    {
        std::chrono::time_point<std::chrono::system_clock> start, end;
        start = std::chrono::system_clock::now();

        Region r({-1, 1}, {-1, 1}, {-1, 1}, 100);
        auto out = Heightmap::Render(&t, r);

        end = std::chrono::system_clock::now();
        std::chrono::duration<double> elapsed = end - start;

        std::cout << "Rendered sphere in " << elapsed.count()
                  << " sec\n";
    }
}
