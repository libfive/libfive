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
        Region r({-1, 1}, {-1, 1}, {0, 0}, 10);
        auto out = Heightmap::Render(&t, r);
        std::cout << "2D circle:\n" << out << "\n";
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
        Region r({-1, 1}, {-1, 1}, {-1, 1}, 10);
        auto out = Heightmap::Render(&t, r);
        std::cout << "3D sphere:\n" << out << "\n";
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
