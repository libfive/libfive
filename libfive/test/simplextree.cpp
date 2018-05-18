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

#include <fstream>

#include "libfive/render/simplex/simplextree.hpp"
#include "libfive/render/simplex/simplex.hpp"
#include "libfive/render/brep/eval_xtree.hpp"
#include "libfive/render/simplex/walk2d.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SimplexTree<3>::SimplexTree")
{
    auto s = sphere(1);
    auto eval = XTreeEvaluator(s);

    auto t = SimplexTree<3>(&eval, Region<3>({-2, -2, -2}, {2, 2, 2}));
    REQUIRE(t.type == Interval::AMBIGUOUS);
    t.findVertices(&eval);
    t.checkVertices(&eval);

    REQUIRE(true);
}

TEST_CASE("SimplexTree<2>::SimplexTree")
{
    auto s = circle(1);
    auto eval = XTreeEvaluator(s);

    auto t = SimplexTree<2>(&eval, Region<2>({-2, -2}, {2, 2}));
    REQUIRE(t.type == Interval::AMBIGUOUS);

    t.findVertices(&eval);
    t.checkVertices(&eval);
    REQUIRE(true);
}

template <unsigned N>
std::list<const SimplexTree<N>*> leafs(SimplexTree<N>* root)
{
    std::list<const SimplexTree<N>*> todo = {root};
    std::list<const SimplexTree<N>*> out;
    while (todo.size())
    {
        auto next = todo.front();
        todo.pop_front();
        if (next->isBranch())
        {
            for (auto& n : next->children)
            {
                todo.push_back(n.get());
            }
        }
        else
        {
            out.push_back(next);
        }
    }
    return out;
}

TEST_CASE("SimplexTree<2>: Vertex placement")
{
    // This shape has no sharp features in the distance field, so every vertex
    // should be at the center of its respective simplex.
    auto s = Tree::X();
    auto eval = XTreeEvaluator(s);
    Region<2> r({-2, -2}, {2, 2});

    auto contours = walk2d(&eval, r, 2, 4, 0.0001);

    for (auto t : leafs(contours.second.get()))
    {
        if (t->type != Interval::AMBIGUOUS)
        {
            continue;
        }
        for (unsigned i=0; i < t->vertices.cols(); ++i)
        {
            Eigen::Vector2d center = Eigen::Vector2d::Zero();
            int count = 0;
            for (auto c : {0, 1, 3, 4})
            {
                if (Simplex<2>::fromIndex(i).containsSimplex(
                            Simplex<2>::fromIndex(c)))
                {
                    center += t->vertices.col(c).head<2>();
                    count++;
                }
            }
            assert(count);
            center /= count;

            auto v = t->vertices.col(i).head<2>().eval();
            auto err = v - center;
            CAPTURE(v.transpose());
            CAPTURE(center.transpose());
            REQUIRE(err.norm() < 1e-6);
        }
    }
}

TEST_CASE("SimplexTree<2>: Vertex error")
{
    auto s = circle(1);
    auto eval = XTreeEvaluator(s);

    {   // Any point on the circle that doesn't contain the center
        // will be error-full.
        auto t = SimplexTree<2>(&eval, {{1, 1}, {2, 2}});
        auto err = t.findVertices(&eval);
        CAPTURE(err);
        REQUIRE(err > 1e-6);
    }

    {   // A circle can have low error at the center
        auto t = SimplexTree<2>(&eval, {{-1, -1}, {2, 2}});
        auto err = t.findVertices(&eval);
        CAPTURE(err);
        REQUIRE(err < 1e-6);
    }
}

TEST_CASE("SimplexTree<2>: Max depth")
{
    auto s = circle(1);
    auto eval = XTreeEvaluator(s);

    auto contours = walk2d(&eval, {{-2, -2}, {2, 2}}, 2, 4, 0.0001);
    unsigned max_depth = 0;

    for (auto t : leafs(contours.second.get()))
    {
        if (t->type != Interval::AMBIGUOUS)
        {
            continue;
        }
        max_depth = std::max(max_depth, t->depth);
    }
    REQUIRE(max_depth == 4);
}

#include "libfive/render/discrete/heightmap.hpp"
#include "libfive/render/brep/contours.hpp"
#include "libfive.h"
TEST_CASE("SimplexTree<2>: SVG debugging")
{
    //auto s = move(circle(1), {0.0, 0.1, 0.0});
    //auto s = move(max(Tree::X(), Tree::Y()), {0.0, 0.1, 0});
    //auto s = max(circle(1), gyroid2d(50, 0.1));
    //auto s = max(circle(1), -circle(0.9));
    //auto s = Tree::Y();
    /*
    auto s = move(max(Tree::X(), Tree::Y()), {0.0, 0.1, 0});
    */

    auto s = *libfive_tree_load("magic.frep");
    Region<2> r({-1, -10}, {19, 20});

        /*
    auto s = max(max(-Tree::X() - 0.45, Tree::X() - 0.65),
                 max(-Tree::Y() - 0.15, Tree::Y() - 1.15));
    Region<2> r({0, 0}, {2, 2});
    */
    auto eval = XTreeEvaluator(s);

    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    start = std::chrono::system_clock::now();

    for (unsigned i=0; i < 20; ++i)
    {
        //auto contours = walk2d(&eval, r, 2, 4, 0.0001);
    }
    auto out = walk2d(&eval, r, 4, 8, 0.0001);
    auto contours = out.first;
    auto& t = out.second;
    end = std::chrono::system_clock::now();
    std::cout << "Ran in " << (end - start).count() << "\n";

    std::ofstream file;
    file.open("out.svg", std::ios::out);
    file << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
    "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"\n"
      " width=\"" << r.upper.x() - r.lower.x() <<
    "\" height=\"" << r.upper.y() - r.lower.y() <<
    "\" id=\"libfive\">\n";
    file << "<image xlink:href=\"out.png\" x=\"0\" y=\"0\" "
        << " width=\"" << r.upper.x() - r.lower.x()
        << "\" height=\"" << r.upper.y() - r.lower.y() << "\" />\n";

    /*
    for (auto next : leafs(t.get()))
    {
        if ((next->type == Interval::FILLED || next->type == Interval::EMPTY) &&
                !next->complete)
        {
            auto v = next->region.center();
            auto fill = (next->type == Interval::FILLED ? "white" : "black");
            auto stroke = (next->type == Interval::FILLED ? "black" : "white");
            file << "<circle cx=\"" << v.x() - r.lower.x() << "\" "
                 << "cy=\"" << r.upper.y() - v.y() << "\" "
                 << "r=\"0.01\" stroke-width=\"0.005\" stroke=\"" << stroke << "\" "
                 << "fill=\"" << fill << "\" />\n";
        }

        if (!next->complete) continue;
        for (unsigned i=0; i < next->vertices.cols(); ++i)
        {
            auto v = next->vertices.col(i).eval();
            std::string fill;
            switch (Simplex<2>::fromIndex(i).freeAxes())
            {
                case 0: fill = "red"; break;
                case 1: fill = "yellow"; break;
                case 2: fill = "green"; break;
                default: fill = "white"; break;
            }
            file << "<circle cx=\"" << v.x() - r.lower.x() << "\" "
                 << "cy=\"" << r.upper.y() - v.y() << "\" "
                 << "r=\"0.01\" stroke-width=\"none\" "
                 << "fill=\"" << fill << "\" />\n";
        }

        for (unsigned i : {0, 1, 4, 3})
        {
            auto v = next->vertices.col(i).eval();
            if (i == 0)
            {
                file << "<path d=\"M ";
            }
            else
            {
                file << "L ";
            }
            file << v.x() - r.lower.x() << " " << r.upper.y() - v.y() << " ";
        }
        file << "Z\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.005\"/>\n";

        auto center = next->vertices.col(8);
        for (unsigned i : {0, 1, 4, 3})
        {
            auto v = next->vertices.col(i).eval();
            file << "<path d=\"M ";
            file << v.x() - r.lower.x() << " " << r.upper.y() - v.y() << " ";
            file << center.x() - r.lower.x() << " " << r.upper.y() - center.y() << " ";
            file << "\" fill=\"none\" stroke=\"green\" stroke-width=\"0.005\"/>\n";
        }
        for (unsigned i : {2, 5, 6, 7})
        {
            auto v = next->vertices.col(i).eval();
            file << "<path d=\"M ";
            file << v.x() - r.lower.x() << " " << r.upper.y() - v.y() << " ";
            file << center.x() - r.lower.x() << " " << r.upper.y() - center.y() << " ";
            file << "\" fill=\"none\" stroke=\"yellow\" stroke-width=\"0.005\"/>\n";
        }
    }
    */

    for (auto& e : contours.branes)
    {
        file << "<path d=\"M ";
        auto a = contours.verts[e(0)];
        auto b = contours.verts[e(1)];

        file << a.x() - r.lower.x() << " " << r.upper.y() - a.y() << " ";
        file << b.x() - r.lower.x() << " " << r.upper.y() - b.y() << " ";
        file << "\" fill=\"none\" stroke=\"red\" stroke-width=\"0.02\"/>\n";
    }

    file << "</svg>";


    Voxels v(r.lower3().template cast<float>(),
             r.upper3().template cast<float>(), {350, 350, 0});
    std::atomic_bool abort(false);
    Heightmap::render(s, v, abort)->savePNG("out.png");

    REQUIRE(true);

    start = std::chrono::system_clock::now();
    for (unsigned i=0; i < 20; ++i)
    {
        auto c = Contours::render(s, r, 0.26);
    }
    auto c = Contours::render(s, r, 0.2);
    end = std::chrono::system_clock::now();
    std::cout << "Ran in " << (end - start).count() << "\n";
    c->saveSVG("out2.svg");
}
