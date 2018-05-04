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
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SimplexTree<3>::SimplexTree")
{
    auto s = sphere(1);
    auto eval = XTreeEvaluator(s);

    auto t = SimplexTree<3>(&eval, Region<3>({-2, -2, -2}, {2, 2, 2}),
                            0.5, 0.1, 4);
    REQUIRE(true);
}

TEST_CASE("SimplexTree<2>::SimplexTree")
{
    auto s = circle(1);
    auto eval = XTreeEvaluator(s);

    auto t = SimplexTree<2>(&eval, Region<2>({-2, -2}, {2, 2}),
                            0.5, 0.1, 4);
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

    auto t = SimplexTree<2>(&eval, r, 0.5, 0.01, 4);

    for (auto t : leafs(&t))
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

#include "libfive/render/discrete/heightmap.hpp"
#include "libfive/render/simplex/walk2d.hpp"
TEST_CASE("SimplexTree<2>: SVG debugging")
{
    //auto s = move(circle(1), {0.0, 0.1, 0.0});
    //auto s = move(max(Tree::X(), Tree::Y()), {0.0, 0.1, 0});
    auto s = move(menger2d(1), {0.01, 0.2, 1.4});
    //auto s = Tree::Y();

    auto eval = XTreeEvaluator(s);
    Region<2> r({-2, -2}, {2, 2});
    auto t = SimplexTree<2>(&eval, r, 0.5, 0.001, 1);

    auto contours = walk2d(&t);

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

    std::list<const SimplexTree<2>*> todo = {&t};
    for (auto next : leafs(&t))
    {
        if (next->type != Interval::AMBIGUOUS) continue;
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
                 << "r=\"0.02\" stroke=\"black\" stroke-width=\"0.001\" "
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
        file << "Z\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.01\"/>\n";

        auto center = next->vertices.col(8);
        for (unsigned i : {0, 1, 4, 3})
        {
            auto v = next->vertices.col(i).eval();
            file << "<path d=\"M ";
            file << v.x() - r.lower.x() << " " << r.upper.y() - v.y() << " ";
            file << center.x() - r.lower.x() << " " << r.upper.y() - center.y() << " ";
            file << "\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.01\"/>\n";
        }
    }

    for (auto& e : contours.branes)
    {
        file << "<path d=\"M ";
        auto a = contours.verts[e(0)];
        auto b = contours.verts[e(1)];

        file << a.x() - r.lower.x() << " " << r.upper.y() - a.y() << " ";
        file << b.x() - r.lower.x() << " " << r.upper.y() - b.y() << " ";
        file << "\" fill=\"none\" stroke=\"red\" stroke-width=\"0.01\"/>\n";
    }

    file << "</svg>";


    Voxels v(r.lower3().template cast<float>(),
             r.upper3().template cast<float>(), {150, 150, 0});
    std::atomic_bool abort(false);
    Heightmap::render(s, v, abort)->savePNG("out.png");

    REQUIRE(true);
}
