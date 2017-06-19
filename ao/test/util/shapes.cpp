#include "ao/tree/tree.hpp"

#include "shapes.hpp"

using namespace Kernel;

Tree rectangle(float xmin, float xmax, float ymin, float ymax,
               Eigen::Matrix4f M)
{
    auto x = M(0,0)*Tree::X() + M(0,1)*Tree::Y() + M(0,2)*Tree::Z() + M(0,3);
    auto y = M(1,0)*Tree::X() + M(1,1)*Tree::Y() + M(1,2)*Tree::Z() + M(1,3);

    return max(max(xmin - x, x - xmax), max(ymin - y, y - ymax));
}

Tree recurse(float x, float y, float scale, Eigen::Matrix4f M, int i)
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

        return min(base,
               min(recurse(x + scale, y, t, M, j),
               min(recurse(x - scale, y, t, M, j),
               min(recurse(x, y + scale, t, M, j),
               min(recurse(x, y - scale, t, M, j),
               min(recurse(x + scale, y + scale, t, M, j),
               min(recurse(x + scale, y - scale, t, M, j),
               min(recurse(x - scale, y + scale, t, M, j),
                   recurse(x - scale, y - scale, t, M, j)
               ))))))));
    }
}

Tree menger(int i)
{
    Eigen::Matrix3f m = Eigen::Matrix3f::Identity();
    Eigen::Matrix4f M = Eigen::Matrix4f::Zero();
    M.block<3,3>(0,0) = m;
    Tree a = recurse(0, 0, 1, M, i);

    m = Eigen::AngleAxisf(float(M_PI/2), Eigen::Vector3f::UnitX());
    M.block<3,3>(0,0) = m;
    Tree b = recurse(0, 0, 1, M, i);

    m = Eigen::AngleAxisf(float(M_PI/2), Eigen::Vector3f::UnitY());
    M.block<3,3>(0,0) = m;
    Tree c = recurse(0, 0, 1, M, i);

    auto cube = max(max(
                    max(-(Tree::X() + 1.5),
                          Tree::X() - 1.5),
                    max(-(Tree::Y() + 1.5),
                          Tree::Y() - 1.5)),
                    max(-(Tree::Z() + 1.5),
                          Tree::Z() - 1.5));

    auto cutout = -min(min(a, b), c);
    return max(cube, cutout);
}

Tree circle(float r)
{
    return square(Tree::X()) + square(Tree::Y()) - pow(r, 2);
}

Tree sphere(float r)
{
    return sqrt(square(Tree::X()) + square(Tree::Y()) + square(Tree::Z())) - r;
}
