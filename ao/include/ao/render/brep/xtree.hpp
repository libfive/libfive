#pragma once
#include <iostream>

#include <array>
#include <cstdint>
#include <Eigen/Eigen>

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

template <unsigned N>
class XTree
{
public:
    XTree(Evaluator* eval, Region<N> region)
        : region(region)
    {
        // Do a preliminary evaluation to prune the tree
        eval->eval(region.lower3(), region.upper3());
        eval->push();

        if (!findVertex(eval))
        {
            auto rs = region.subdivide();
            for (unsigned i=0; i < (1 << N); ++i)
            {
                if (!rs[i].empty())
                {
                    children[i].reset(new XTree<N>(eval, rs[i]));
                }
            }
        }
        eval->pop();
    }

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  The region filled by this XTree */
    Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<XTree<N>>, 1 << N> children;

    /*  Vertex location, if this is a leaf  */
    Eigen::Array<float, N, 1> vert;

protected:
    /*  Used for compile-time checking of array bounds in findVertex */
    static constexpr unsigned _pow(unsigned x, unsigned y)
    {
        return y ? x * _pow(x, y - 1) : 1;
    }

    /*
     *  Searches for a vertex within the XTree cell, using a QEF
     *  and `res` samples per axis.
     *
     *  Returns true if a vertex is found with sufficiently low error,
     *  otherwise false.
     */
    template <unsigned R=4>
    bool findVertex(Evaluator* eval)
    {
        constexpr unsigned num = _pow(R, N);
        static_assert(num < Result::N, "Bad resolution");

        // Evaluation takes place in 3-space regardless of tree dimensionality
        // because evaluators need X / Y / Z coordinates
        auto lower = region.lower;
        auto upper = region.upper;
        auto center = (region.lower + region.upper) / 2;
        Eigen::Array<float, R, N> pts;

        // Pre-compute per-axis grid positions
        for (unsigned i=0; i < R; ++i)
        {
            const float frac = (i + 0.5) / R;
            pts.row(i) = lower * (1 - frac) + upper * frac;
        }

        // Load all sample points into the evaluator
        Eigen::Array<float, num, N> positions;
        for (unsigned i=0; i < num; ++i)
        {
            Eigen::Vector3f pos; // the evaluator works in 3-space

            // Unpack from grid positions into the position vector
            for (unsigned j=0; j < N; ++j)
            {
                pos(j) = pts((i % _pow(R, j + 1)) / _pow(R, j), j);
                positions(i, j) = pos(j);
            }
            pos.tail<3 - N>() = region.perp;
            eval->set(pos, i);
        }

        // Get derivatives!
        auto ds = eval->derivs(num);

        // Load data into QEF arrays here
        Eigen::Matrix<float, num, N + 1> A;
        Eigen::Matrix<float, num, 1> b;
        for (unsigned i=0; i < num; ++i)
        {
            Eigen::Array3f deriv(ds.dx[i], ds.dy[i], ds.dz[i]);
            Eigen::Matrix<float, 1, N + 1> n;

            A(i, N) = -1;
            n(N) = ds.v[i];
            for (unsigned j=0; j < N; ++j)
            {
                A(i, j) = deriv(j);
                n(j) = positions(i, j);
            }
            b(i) = A.row(i).dot(n);
        }

        // Solve QEF (least-squares)
        auto sol = A.jacobiSvd(Eigen::ComputeThinU |
                               Eigen::ComputeThinV).solve(b);
        std::cout << A << "\n\n";
        std::cout << b << "\n\n";
        std::cout << "sol:\n" << sol << "\n";

        std::cout << "result:" << A * sol << '\n';

        // Check error
        return true;
    }
};

}   // namespace Kernel
