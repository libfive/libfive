#include <numeric>
#include <functional>

#include "ao/render/brep/xtree.hpp"

namespace Kernel {

template <unsigned N>
XTree<N>::XTree(Evaluator* eval, Region<N> region)
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

/*  Used for compile-time checking of array bounds in findVertex */
constexpr unsigned _pow(unsigned x, unsigned y)
{
    return y ? x * _pow(x, y - 1) : 1;
}

template <unsigned N>
template <unsigned R>
bool XTree<N>::findVertex(Evaluator* eval)
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
        // Unpack from grid positions into the position vector
        for (unsigned j=0; j < N; ++j)
        {
            positions(i, j) = pts((i % _pow(R, j + 1)) / _pow(R, j), j);
        }

        // The evaluator works in 3-space,
        // regardless of the XTree's dimensionality
        Eigen::Vector3f pos;
        pos << positions.row(i).transpose(), region.perp;
        eval->set(pos, i);
    }

    // Get derivatives!
    auto ds = eval->derivs(num);

    // Load data into QEF arrays here
    Eigen::Matrix<float, num, N + 1> A;
    Eigen::Matrix<float, num, 1> b;

    // Find average value for w0, which we'll use as an offset
    //
    // This, in conjunction with offsetting by the cell's center, forces
    // the solver to minimize towards the center of the cell and towards
    // the average distance field value, rather than making strange
    // trade-offs (e.g. moving out of the cell towards the 0 position of
    // the distance field).
    const float w0 = std::accumulate(
            ds.v, ds.v + num, 0.0f, std::plus<float>()) / num;

    for (unsigned i=0; i < num; ++i)
    {
        // Load this row of A matrix
        auto derivs = Eigen::Array3f(ds.dx[i], ds.dy[i], ds.dz[i]);
        A.row(i) << derivs.head<N>().transpose(), -1;

        // Temporary variable for dot product
        Eigen::Matrix<float, 1, N + 1> n;
        n << (positions.row(i) - center.transpose()), (ds.v[i] - w0);

        b(i) = A.row(i).dot(n);
    }

    // Solve QEF (least-squares)
    auto sol = A.jacobiSvd(Eigen::ComputeThinU |
                           Eigen::ComputeThinV).solve(b);

    // Store vertex location
    vert = sol.template head<N>().array() + center;

    std::cout << "vert:\n" << vert << "\n";

    auto err = A * sol - b;
    std::cout << "err: " << err.dot(err) << '\n';

    // Check error
    return true;
}

// Explicit initialization of templates
template class XTree<2>;
template class XTree<3>;

}   // namespace Kernel
