#include <numeric>
#include <functional>

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"
#include "ao/render/brep/scaffold.hpp"

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

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
struct Refiner
{
    Refiner(Evaluator* eval) : eval(eval) {}

    void operator()(const std::array<const XTree<N>*, (1 << N)>& a)
    {
        bool all_empty = true;
        bool all_full = true;

        for (unsigned i=0; i < (1 << N); ++i)
        {
            all_empty &= (a[i]->type == Interval::EMPTY);
            all_full  &= (a[i]->type == Interval::FILLED);
        }
        if (!all_empty && !all_full)
        {
            for (unsigned i=0; i < (1 << N); ++i)
            {
                targets.insert(a[i]);
            }
        }
    }

    Evaluator* eval;
    std::set<const XTree<N>*> targets;
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
XTree<N>::XTree(Evaluator* eval, const Scaffold<N>& scaffold)
    : region(scaffold.region)
{
    if (scaffold.children[0].get())
    {
        for (unsigned i=0; i < (1 << N); ++i)
        {
            children[i].reset(new XTree<N>(nullptr, *scaffold.children[i]));
        }
    }

    // Encode the cell type into the error field for the moment
    err = -scaffold.type - 2;

    if (eval)
    {
        Refiner<N> ref(eval);
        Dual<N>::walk(*this, ref);
    }
}

////////////////////////////////////////////////////////////////////////////////

/*  Used for compile-time checking of array bounds in findVertex */
constexpr unsigned _pow(unsigned x, unsigned y)
{
    return y ? x * _pow(x, y - 1) : 1;
}

////////////////////////////////////////////////////////////////////////////////

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

    // If the vertex ended up outside of the cell, then minimize the QEF
    // on each of the cell's boundaries and position the vertex at the best
    // boundary position.
    if ((vert < lower).any() || (vert > upper).any())
    {
        err = std::numeric_limits<float>::infinity();

        // This is our reduced matrix, which is missing an axis
        Eigen::Matrix<float, num, N> A_;
        A_.col(N - 1) = A.col(N);

        // Our b matrix is the same shape, but will be offset
        Eigen::Matrix<float, num, 1> b_;

        for (unsigned i=0; i < N*2; ++i)
        {
            // Pick out axis and boundary value
            auto axis = i / 2;
            auto value = ((i & 1) ? lower : upper)(axis);

            // Construct modified A matrix by skipping one axis
            unsigned k=0;
            for (unsigned j=0; j < N; ++j)
            {
                if (j != axis)
                {
                    A_.col(k++) = A.col(j);
                }
            }

            // Apply removed axis to the b_ matrix
            b_ = b - A.col(axis) * value;

            // Find new solution and error thereof
            auto sol = A_.jacobiSvd(Eigen::ComputeThinU |
                                    Eigen::ComputeThinV).solve(b_);
            auto err_ = (A_ * sol - b_).squaredNorm();

            if (err_ < err)
            {
                err = err_;
                // Save bounded vertex position
                unsigned k = 0;
                for (unsigned j=0; j < N; ++j)
                {
                    if (j != axis)
                    {
                        vert(j) = sol(k++);
                    }
                }
                vert(axis) = value;
            }
        }
        return err < 1e-6;
    }
    else
    {
        err = (A * sol - b).squaredNorm();
        return err < 1e-6;
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Eigen::Vector3f XTree<N>::vert3() const
{
    Eigen::Vector3f out;
    out << vert, region.perp;
    return out;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of templates
template class XTree<2>;
template class XTree<3>;

}   // namespace Kernel
