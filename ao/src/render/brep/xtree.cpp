#include <numeric>
#include <functional>
#include <limits>

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"
#include "ao/render/brep/scaffold.hpp"

namespace Kernel {

template <unsigned N>
XTree<N>::XTree(Evaluator* eval, Region<N> region, float max_err)
    : region(region), vert(region.center()), max_error(max_err)
{
    // Do a preliminary evaluation to prune the tree
    eval->eval(region.lower3(), region.upper3());
    eval->push();

    if (!findVertex(eval))
    {
        auto rs = region.subdivide();
        for (unsigned i=0; i < (1 << N); ++i)
        {
            children[i].reset(new XTree<N>(eval, rs[i], max_err));
        }
    }
    eval->pop();
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
struct Refiner
{
    void operator()(const std::array<XTree<N>*, (1 << N)>& as)
    {
        bool all_empty = true;
        bool all_full = true;

        for (auto& a : as)
        {
            assert(a->type != Interval::UNKNOWN);
            all_empty &= (a->type == Interval::EMPTY);
            all_full  &= (a->type == Interval::FILLED);
        }
        if (!all_empty && !all_full)
        {
            for (auto& a : as)
            {
                targets.insert(a);
            }
        }
    }

    std::set<XTree<N>*> targets;
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
XTree<N>::XTree(Evaluator* eval, const Scaffold<N>& scaffold, float max_err)
    : region(scaffold.region), vert(region.center()),
      type(scaffold.type), max_error(max_err)
{
    // Recurse until the scaffold is empty
    if (scaffold.children[0].get())
    {
        for (unsigned i=0; i < (1 << N); ++i)
        {
            children[i].reset(new XTree<N>(
                        nullptr, *scaffold.children[i], max_err));
        }
    }

    // If this is the root, then walk through the dual grid, tagging
    // cells that could include parts of the mesh (and therefore need
    // vertex positioning / refinement).
    if (eval)
    {
        Refiner<N> ref;
        Dual<N>::walk(*this, ref);

        for (auto& t : ref.targets)
        {
            if (!t->findVertex(eval))
            {
                auto rs = t->region.subdivide();
                for (unsigned i=0; i < (1 << N); ++i)
                {
                    t->children[i].reset(new XTree<N>(eval, rs[i], max_err));
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

/*  Used for compile-time checking of array bounds in findVertex */
constexpr static unsigned _pow(unsigned x, unsigned y)
{ return y ? x * _pow(x, y - 1) : 1; }

/*  Used for compile-time array construction in solveQEF  */
constexpr static unsigned _count_bits(unsigned x)
{ return x ? (x & 1) + _count_bits(x >> 1) : 0; }

////////////////////////////////////////////////////////////////////////////////

template<unsigned N, unsigned R, unsigned C>
class Solver
{
public:
static std::pair<Eigen::Array<float, N, 1>, float> solveQEF(
        const Eigen::Matrix<float, _pow(R, N), N + 1>& A,
        const Eigen::Matrix<float, _pow(R, N), 1>& b,
        const Region<N>& region)
{
    constexpr unsigned CONSTRAINED_AXES_COUNT = _count_bits(C);
    static_assert(C > 0, "Unconstrained optimization shouldn't use solveQEF");
    static_assert(C < _pow(2, N), "solveQEF called with too-large bitfield");
    static_assert(CONSTRAINED_AXES_COUNT <= N , "solveQEF called with too-large bitfield");

    // Our reduced A matrix still has the w column
    Eigen::Matrix<float, _pow(R, N), N + 1 - CONSTRAINED_AXES_COUNT> A_;
    A_.col(N - CONSTRAINED_AXES_COUNT) = A.col(N);

    // The b matrix is expanded to search for multiple possible solutions,
    // e.g. if we're constrained on the X axis, then we'll solve for
    //      X = upper.X     and     X = lower.Y
    // simultaneously then pick the best result.
    Eigen::Matrix<float, _pow(R, N), 1 << CONSTRAINED_AXES_COUNT> b_;

    // Construct the pruned A matrix and record which axes are constrained
    uint8_t constrained_axes[CONSTRAINED_AXES_COUNT];
    for (unsigned i=0, c=0, a=0; i < N; ++i)
    {
        // If this axis is constrained, then record that fact
        if (C & (1 << i))
        {
            constrained_axes[a++] = i;
        }
        // Otherwise, keep it in the A matrix
        else
        {
            A_.col(c++) = A.col(i);
        }
    }

    // Figure out all of the possible constrained axes values, storing them
    // into an array named 'verts' and removing the from the appropriate
    // column of the b_ matrix
    Eigen::Matrix<float, N + 1, (1 << CONSTRAINED_AXES_COUNT)> verts;
    for (unsigned i=0; i < (1 << CONSTRAINED_AXES_COUNT); ++i)
    {
        // Start with the original b vector
        b_.col(i) = b;

        for (unsigned j=0; j < CONSTRAINED_AXES_COUNT; ++j)
        {
            auto a = constrained_axes[j];
            // Pick whether we're constraining to the top or bottom of the axis
            verts(a, i) = ((i & (1 << j))
                    ? region.lower(a)
                    : region.upper(a));

            // Then offset by the region's center, as we do for unconstrained
            // points as well.
            verts(a, i) -= region.center()(a);

            // Strip out the offset from this particular column of b_
            b_.col(i).array() -= (A.col(a) * verts(a, i)).array();
        }
    }

    // Find constrained solution
    auto sol = A_.colPivHouseholderQr().solve(b_);

    // Unpack the solution into a set of full positions
    for (unsigned i=0; i < (1 << CONSTRAINED_AXES_COUNT); ++i)
    {
        // Store the w value at the end of the vertex
        verts(N, i) = sol(N - CONSTRAINED_AXES_COUNT, i);

        // Then, unpack either solved or constrained values
        for (unsigned j=0, k=0; j < N; ++j)
        {
            // If this axis was unconstrained, then pick out its position
            // value from the solution array
            if (!(C & (1 << j)))
            {
                verts(j, i) = sol(k++, i);
            }
        }
    }

    auto errs = ((A * verts).colwise() - b).colwise().squaredNorm();
    unsigned best_index;
    float err_ = errs.minCoeff(&best_index);
    auto vert_ = verts.col(best_index).template head<N>().array() + region.center();

    if (!region.contains(vert_))
    {
        err_ = std::numeric_limits<float>::infinity();
    }

    auto next = Solver<N, R, C - 1>::solveQEF(A, b, region);
    return (next.second < err_) ? next : std::make_pair(vert_, err_);
}
};

/*
 *  Partial template specialization to make expansion terminate
 */
template<unsigned N, unsigned R>
class Solver<N, R, 0>
{
public:
static std::pair<Eigen::Array<float, N, 1>, float> solveQEF(
        const Eigen::Matrix<float, _pow(R, N), N + 1>&,
        const Eigen::Matrix<float, _pow(R, N), 1>&,
        const Region<N>&)
{
    return {Eigen::Array<float, N, 1>::Zero(),
            std::numeric_limits<float>::infinity()};
}
};

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
template <unsigned R>
bool XTree<N>::findVertex(Evaluator* eval)
{
    constexpr unsigned num = _pow(R, N);
    static_assert(num < Result::N, "Bad resolution");

    // Pre-compute per-axis grid positions
    Eigen::Array<float, R, N> pts;
    for (unsigned i=0; i < R; ++i)
    {
        const float frac = i / (R - 1.0f);
        pts.row(i) = region.lower * (1 - frac) + region.upper * frac;
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
        if (std::isnan(ds.dx[i]) || std::isnan(ds.dy[i]) ||
            std::isnan(ds.dz[i]))
        {
            derivs << 0, 0, 0;
        }
        A.row(i) << derivs.head<N>().transpose(), -1;

        // Temporary variable for dot product
        Eigen::Matrix<float, 1, N + 1> n;
        n << (positions.row(i) - region.center().transpose()), (ds.v[i] - w0);

        b(i) = A.row(i).dot(n);
    }

    // Solve QEF (least-squares)
    auto sol = A.colPivHouseholderQr().solve(b);

    // Store vertex location
    vert = sol.template head<N>().array() + region.center();

    // If the vertex ended up outside of the cell, then minimize the QEF
    // on each of the cell's boundaries and position the vertex at the best
    // boundary position.
    if (!region.contains(vert))
    {
        auto out = Solver<N, R, (1 << N) - 1>::solveQEF(A, b, region);
        err = out.second;
        vert = out.first;
        return err < max_error;
    }
    else
    {
        err = (A * sol - b).squaredNorm();
        return err < max_error;
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
