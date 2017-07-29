#include <future>
#include <numeric>
#include <functional>
#include <limits>

#include <Eigen/StdVector>

#include "ao/render/brep/xtree.hpp"
#include "ao/render/axes.hpp"

namespace Kernel {

//  Here's our cutoff value (with a value set in the header)
template <unsigned N> constexpr double XTree<N>::EIGENVALUE_CUTOFF;

//  Used for compile-time checking of array bounds in vertex finding
constexpr static unsigned _pow(unsigned x, unsigned y)
{ return y ? x * _pow(x, y - 1) : 1; }

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
std::unique_ptr<const XTree<N>> XTree<N>::build(
        Tree t, Region<N> region, double min_feature, bool multithread)
{
    XTree<N>* out = nullptr;
    if (multithread)
    {
        std::vector<Evaluator, Eigen::aligned_allocator<Evaluator>> es;
        es.reserve(1 << N);
        for (unsigned i=0; i < (1 << N); ++i)
        {
            es.emplace_back(Evaluator(t));
        }
        out = new XTree(es.data(), region, min_feature, false);
    }
    else
    {
        Evaluator e(t);
        out = new XTree(&e, region, min_feature, false);
    }
    return std::unique_ptr<const XTree<N>>(out);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
XTree<N>::XTree(Evaluator* eval, Region<N> region,
                double min_feature, bool multithread)
    : region(region)
{
    // Do a preliminary evaluation to prune the tree
    auto i = eval->eval(region.lower3().template cast<float>(),
                        region.upper3().template cast<float>());

    eval->push();
    if (Interval::isFilled(i))
    {
        type = Interval::FILLED;
    }
    else if (Interval::isEmpty(i))
    {
        type = Interval::EMPTY;
    }
    // If the cell wasn't empty or filled, attempt to subdivide and recurse
    else
    {
        bool all_empty = true;
        bool all_full  = true;

        // Recurse until volume is too small
        if (((region.upper - region.lower) > min_feature).any())
        {
            auto rs = region.subdivide();

            if (multithread)
            {
                // Evaluate every child in a separate thread
                std::array<std::future<XTree<N>*>, 1 << N> futures;

                assert(children.size() == futures.size());

                for (unsigned i=0; i < children.size(); ++i)
                {
                    futures[i] = std::async(std::launch::async,
                        [&eval, &rs, i, min_feature]()
                        { return new XTree(eval + i, rs[i],
                                           min_feature, false); });
                }
                for (unsigned i=0; i < children.size(); ++i)
                {
                    children[i].reset(futures[i].get());
                }
            }
            // Single-threaded recursive construction
            else
            {
                for (uint8_t i=0; i < children.size(); ++i)
                {
                    // Populate child recursively
                    children[i].reset(new XTree<N>(
                                eval, rs[i], min_feature, false));
                }
            }
            // Update corner and filled / empty state from children
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Grab corner values from children
                corners[i] = children[i]->corners[i];

                all_empty &= children[i]->type == Interval::EMPTY;
                all_full  &= children[i]->type == Interval::FILLED;
            }
        }
        // Terminate recursion here
        else
        {
            // Pack corners into evaluator
            std::array<Eigen::Vector3f, 1 << N> pos;
            for (uint8_t i=0; i < children.size(); ++i)
            {
                pos[i] << cornerPos(i).template cast<float>(),
                          region.perp.template cast<float>();
                eval->set(pos[i], i);
            }

            // Evaluate the region's corners and unpack from evaluator
            std::array<float, 1 << N> fs;
            assert(fs.size() == children.size());
            std::copy_n(eval->values(children.size()), fs.size(), fs.begin());
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Handle inside, outside, and on-boundary crossings
                if (fs[i] < 0)      { corners[i] = Interval::FILLED; }
                else if (fs[i] > 0) { corners[i] = Interval::EMPTY; }
                else
                {
                    corners[i] = eval->isInside(pos[i])
                        ? Interval::FILLED : Interval::EMPTY;
                }

                all_full  &=  corners[i];
                all_empty &= !corners[i];
            }
        }
        type = all_empty ? Interval::EMPTY
             : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;
    }
    eval->pop();

    // If this cell is unambiguous, then fill its corners with values and
    // forget all its branches; these may be no-ops, but they're idempotent
    if (type == Interval::FILLED || type == Interval::EMPTY)
    {
        std::fill(corners.begin(), corners.end(), type);
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
        manifold = true;
    }

    // Branch checking and simplifications
    if (isBranch())
    {
        // Store this tree's depth as a function of its children
        level = std::accumulate(children.begin(), children.end(), (unsigned)0,
            [](const unsigned& a, const std::unique_ptr<const XTree<N>>& b)
            { return std::max(a, b->level);} ) + 1;

        // If all children are non-branches, then we could collapse
        if (std::all_of(children.begin(), children.end(),
                        [](const std::unique_ptr<const XTree<N>>& o)
                        { return !o->isBranch(); }))
        {
            //  This conditional implements the three checks described in
            //  [Ju et al, 2002] in the section titled
            //      "Simplification with topology safety"
            manifold = cornersAreManifold() &&
                std::all_of(children.begin(), children.end(),
                        [](const std::unique_ptr<const XTree<N>>& o)
                        { return o->manifold; }) &&
                leafsAreManifold();

            // Attempt to collapse this tree by positioning the vertex
            // in the summed QEF and checking to see if the error is small
            if (manifold)
            {
                // Populate the feature rank as the maximum of all children
                // feature ranks (as seen in DC: The Secret Sauce)
                rank = std::accumulate(
                        children.begin(), children.end(), (unsigned)0,
                        [](unsigned a, const std::unique_ptr<const XTree<N>>& b)
                            { return std::max(a, b->rank);} );

                // Accumulate the mass point and QEF matrices
                for (const auto& c : children)
                {
                    if (c->rank == rank)
                    {
                        _mass_point += c->_mass_point;
                    }
                    AtA += c->AtA;
                    AtB += c->AtB;
                    BtB += c->BtB;
                }
                assert(region.contains(massPoint()));

                // If the vertex error is below a threshold, and the vertex
                // is well-placed in the distance field, then convert into
                // a leaf by erasing all of the child branches
                if (findVertex() < 1e-8 &&
                    fabs(eval->eval(vert3().template cast<float>())) < 1e-8)
                {

                    std::for_each(children.begin(), children.end(),
                        [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
                }
            }
        }
    }
    else if (type == Interval::AMBIGUOUS)
    {
        // Figure out if the leaf is manifold
        manifold = cornersAreManifold();

        // Populate mass point here, as we use it in both the non-manifold
        // case (where it becomes the cell's vertex) and the manifold case
        // (where we minimize the QEF towards it)
        for (auto e : edges())
        {
            if (cornerState(e.first) != cornerState(e.second))
            {
                auto inside = (cornerState(e.first) == Interval::FILLED)
                    ? cornerPos(e.first) : cornerPos(e.second);
                auto outside = (cornerState(e.first) == Interval::FILLED)
                    ? cornerPos(e.second) : cornerPos(e.first);

                // We do an N-fold reduction at each stage
                constexpr int SEARCH_COUNT = 4;
                constexpr int POINTS_PER_SEARCH = 16;

                // Binary search for intersection
                for (int i=0; i < SEARCH_COUNT; ++i)
                {
                    // Load search points into evaluator
                    Eigen::Array<double, N, 1> ps[POINTS_PER_SEARCH];
                    for (int j=0; j < POINTS_PER_SEARCH; ++j)
                    {
                        double frac = j / (POINTS_PER_SEARCH - 1.0);
                        ps[j] = (inside * (1 - frac)) + (outside * frac);
                        Eigen::Vector3d pos;
                        pos << ps[j], region.perp;
                        eval->setRaw(pos.template cast<float>(), j);
                    }

                    // Evaluate, then search for the first outside point
                    // and adjust inside / outside to their new positions
                    //
                    // We copy to a temporary array here to avoid invalidating
                    // out if we need to call eval->isInside (e.g. when out[i]
                    // is exactly 0 so we're not sure about the boundary)
                    float out[POINTS_PER_SEARCH];
                    std::copy_n(eval->values(POINTS_PER_SEARCH),
                                POINTS_PER_SEARCH, out);
                    for (int j=0; j < POINTS_PER_SEARCH; ++j)
                    {
                        if (out[j] > 0)
                        {
                            inside = ps[j - 1];
                            outside = ps[j];
                            break;
                        }
                        else if (out[j] == 0)
                        {
                            Eigen::Vector3d pos;
                            pos << ps[j], region.perp;
                            if (!eval->isInside(pos.template cast<float>()))
                            {
                                inside = ps[j - 1];
                                outside = ps[j];
                                break;
                            }
                        }
                    }
                }

                // Accumulate this intersection in the mass point
                Eigen::Matrix<double, N + 1, 1> mp;
                mp << inside, 1;
                _mass_point += mp;
            }
        }
        assert(region.contains(massPoint()));

        // If this leaf cell is manifold, then find its vertex
        // Here, we diverge from standard DC, using the sampling strategy
        // from DMC (with regularly spaced samples on a grid), then solving
        // for the constrained minimizer with w = 0 (as described in the
        // "sliver elimination" section of the DMC paper).
        if (manifold)
        {
            constexpr unsigned R = 4;
            constexpr unsigned num = _pow(R, N);
            static_assert(num < Result::N, "Bad resolution");

            // Pre-compute per-axis grid positions
            Eigen::Array<double, R, N> pts;
            for (unsigned i=0; i < R; ++i)
            {
                const double frac = i / (R - 1.0f);
                pts.row(i) = region.lower.template cast<double>() * (1 - frac) +
                             region.upper.template cast<double>() * frac;
            }

            // Load all sample points into the evaluator
            Eigen::Array<double, num, N> positions;
            for (unsigned i=0; i < num; ++i)
            {
                // Unpack from grid positions into the position vector
                for (unsigned j=0; j < N; ++j)
                {
                    positions(i, j) = pts((i % _pow(R, j + 1)) / _pow(R, j), j);
                }

                // The evaluator works in 3-space,
                // regardless of the XTree's dimensionality
                Eigen::Vector3d pos;
                pos << positions.row(i).transpose(), region.perp;
                eval->set(pos.template cast<float>(), i);
            }

            // Get derivatives!
            auto ds = eval->derivs(num);

            //  The A matrix is of the form
            //  [n1x, n1y, n1z]
            //  [n2x, n2y, n2z]
            //  [n3x, n3y, n3z]
            //  ...
            //  (with one row for each sampled point's normal)
            Eigen::Matrix<double, num, N> A;

            //  The b matrix is of the form
            //  [p1 . n1 - w1]
            //  [p2 . n2 - w2]
            //  [p3 . n3 - w3]
            //  ...
            //  (with one row for each sampled point)
            Eigen::Matrix<double, num, 1> b;

            // Load samples into the QEF arrays
            for (unsigned i=0; i < num; ++i)
            {
                // Load this row of A matrix, with a special case for
                // situations with NaN derivatives
                auto derivs = Eigen::Array3d(ds.dx[i], ds.dy[i], ds.dz[i]);
                auto w = ds.v[i];
                if (derivs.array().isNaN().any())
                {
                    derivs << 0, 0, 0;
                }
                else
                {
                    auto norm = derivs.matrix().norm();
                    w /= norm;
                    derivs /= norm;
                }
                A.row(i) << derivs.head<N>().transpose();
                b(i) = A.row(i).dot(positions.row(i).matrix()) - w;
            }

            // Save compact QEF matrices
            auto At = A.transpose();
            AtA = At * A;
            AtB = At * b;
            BtB = b.transpose() * b;

            // Use eigenvalues to find rank, then re-use the solver
            // to find vertex position
            Eigen::EigenSolver<Eigen::Matrix<double, N, N>> es(AtA);
            auto eigenvalues = es.eigenvalues().real();

            // Count non-singular Eigenvalues to determine rank
            rank = (eigenvalues.array().abs() >= EIGENVALUE_CUTOFF).count();

            // Re-use the solver to find the vertex position, ignoring the
            // error result (because this is the bottom of the recursion)
            findVertex(es);
        }
        else
        {
            // For non-manifold leaf nodes, put the vertex at the mass point.
            // As described in "Dual Contouring: The Secret Sauce", this improves
            // mesh quality.
            vert = massPoint();
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
uint8_t XTree<N>::cornerMask() const
{
    uint8_t mask = 0;
    for (unsigned i=0; i < children.size(); ++i)
    {
        if (cornerState(i) == Interval::FILLED)
        {
            mask |= 1 << i;
        }
    }
    return mask;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
double XTree<N>::findVertex()
{
    Eigen::EigenSolver<Eigen::Matrix<double, N, N>> es(AtA);
    return findVertex(es);
}

template <unsigned N>
double XTree<N>::findVertex(
        Eigen::EigenSolver<Eigen::Matrix<double, N, N>>& es)
{
    assert(_mass_point(N) > 0);

    // We need to find the pseudo-inverse of AtA.
    auto eigenvalues = es.eigenvalues().real();

    // Truncate near-singular eigenvalues in the SVD's diagonal matrix
    Eigen::Matrix<double, N, N> D = Eigen::Matrix<double, N, N>::Zero();
    for (unsigned i=0; i < N; ++i)
    {
        D.diagonal()[i] = (std::abs(eigenvalues[i]) < EIGENVALUE_CUTOFF)
            ? 0 : (1 / eigenvalues[i]);
    }

    // Sanity-checking that rank matches eigenvalue count
    if (!isBranch())
    {
        assert(D.diagonal().count() == rank);
    }

    // SVD matrices
    auto U = es.eigenvectors().real(); // = V

    // Pseudo-inverse of A
    auto AtAp = U * D * U.transpose();

    // Solve for vertex (minimizing distance to center)
    auto center = massPoint();
    vert = AtAp * (AtB - (AtA * center)) + center;

    // Return the QEF error
    return (vert.matrix().transpose() * AtA * vert.matrix() - 2*vert.matrix().transpose() * AtB)[0] + BtB;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Eigen::Vector3d XTree<N>::vert3() const
{
    Eigen::Vector3d out;
    out << vert, region.perp.template cast<double>();
    return out;
}

template <unsigned N>
Eigen::Matrix<double, N, 1> XTree<N>::massPoint() const
{
    return _mass_point.template head<N>() / _mass_point(N);
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
template <>
bool XTree<2>::leafsAreManifold() const
{
    /*  See detailed comment in Octree::leafTopology */
    const bool edges_safe =
        (child(0)->cornerState(Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::X) == cornerState(Axis::X))
    &&  (child(0)->cornerState(Axis::Y) == cornerState(0) ||
         child(0)->cornerState(Axis::Y) == cornerState(Axis::Y))
    &&  (child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X) ||
         child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X|Axis::Y))
    &&  (child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X));

    const bool faces_safe =
        (child(0)->cornerState(Axis::Y|Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::X) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X));

    return edges_safe && faces_safe;
}

template <>
bool XTree<2>::cornersAreManifold() const
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[cornerMask()];
}

template <>
const std::vector<std::pair<uint8_t, uint8_t>>& XTree<2>::edges() const
{
    static const std::vector<std::pair<uint8_t, uint8_t>> es =
        {{0, Axis::X}, {0, Axis::Y},
         {Axis::X, Axis::X|Axis::Y}, {Axis::Y, Axis::Y|Axis::X}};
    return es;
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for octree
template <>
bool XTree<3>::leafsAreManifold() const
{
    /*  - The sign in the middle of a coarse edge must agree with the sign of at
     *    least one of the edge’s two endpoints.
     *  - The sign in the middle of a coarse face must agree with the sign of at
     *    least one of the face’s four corners.
     *  - The sign in the middle of a coarse cube must agree with the sign of at
     *    least one of the cube’s eight corners.
     *  [Ju et al, 2002]    */

    // Check the signs in the middle of leaf cell edges
    const bool edges_safe =
        (child(0)->cornerState(Axis::Z) == cornerState(0) ||
         child(0)->cornerState(Axis::Z) == cornerState(Axis::Z))
    &&  (child(0)->cornerState(Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::X) == cornerState(Axis::X))
    &&  (child(0)->cornerState(Axis::Y) == cornerState(0) ||
         child(0)->cornerState(Axis::Y) == cornerState(Axis::Y))

    &&  (child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X) ||
         child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X|Axis::Y))
    &&  (child(Axis::X)->cornerState(Axis::X|Axis::Z) == cornerState(Axis::X) ||
         child(Axis::X)->cornerState(Axis::X|Axis::Z) == cornerState(Axis::X|Axis::Z))

    &&  (child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X))
    &&  (child(Axis::Y)->cornerState(Axis::Y|Axis::Z) == cornerState(Axis::Y) ||
         child(Axis::Y)->cornerState(Axis::Y|Axis::Z) == cornerState(Axis::Y|Axis::Z))

    &&  (child(Axis::X|Axis::Y)->cornerState(Axis::X|Axis::Y|Axis::Z) ==
                               cornerState(Axis::X|Axis::Y) ||
         child(Axis::X|Axis::Y)->cornerState(Axis::X|Axis::Y|Axis::Z) ==
                               cornerState(Axis::X|Axis::Y|Axis::Z))

    &&  (child(Axis::Z)->cornerState(Axis::Z|Axis::X) == cornerState(Axis::Z) ||
         child(Axis::Z)->cornerState(Axis::Z|Axis::X) == cornerState(Axis::Z|Axis::X))
    &&  (child(Axis::Z)->cornerState(Axis::Z|Axis::Y) == cornerState(Axis::Z) ||
         child(Axis::Z)->cornerState(Axis::Z|Axis::Y) == cornerState(Axis::Z|Axis::Y))

    &&  (child(Axis::Z|Axis::X)->cornerState(Axis::Z|Axis::X|Axis::Y) ==
                               cornerState(Axis::Z|Axis::X) ||
         child(Axis::Z|Axis::X)->cornerState(Axis::Z|Axis::X|Axis::Y) ==
                               cornerState(Axis::Z|Axis::X|Axis::Y))

    &&  (child(Axis::Z|Axis::Y)->cornerState(Axis::Z|Axis::Y|Axis::X) ==
                               cornerState(Axis::Z|Axis::Y) ||
         child(Axis::Z|Axis::Y)->cornerState(Axis::Z|Axis::Y|Axis::X) ==
                               cornerState(Axis::Z|Axis::Y|Axis::X));

    const bool faces_safe =
        (child(0)->cornerState(Axis::X|Axis::Z) == cornerState(0) ||
         child(0)->cornerState(Axis::X|Axis::Z) == cornerState(Axis::X) ||
         child(0)->cornerState(Axis::X|Axis::Z) == cornerState(Axis::Z) ||
         child(0)->cornerState(Axis::X|Axis::Z) == cornerState(Axis::X|Axis::Z))
    &&  (child(0)->cornerState(Axis::Y|Axis::Z) == cornerState(0) ||
         child(0)->cornerState(Axis::Y|Axis::Z) == cornerState(Axis::Y) ||
         child(0)->cornerState(Axis::Y|Axis::Z) == cornerState(Axis::Z) ||
         child(0)->cornerState(Axis::Y|Axis::Z) == cornerState(Axis::Y|Axis::Z))
    &&  (child(0)->cornerState(Axis::Y|Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::X) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X))

    && (child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::X) == cornerState(Axis::X) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::X) == cornerState(Axis::X|Axis::Z) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::X) == cornerState(Axis::X|Axis::Y) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::X) ==
                                     cornerState(Axis::X|Axis::Y|Axis::Z))
    && (child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Y) == cornerState(Axis::Y) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Y) == cornerState(Axis::Y|Axis::Z) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Y) == cornerState(Axis::Y|Axis::X) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Y) ==
                                     cornerState(Axis::Y|Axis::Z|Axis::X))
    && (child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Z) == cornerState(Axis::Z) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Z) == cornerState(Axis::Z|Axis::Y) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Z) == cornerState(Axis::Z|Axis::X) ||
        child(Axis::X|Axis::Y|Axis::Z)->cornerState(Axis::Z) ==
                                     cornerState(Axis::Z|Axis::Y|Axis::X));

    const bool center_safe =
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(0) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::X) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::Y) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::X|Axis::Y) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::Z) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::Z|Axis::X) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::Z|Axis::Y) ||
        child(0)->cornerState(Axis::X|Axis::Y|Axis::Z) == cornerState(Axis::Z|Axis::X|Axis::Y);

    return edges_safe && faces_safe && center_safe;
}

template <>
bool XTree<3>::cornersAreManifold() const
{
    /* The code to generate the table is given below:
    def safe(index):
        f = [(index & (1 << i)) != 0 for i in range(8)]
        edges = [(0,1), (0,2), (2,3), (1,3),
                 (4,5), (4,6), (6,7), (5,7),
                 (0,4), (2,6), (1,5), (3,7)]
        def merge(a, b):
            merged = [(e[0] if e[0] != a else b,
                       e[1] if e[1] != a else b) for e in edges]
            return [e for e in merged if e[0] != e[1]]
        while True:
            for e in edges:
                if f[e[0]] == f[e[1]]:
                    edges = merge(e[0], e[1])
                    break
            else:
                break
        s = set(map(lambda t: tuple(sorted(t)),edges))
        return len(s) <= 1
    out = ""
    for i,s in enumerate([safe(i) for i in range(256)]):
        if out == "": out += "{"
        else: out += ","
        if i and i % 32 == 0:
            out += '\n '
        if s: out += "1"
        else: out += "0"
    out += "}"
    print(out)
    */
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,
         1,0,1,1,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,1,
         1,0,0,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,
         1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[cornerMask()];
}

template <>
const std::vector<std::pair<uint8_t, uint8_t>>& XTree<3>::edges() const
{
    static const std::vector<std::pair<uint8_t, uint8_t>> es =
        {{0, Axis::X}, {0, Axis::Y}, {0, Axis::Z},
         {Axis::X, Axis::X|Axis::Y}, {Axis::X, Axis::X|Axis::Z},
         {Axis::Y, Axis::Y|Axis::X}, {Axis::Y, Axis::Y|Axis::Z},
         {Axis::X|Axis::Y, Axis::X|Axis::Y|Axis::Z},
         {Axis::Z, Axis::Z|Axis::X}, {Axis::Z, Axis::Z|Axis::Y},
         {Axis::Z|Axis::X, Axis::Z|Axis::X|Axis::Y},
         {Axis::Z|Axis::Y, Axis::Z|Axis::Y|Axis::X}};
    return es;
}

////////////////////////////////////////////////////////////////////////////////

// Explicit initialization of templates
template class XTree<2>;
template class XTree<3>;

}   // namespace Kernel
