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

//  Allocating static var for marching cubes table
template <unsigned N>
std::unique_ptr<const Marching::MarchingTable<N>> XTree<N>::mt;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
std::unique_ptr<const XTree<N>> XTree<N>::build(
        Tree t, Region<N> region, double min_feature, bool multithread)
{
    // Lazy initialization of marching squares / cubes table
    if (mt.get() == nullptr)
    {
        mt = Marching::buildTable<N>();
    }

    XTree<N>* out = nullptr;
    if (multithread)
    {
        std::vector<Evaluator, Eigen::aligned_allocator<Evaluator>> es;
        es.reserve(1 << N);
        for (unsigned i=0; i < (1 << N); ++i)
        {
            es.emplace_back(Evaluator(t));
        }
        out = new XTree(es.data(), region, min_feature, true);
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

    // If this cell is unambiguous, then fill its corners with values and
    // forget all its branches; these may be no-ops, but they're idempotent
    if (type == Interval::FILLED || type == Interval::EMPTY)
    {
        std::fill(corners.begin(), corners.end(), type);
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
        manifold = true;
    }

    // Build and store the corner mask
    for (unsigned i=0; i < children.size(); ++i)
    {
        corner_mask |= (corners[i] == Interval::FILLED) << i;
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
                if (findVertex(vertex_count++) < 1e-8 &&
                    fabs(eval->baseEval(vert3().template cast<float>())) < 1e-8)
                {
                    std::for_each(children.begin(), children.end(),
                        [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
                }
                else
                {
                    vertex_count = 0;
                }
            }
        }
    }
    else if (type == Interval::AMBIGUOUS)
    {
        // Figure out if the leaf is manifold
        manifold = cornersAreManifold();

        // Here, we'll prepare to store position, {normal, value} pairs
        // for every crossing and feature
        typedef std::pair<Eigen::Matrix<double, N, 1>,
                          Eigen::Matrix<double, N + 1, 1>> Intersection;
        std::vector<Intersection, Eigen::aligned_allocator<Intersection>>
            intersections;

        // RAM is cheap, so reserve a bunch of space here to avoid
        // the need for re-allocating later on
        intersections.reserve(_edges(N) * 2);

        // Iterate over manifold patches for this corner case
        const auto& ps = mt->v[corner_mask];
        while (vertex_count < ps.size() && ps[vertex_count][0].first != -1)
        {
            // Reset mass point and intersections
            _mass_point = _mass_point.Zero();
            intersections.clear();

            // Iterate over edges in this patch
            for (unsigned e=0; e < ps[vertex_count].size() &&
                               ps[vertex_count][e].first != -1; ++e)
            {
                // Sanity-checking
                assert(corners[ps[vertex_count][e].first] == Interval::FILLED);
                assert(corners[ps[vertex_count][e].second] == Interval::EMPTY);

                // Search edge for intersection
                auto inside = cornerPos(ps[vertex_count][e].first);
                auto outside = cornerPos(ps[vertex_count][e].second);

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

                // Here, we'll store position, {normal, value} pairs
                // for every crossing (inside/outside) and feature
                for (const auto& pt : {inside, outside})
                {
                    Eigen::Vector3d pos;
                    pos << pt, region.perp;
                    auto pos_ = pos.template cast<float>();

                    eval->set(pos_, 0);

                    auto saveIntersection = [&]()
                    {
                        const auto ds = eval->derivs(1);

                        // Unpack 3D derivatives into XTree-specific
                        // dimensionality, and find normal.
                        const auto derivs3 = Eigen::Vector3d(
                                ds.dx[0], ds.dy[0], ds.dz[0]);
                        const auto derivs = derivs3.template head<N>();
                        const auto norm = derivs.norm();

                        // Find normalized derivatives and distance value
                        Eigen::Matrix<double, N + 1, 1> dv;
                        dv << derivs / norm, ds.v[0] / norm;
                        if (!dv.array().isNaN().any())
                        {
                            intersections.push_back({pt, dv});
                        }
                    };

                    if (eval->isAmbiguous(pos_))
                    {
                        const auto fs = eval->featuresAt(pos_);
                        for (auto& f : fs)
                        {
                            // Evaluate feature-specific distance and
                            // derivatives value at this particular point
                            eval->push(f);
                            saveIntersection();
                            eval->pop();
                        }
                    }
                    else
                    {
                        // Perform a single evaluation and save the intersection
                        saveIntersection();
                    }
                }
            }

            // Now, we'll unpack into A and b matrices
            //
            //  The A matrix is of the form
            //  [n1x, n1y, n1z]
            //  [n2x, n2y, n2z]
            //  [n3x, n3y, n3z]
            //  ...
            //  (with one row for each sampled point's normal)
            Eigen::Matrix<double, Eigen::Dynamic, N> A(intersections.size(), N);

            //  The b matrix is of the form
            //  [p1 . n1]
            //  [p2 . n2]
            //  [p3 . n3]
            //  ...
            //  (with one row for each sampled point)
            Eigen::Matrix<double, Eigen::Dynamic, 1> b(intersections.size(), 1);

            // Load samples into the QEF arrays
            //
            // Since we're deliberately sampling on either side of the
            // intersection, we subtract out the distance-field value
            // to make the math work out.
            for (unsigned i=0; i < intersections.size(); ++i)
            {
                A.row(i) << intersections[i].second
                                            .template head<N>()
                                            .transpose();
                b(i) = A.row(i).dot(intersections[i].first) -
                       intersections[i].second(N);
            }

            // Save compact QEF matrices
            auto At = A.transpose().eval();
            AtA = At * A;
            AtB = At * b;
            BtB = b.transpose() * b;

            // Find the vertex position, storing into the appropriate column
            // of the vertex array and ignoring the error result (because
            // this is the bottom of the recursion)
            findVertex(vertex_count++);
        }
    }

    // ...and we're done.
    eval->pop();
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
double XTree<N>::findVertex(unsigned index)
{
    Eigen::EigenSolver<Eigen::Matrix<double, N, N>> es(AtA);
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

    // Get rank from eigenvalues
    if (!isBranch())
    {
        assert(index > 0 || rank == 0);
        rank = D.diagonal().count();
    }

    // SVD matrices
    auto U = es.eigenvectors().real().eval(); // = V

    // Pseudo-inverse of A
    auto AtAp = U * D * U.transpose();

    // Solve for vertex position (minimizing distance to center)
    auto center = massPoint();
    Eigen::Matrix<double, N, 1> v = AtAp * (AtB - (AtA * center)) + center;

    // Store this specific vertex in the verts matrix
    verts.col(index) = v;

    // Return the QEF error
    return (v.transpose() * AtA * v - 2*v.transpose() * AtB)[0] + BtB;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Eigen::Vector3d XTree<N>::vert3(unsigned index) const
{
    Eigen::Vector3d out;
    out << vert(index), region.perp.template cast<double>();
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
    return corner_table[corner_mask];
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
    return corner_table[corner_mask];
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
