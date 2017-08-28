#pragma once
#include <iostream>

#include <array>
#include <cstdint>
#include <Eigen/Eigen>

#include "ao/render/brep/region.hpp"
#include "ao/render/brep/marching.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/eval/interval.hpp"

namespace Kernel {

template <unsigned N>
class XTree
{
public:
    /*
     *  Constructs an octree or quadtree by subdividing a region
     *  (unstoppable)
     */
    static std::unique_ptr<const XTree> build(
            Tree t, Region<N> region, double min_feature=0.1,
            double max_err=1e-8, bool multithread=true);

    /*
     *  Fully-specified XTree builder (stoppable through cancel)
     */
    static std::unique_ptr<const XTree> build(
            Tree t, Region<N> region, double min_feature,
            double max_err, bool multithread,
            std::atomic_bool& cancel);

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0].get() != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const XTree<N>* child(unsigned i) const
    { return isBranch() ? children[i].get() : this; }

    /*
     *  Returns the filled / empty state for the ith corner
     */
    Interval::State cornerState(uint8_t i) const { return corners[i]; }

    /*
     *  Returns the corner position for the ith corner
     */
    Eigen::Array<double, N, 1> cornerPos(uint8_t i) const
    {
        Eigen::Array<double, N, 1> out;
        for (unsigned axis=0; axis < N; ++axis)
        {
            out(axis) = (i & (1 << axis)) ? region.upper(axis)
                                          : region.lower(axis);
        }
        return out;
    }

    /*
     *  Returns the averaged mass point
     */
    Eigen::Matrix<double, N, 1> massPoint() const;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Unpack the vertex into a 3-element array
     *  (using the region's perpendicular coordinates)
     */
    Eigen::Vector3d vert3(unsigned index=0) const;

    /*  The region filled by this XTree */
    const Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<const XTree<N>>, 1 << N> children;

    /*  level = max(map(level, children)) + 1  */
    unsigned level=0;

    /*  Vertex locations, if this is a leaf
     *
     *  To make cells manifold, we may store multiple vertices in a single
     *  leaf; see writeup in marching.cpp for details  */
    Eigen::Matrix<double, N, _pow(2, N - 1)> verts;

    /*
     *  Look up a particular vertex by index
     */
    Eigen::Matrix<double, N, 1> vert(unsigned i=0) const
    { assert(i < vertex_count); return verts.col(i); }

    /*  Array of filled states for the cell's corners
     *  (must only be FILLEd / EMPTY, not UNKNOWN or AMBIGUOUS ) */
    std::array<Interval::State, 1 << N> corners;

    /*  Leaf cell state, when known  */
    Interval::State type=Interval::UNKNOWN;

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        *
     *                                                               *
     *  This value is populated in find{Leaf|Branch}Matrices and     *
     *  used when merging intersections from lower-ranked children   */
    unsigned rank=0;

    /* Used as a unique per-vertex index when unpacking into a b-rep;   *
     * this is cheaper than storing a map of XTree* -> uint32_t         */
    mutable std::array<uint32_t, _pow(2, N - 1)> index;

    /*  Bitfield marking which corners are set */
    uint8_t corner_mask=0;

    /*  Stores the number of patches / vertices in this cell
     *  (which could be more than one to keep the surface manifold */
    unsigned vertex_count=0;

    /*  Marks whether this cell is manifold or not  */
    bool manifold=false;

    /*  Single copy of the marching squares / cubes table, lazily
     *  initialized when needed */
    static std::unique_ptr<const Marching::MarchingTable<N>> mt;

protected:
    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*
     *  Private constructor for XTree
     *
     *  If multiple evaluators are provided, then tree construction will
     *  be distributed across multiple threads.
     */
    XTree(Evaluator* eval, Region<N> region,
          double min_feature, double max_err, bool multithread,
          std::atomic_bool& cancel);

    /*
     *  Searches for a vertex within the XTree cell, using the QEF matrices
     *  that are pre-populated in AtA, AtB, etc.
     *
     *  Minimizes the QEF towards mass_point
     *
     *  Stores the vertex in vert and returns the QEF error
     */
    double findVertex(unsigned i=0);

    /*
     *  Returns edges (as indices into corners)
     *  (must be specialized for a specific dimensionality)
     */
    const std::vector<std::pair<uint8_t, uint8_t>>& edges() const;

    /*
     *  Returns a table such that looking up a particular corner
     *  configuration returns whether that configuration is safe to
     *  collapse.
     *  (must be specialized for a specific dimensionality)
     *
     *  This implements the test from [Gerstner et al, 2000], as
     *  described in [Ju et al, 2002].
     */
    bool cornersAreManifold() const;

    /*
     *  Checks to make sure that the fine contour is topologically equivalent
     *  to the coarser contour by comparing signs in edges and faces
     *  (must be specialized for a specific dimensionality)
     *
     *  Returns true if the cell can be collapsed without changing topology
     *  (with respect to the leaves)
     */
    bool leafsAreManifold() const;

    /*  Mass point is the average intersection location *
     *  (the last coordinate is number of points summed) */
    Eigen::Matrix<double, N + 1, 1> _mass_point=
        Eigen::Matrix<double, N + 1, 1>::Zero();

    /*  QEF matrices */
    Eigen::Matrix<double, N, N> AtA=Eigen::Matrix<double, N, N>::Zero();
    Eigen::Matrix<double, N, 1> AtB=Eigen::Matrix<double, N, 1>::Zero();
    double BtB=0;

    /*  Eigenvalue threshold for determining feature rank  */
    constexpr static double EIGENVALUE_CUTOFF=0.1f;
};

// Explicit template instantiation declarations
template <> bool XTree<2>::cornersAreManifold() const;
template <> bool XTree<3>::cornersAreManifold() const;

template <> bool XTree<2>::leafsAreManifold() const;
template <> bool XTree<3>::leafsAreManifold() const;

template <> const std::vector<std::pair<uint8_t, uint8_t>>& XTree<2>::edges() const;
template <> const std::vector<std::pair<uint8_t, uint8_t>>& XTree<3>::edges() const;

extern template class XTree<2>;
extern template class XTree<3>;

}   // namespace Kernel
