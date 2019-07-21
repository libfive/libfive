/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <iostream>
#include <Eigen/Eigen>

#include "libfive/render/brep/dc/marching.hpp"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////

/*
 *  Returns a set of all the rigid rotations for a particular dimension
 *  (must be specialized to a particular dimension)
 */
template <unsigned N>
static std::list<Eigen::Matrix<double, N, N>,
                 Eigen::aligned_allocator<Eigen::Matrix<double, N, N>>>
    rigidRotations();

template <>
std::list<Eigen::Matrix<double, 2, 2>,
          Eigen::aligned_allocator<Eigen::Matrix<double, 2, 2>>>
    rigidRotations<2>()
{
    Eigen::Matrix2d r;
    r = Eigen::Rotation2Dd(M_PI/2);
    return {r};
}

template <>
std::list<Eigen::Matrix<double, 3, 3>,
          Eigen::aligned_allocator<Eigen::Matrix<double, 3, 3>>>
    rigidRotations<3>()
{
    Eigen::Matrix3d x, y, z;
    x = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitX());
    y = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitY());
    z = Eigen::AngleAxisd(M_PI/2, Eigen::Vector3d::UnitZ());

    return {x, y, z};
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Load initial cases into a marching squares / cubes table
 *  (must be specialized to a particular dimension)
 */
template <unsigned N>
static void loadCases(typename MarchingTable<N>::VertsToPatches& t);

/*
 *  We use the following vertex numbering scheme:
 *      2-------3
 *      |       |
 *      |       |
 *      0-------1
 *
 *      ^ Y
 *      |
 *      ---> X
 */
template <>
void loadCases<2>(typename MarchingTable<2>::VertsToPatches& t)
{
    // Empty
    // Nothing to do here

    // Single corner
    t[1][0][0] = {0, 1};
    t[1][0][1] = {0, 2};

    // Adjacent corners
    t[3][0][0] = {1, 3};
    t[3][0][1] = {0, 2};

    // Opposite corners
    t[9][0][0] = {0, 1};
    t[9][0][1] = {0, 2};
    t[9][1][0] = {3, 2};
    t[9][1][1] = {3, 1};

    // All but one corner
    t[7][0][0] = {1, 3};
    t[7][0][1] = {2, 3};

    // Filled
    // Nothing to do here
}

/*
 *  Based on Figure 5 in Nielson's Dual Marching Cubes
 *
 *  Vertices are numbered as follows:
 *
 *          6 -------- 7
 *         /          /       Z
 *        / |        / |      ^  _ Y
 *       4----------5  |      | /
 *       |  |       |  |      |/
 *       |  2-------|--3      ---> X
 *       | /        | /
 *       |/         |/
 *       0----------1
 */
template <>
void loadCases<3>(typename MarchingTable<3>::VertsToPatches& t)
{
    unsigned bitmap;
    unsigned patch;
    unsigned edge;

    auto begin = [&](std::set<unsigned> verts)
    {
        bitmap = 0;
        for (auto& i : verts)
        {
            bitmap |= 1 << i;
        }
        assert(bitmap > 0 && bitmap < t.size());
        patch = 0;
        edge = 0;
    };

    auto push = [&](MarchingTable<3>::Edge e)
    {
        assert(patch < t[bitmap].size());
        assert(edge < t[bitmap][patch].size());
        t[bitmap][patch][edge++] = e;
    };
    auto next = [&]()
    {
        patch++;
        edge = 0;
    };

    // Case 0 (no vertices set)
    // (Nothing to do here)

    // Case 1
    begin({0});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    // Case 2
    begin({0, 1});
    push({1, 3});
    push({0, 2});
    push({0, 4});
    push({1, 5});

    // Case 3
    begin({0, 5});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    next();
    push({5, 7});
    push({5, 4});
    push({5, 1});

    // Case 4
    begin({0, 7});
    push({0, 1});
    push({0, 2});
    push({0, 4});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 5
    begin({1, 2, 3});
    push({1, 0});
    push({1, 5});
    push({3, 7});
    push({2, 6});
    push({2, 0});

    // Case 6
    begin({0, 1, 7});
    push({1, 3});
    push({0, 2});
    push({0, 4});
    push({1, 5});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 7
    begin({1, 4, 7});
    push({1, 3});
    push({1, 0});
    push({1, 5});

    next();
    push({4, 6});
    push({4, 5});
    push({4, 0});

    next();
    push({7, 6});
    push({7, 3});
    push({7, 5});

    // Case 8
    begin({0, 1, 2, 3});
    push({0, 4});
    push({1, 5});
    push({3, 7});
    push({2, 6});

    // Case 9
    begin({0, 2, 3, 6});
    push({0, 1});
    push({3, 1});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({0, 4});

    // Case 10
    begin({0, 4, 3, 7});
    push({0, 1});
    push({0, 2});
    push({4, 6});
    push({4, 5});

    next();
    push({3, 2});
    push({3, 1});
    push({7, 5});
    push({7, 6});

    // Case 11
    begin({0, 2, 3, 7});
    push({0, 1});
    push({3, 1});
    push({7, 5});
    push({7, 6});
    push({2, 6});
    push({0, 4});

    // Case 12
    begin({1, 2, 3, 4});
    push({4, 6});
    push({4, 5});
    push({4, 0});

    next();
    push({1, 0});
    push({1, 5});
    push({3, 7});
    push({2, 6});
    push({2, 0});

    // Case 13
    begin({0, 3, 6, 5});
    push({0, 2});
    push({0, 4});
    push({0, 1});

    next();
    push({3, 7});
    push({3, 2});
    push({3, 1});

    next();
    push({6, 4});
    push({6, 2});
    push({6, 7});

    next();
    push({5, 7});
    push({5, 1});
    push({5, 4});

    // Case 14
    begin({1, 3, 2, 6});
    push({1, 5});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({2, 0});
    push({1, 0});

    // Case 15
    begin({0, 2, 3,5, 6});
    push({0, 1});
    push({3, 1});
    push({3, 7});
    push({6, 7});
    push({6, 4});
    push({0, 4});

    next();
    push({5, 7});
    push({5, 1});
    push({5, 4});

    // Case 16
    begin({2, 3, 4, 5, 6});
    push({3,1});
    push({3,7});
    push({6, 7});
    push({5, 7});
    push({5, 1});
    push({4, 0});
    push({2, 0});

    // Case 17
    begin({0, 4, 5, 6, 7});
    push({0, 2});
    push({6, 2});
    push({7, 3});
    push({5, 1});
    push({0, 1});

    // Case 18
    begin({1, 2, 3, 4, 5, 6});
    push({1, 0});
    push({4, 0});
    push({2, 0});

    next();
    push({6, 7});
    push({5, 7});
    push({3, 7});

    // Case 19
    begin({1, 2, 3, 4, 6, 7});
    push({1, 0});
    push({1, 5});
    push({7, 5});
    push({4, 5});
    push({4, 0});
    push({2, 0});

    // Case 20
    begin({2, 3, 4, 5, 6, 7});
    push({4, 0});
    push({5, 1});
    push({3, 1});
    push({2, 0});

    // Case 21
    begin({1,2,3,4,5,6,7});
    push({1, 0});
    push({4, 0});
    push({2, 0});

    // Case 22
    // (nothing to do here)
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Applies a rigid-body rotation to a single vertex id
 *  returning a new vertex (by id)
 */
template <unsigned N>
static int rotateVertex(int vert, const Eigen::Matrix<double, N, N>& rot)
{
    assert(vert < _verts(N));

    // Unpack the bitmask into a vector
    Eigen::Matrix<double, N, 1> v;
    for (unsigned i=0; i < N; ++i)
    {
        v(i) = (vert & (1 << i)) ? 1 : -1;
    }

    Eigen::Matrix<double, N, 1> v_ = rot * v;
    int vert_ = 0;
    for (unsigned i=0; i < N; ++i)
    {
        vert_ |= (v_(i) > 0) << i;
    }
    return vert_;
}

/*
 *  Applies a rigid-body rotation to a bitmasked set of vertices
 *  returning a new bitmasked set of vertices.
 */
template <unsigned N>
static unsigned rotateMask(unsigned mask, const Eigen::Matrix<double, N, N>& rot)
{
    assert(mask < ipow(2, _verts(N)));

    unsigned mask_ = 0;
    for (unsigned i=0; i < ipow(2, N); ++i)
    {
        if (mask & (1 << i))
        {
            mask_ |= 1 << rotateVertex<N>(i, rot);
        }
    }
    return mask_;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
MarchingTable<N>::MarchingTable()
{
    // Mark every case as uninitialized
    for (auto& t : v_data)
    {
        for (auto& p : t)
        {
            std::fill(p.begin(), p.end(), std::make_pair(-1, -1));
        }
    }

    // Load the initial set of cases (specialized on a per-dimension basis)
    loadCases<N>(v_data);

    //  Load all possible rigid-body rotations, which we will use to populate
    //  the rest of the table
    auto rots = rigidRotations<N>();

    //  Start by marking the changed elements on the table
    std::array<bool, ipow(2, _verts(N))> changed;
    for (unsigned i=0; i < v_data.size(); ++i)
    {
        changed[i] = v_data[i][0][0].first != -1;
    }

    // Loop until the system stabilizes
    bool any_changed = true;
    while (any_changed)
    {
        any_changed = false;
        for (unsigned i=0; i < v_data.size(); ++i)
        {
            // If this vertex bitmask has changed in the previous cycle,
            // then apply every possible rotation to fill out the table.
            if (changed[i])
            {
                changed[i] = false;
                for (const auto& rot : rots)
                {
                    const Patches& patches = v_data[i];
                    auto i_ = rotateMask<N>(i, rot);
                    Patches& target = v_data[i_];

                    // If this new target is uninitialized, then populate it
                    // by applying the rigid rotation to all the patch edges
                    if (target[0][0].first == -1)
                    {
                        changed[i_] = true;
                        any_changed = true;

                        // Iterate over patches
                        for (unsigned p=0; p < patches.size() &&
                                           patches[p][0].first != -1; ++p)
                        {
                            // Iterate over patch edges
                            for (unsigned e=0; e < patches[p].size() &&
                                               patches[p][e].first != -1; ++e)
                            {
                                target[p][e] = {
                                    rotateVertex<N>(patches[p][e].first, rot),
                                    rotateVertex<N>(patches[p][e].second, rot)
                                };
                            }
                        }
                    }
                }
            }
        }
    }

    // Mark every vertex-pair-to-edge and edge-to-patch mapping as invalid
    for (unsigned i=0; i < e_data.size(); ++i)
    {
        std::fill(e_data[i].begin(), e_data[i].end(), -1);
    }
    for (unsigned i=0; i < p_data.size(); ++i)
    {
        std::fill(p_data[i].begin(), p_data[i].end(), -1);
    }

    // Assign every vertex pair in the table to an edge id
    unsigned j=0;
    for (unsigned i=0; i < v_data.size(); ++i)
    {
        for (unsigned p=0; p < v_data[i].size() &&
                           v_data[i][p][0].first != -1; ++p)
        {
            for (unsigned e=0; e < v_data[i][p].size() &&
                               v_data[i][p][e].first != -1; ++e)
            {
                // Store verts-to-edge mapping if necessary
                auto verts = v_data[i][p][e];
                assert(verts.first != verts.second);

                auto& edge = e_data[verts.first][verts.second];
                if (edge == -1)
                {
                    edge = j++;
                }

                // Store edge-to-patch mapping
                p_data[i][edge] = p;
            }
        }
    }
    assert(j == 2*_edges(N));
    assert(v_data[0][0][0].first == -1);
}

template <unsigned N>
const typename MarchingTable<N>::Patches& MarchingTable<N>::v(CornerIndex i)
{
    return instance().v_data[i.i];
}

template <unsigned N>
const std::array<int, _verts(N)>& MarchingTable<N>::e(CornerIndex i)
{
    return instance().e_data[i.i];
}

template <unsigned N>
const std::array<int, _edges(N) * 2>& MarchingTable<N>::p(CornerIndex i)
{
    return instance().p_data[i.i];
}

template <unsigned N>
const MarchingTable<N>& MarchingTable<N>::instance()
{
    static MarchingTable<N> singleton;
    return singleton;
}

// Explicit initialization of template
template class MarchingTable<2>;
template class MarchingTable<3>;

}   // namespace libfive
