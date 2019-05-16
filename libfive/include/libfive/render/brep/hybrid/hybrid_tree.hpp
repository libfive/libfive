/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"

namespace Kernel {

/* Forward declaration */
class XTreeEvaluator;
class Tape;
template <unsigned N> class Region;
template <unsigned N> class HybridNeighbors;

template <unsigned N>
struct HybridLeaf
{
    HybridLeaf();
    void reset();

    Eigen::Matrix<double, N, ipow(3, N)> pos;
    std::array<bool, ipow(3, N)> inside;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template <unsigned N>
class HybridTree : public XTree<N, HybridTree<N>, HybridLeaf<N>>
{
public:
    using Leaf = HybridLeaf<N>;
    using Pool = ObjectPool<HybridTree<N>, HybridLeaf<N>>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit HybridTree();
    explicit HybridTree(HybridTree<N>* parent, unsigned index,
                        const Region<N>& region);
    static std::unique_ptr<HybridTree<N>> empty();

    /*
     *  Populates type, setting corners, manifold, and done if this region is
     *  fully inside or outside the mode.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(XTreeEvaluator* eval,
                                       std::shared_ptr<Tape> tape,
                                       const Region<N>& region,
                                       Pool& object_pool);

    /*
     *  Evaluates a minimum-size octree node.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     */
    void evalLeaf(XTreeEvaluator* eval,
                  std::shared_ptr<Tape> tape,
                  const Region<N>& region,
                  Pool& spare_leafs,
                  const HybridNeighbors<N>& neighbors);

    /*
     *  If all children are present, then collapse cells based on error
     *  metrics (TODO; cell collapsing is not implemented).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(XTreeEvaluator* eval,
                         std::shared_ptr<Tape> tape,
                         const Region<N>& region,
                         Pool& object_pool,
                         double max_err);


    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Releases this tree and any leaf objects to the given object pool
     */
    void releaseTo(Pool& object_pool);
};

extern template class HybridTree<3>;

}   // namespace Kernel
