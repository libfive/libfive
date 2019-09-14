/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <memory>

#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/default_new_delete.hpp"

namespace libfive {

/*  Forward declaration */
class Tape;
class Evaluator;
class VolNeighbors;

/*
 *  A VolTree is a very basic octre that stores filled / empty state of cells.
 *
 *  It is used as an acceleration data structure.  For example, we could
 *  pre-compute a VolTree over a 3D region, then use its data to accelerate
 *  calculations of 2D contours within that region.
 */
class VolTree : public XTree<3, VolTree, void>
{
public:
    using Pool = ObjectPool<VolTree>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit VolTree();
    explicit VolTree(VolTree* parent, unsigned index, const Region<3>& region);
    static std::unique_ptr<VolTree> empty();

    /*
     *  Populates type, setting done if this region is fully inside or outside.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(Evaluator* eval,
                                       const std::shared_ptr<Tape>& tape,
                                       Pool& object_pool);

    void evalLeaf(Evaluator* eval,
                  const std::shared_ptr<Tape>& tape,
                  Pool& spare_leafs,
                  const VolNeighbors& neighbors);

    /*  If all children are EMPTY / FILLED, merges them */
    bool collectChildren(Evaluator* eval,
                         const std::shared_ptr<Tape>& tape,
                         Pool& object_pool,
                         double max_err);

    /*
     *  Releases this tree to the given object pool
     */
    void releaseTo(Pool& object_pool);

    DEFAULT_OPERATORS_NEW_AND_DELETE

    /*  Checks whether the given interval is empty or filled */
    Interval::State check(const Region<3>& r) const;
    Interval::State check(const Region<2>& r) const;

    bool contains(const Region<2>& r) const;
    bool contains(const Region<3>& r) const;

    const VolTree* push(unsigned i, const Region<2>::Perp& perp) const;
    const VolTree* push(unsigned i, const Region<3>::Perp& perp) const;

    static bool hasSingletons() { return false; }
    static VolTree* singletonEmpty() { return nullptr; }
    static VolTree* singletonFilled() { return nullptr; }
    static bool isSingleton(const VolTree*) { return false; }
};

}   // namespace libfive
