/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/render/brep/root.hpp"
#include "libfive/render/brep/multithread_recursive.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/vol/vol_tree.hpp"
#include "libfive/eval/evaluator.hpp"
#include "libfive/render/brep/neighbors.hpp"

namespace libfive {

template <typename T>
Root<T> Root<T>::build(tbb::enumerable_thread_specific<Evaluator>& eval,
                       const Region<T::Dimension>& region_, 
                       const BRepSettings& settings) {
    using Neighbors = typename NeighborHelper<T>::NeighborType;
    if (settings.vol && !settings.vol->contains(region_)) {
        std::cerr << "WorkerPool::build: Invalid region for vol tree\n";
    }
    const auto region = region_.withResolution(settings.min_feature);

    struct Local {
        Evaluator& eval;
        typename T::Pool object_pool;
        EIGEN_MAKE_ALIGNED_OPERATOR_NEW
    };
    tbb::enumerable_thread_specific<Local> locals(
        [&eval]() { return Local{ eval.local() }; });

    struct Node { // Will be Out in multithread_recursive
        T* target;
        std::shared_ptr<Tape> tape; // Needed for post
        std::shared_ptr<Tape> next_tape; // Needed for pre of children
        Neighbors neighbors;
        const VolTree* vol;
    };

    auto pre = [&settings](Region<T::Dimension> region,
        Node* parent, unsigned childNo, bool& recurse, Local& local) {
        assert(region.level >= 0);
        Node out;
        if (parent == nullptr) {
            out.target = new T(nullptr, 0, region);
            out.vol = settings.vol;
            out.tape = local.eval.getDeck()->tape;
        }
        else {
            out.target = local.object_pool.get(parent->target, childNo, region);
            out.vol = parent->vol ? parent->vol->push(childNo, region.perp)
                                  : nullptr;
            out.tape = parent->next_tape;
            ///
            // Find our local neighbors.  We do this at the last minute to
            // give other threads the chance to populate more pointers.
            out.neighbors = parent->neighbors.push(
                out.target->parent_index, parent->target->children);
        }
        const auto& t = out.target; // Aliasing for convenience.

        if (t->region.level > 0) {
            // Even if this tree is larger than the minimum size, we need to
            // recurse only if it is not unambiguously filled or empty.
            if (out.vol) {
                auto i = out.vol->check(t->region);
                if (i == Interval::EMPTY || i == Interval::FILLED) {
                    t->setType(i);
                }
            }
            if (t->type == Interval::UNKNOWN) {
                out.next_tape = 
                    t->evalInterval(&local.eval, out.tape, local.object_pool);
            }
            if (out.next_tape == nullptr) {
                out.next_tape = out.tape;
            }

            assert(t->type != Interval::UNKNOWN);
            recurse = t->type == Interval::AMBIGUOUS && !settings.cancel;
            // If we did an interval evaluation, then we either
            // (a) are done with this tree because it is empty / filled
            // (b) don't do anything until all of its children are done
            //
            // In both cases, we're done with the pre-recursion step except
            // for progress tracking in the former case; the latter case is 
            // handled in "post" below.  (We do also have to return "out".)
            if (!recurse && !settings.cancel && settings.progress_handler)
            {
                // Accumulate all of the child XTree cells that would have been
                // included if we continued to subdivide this tree, then pass
                // all of them to the progress tracker
                uint64_t ticks = 0;
                for (int i = 0; i < t->region.level; ++i) {
                    ticks = (ticks + 1) * (1 << T::Dimension);
                }
                settings.progress_handler->tick(ticks + 1);
            }
        }
        else {
            // We're at a minimum-level region.
            recurse = false;
            if (!settings.cancel) {
                t->evalLeaf(
                    &local.eval, out.tape, local.object_pool, out.neighbors);
                if (settings.progress_handler) {
                    settings.progress_handler->tick(1);
                }
            }
        }
        return out;
    };

    auto post = 
        [&settings](Region<T::Dimension> region, Node& node, Local& local) {
        if (settings.cancel) {
            return;
        }
        node.target->collectChildren(&local.eval, node.tape,
                                     local.object_pool, settings.max_err);
        // Report the volume of completed trees as we walk back
        // up towards the root of the tree.
        if (settings.progress_handler) {
            settings.progress_handler->tick();
        }
    };

    // Kick off the progress tracking thread, based on the number of
    // octree levels and a fixed split per level
    uint64_t ticks = 0;
    for (int i = 0; i < region.level; ++i) {
        ticks = (ticks + 1) * (1 << T::Dimension);
    }
    if (settings.progress_handler) {
        settings.progress_handler->nextPhase(ticks + 1);
    }

    auto rootNode = multithreadRecursive<Region<T::Dimension>, Node>(
        region, locals, pre, post);
    auto out = Root<T>(rootNode.target);
    for (auto& local : locals) {
        out.claim(local.object_pool);
    }
    if (settings.cancel.load()) {
        return Root<T>();
    }
    else {
        return out;
    }
}

}   // namespace libfive
