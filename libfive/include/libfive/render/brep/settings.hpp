/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <atomic>
#include <memory>

namespace libfive {

// Forward declarations
class ProgressHandler;
class FreeThreadHandler;
class VolTree;

enum BRepAlgorithm {
    DUAL_CONTOURING,
    ISO_SIMPLEX,
    HYBRID,
};

struct BRepSettings {
public:
    BRepSettings()
    {
        reset();
    }

    void reset() {
        min_feature = 0.1;
        max_err = 1e-8;
        workers = 8;
        alg = DUAL_CONTOURING;
        free_thread_handler = nullptr;
        progress_handler = nullptr;
        cancel.store(false);
        vol = nullptr;
    }

    /*  The meshing region is subdivided until the smallest region edge
     *  is below min_feature in size.  Make this smaller to get a
     *  higher-resolution model. */
    double min_feature;

    /*  This value is used when deciding whether to collapse cells.  If it
     *  is very small, then only linear regions are merged.  Set as -1 to
     *  completely disable cell merging.  */
    double max_err;

    /*  Number of worker threads to use while meshing.  Set as 0 to use the
     *  platform-default number of threads. */
    unsigned workers;

    /*  This is the meshing algorti */
    BRepAlgorithm alg;

    /*  Optional function called when a thread finds itself without anything
     *  to do.  This can be used to keep threads from spinning if libfive
     *  is embedded in a larger application with its own pooling system. */
    FreeThreadHandler* free_thread_handler;

    /*  Optional class that wraps a progress callback.  */
    ProgressHandler* progress_handler;

    /*  Optional acceleration structure */
    const VolTree* vol;

    mutable std::atomic_bool cancel;
};

}
