/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once
#include <future>
#include <boost/lockfree/stack.hpp>

namespace Kernel {

void EMPTY_PROGRESS_CALLBACK(float);

typedef std::function<void(float)> ProgressCallback;

class ProgressWatcher
{
public:
    /*
     *  If the callback is not EMPTY_PROGRESS_CALLBACK, constructs a new
     *  ProgressWatcher and returns it.
     */
    static ProgressWatcher* build(uint64_t total, float offset,
                                  ProgressCallback callback,
                                  std::atomic_bool& done,
                                  std::atomic_bool& cancel);

    void tick(uint64_t i=1);

    /*  On destruction, either done or cancel must be true.
     *  The destructor waits for the worker thread to finish. */
    ~ProgressWatcher();

protected:
    ProgressWatcher(uint64_t total, float offset,
                    ProgressCallback callback,
                    std::atomic_bool& done,
                    std::atomic_bool& cancel);

    ProgressCallback callback;
    std::atomic_bool& done;
    std::atomic_bool& cancel;

    /*  Used to stop the worker thread early, since it otherwise only
     *  updates at a fixed speed (e.g. 200 Hz). */
    std::timed_mutex mut;

    std::atomic_uint64_t counter;
    const uint64_t total;
    const float offset;

    std::future<void> future;
};

}   // namespace Kernel
