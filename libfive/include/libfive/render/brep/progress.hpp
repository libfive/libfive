/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once
#include <future>
#include <boost/lockfree/stack.hpp>

namespace Kernel {

bool EMPTY_PROGRESS_CALLBACK(float);

typedef std::function<bool(float)> ProgressCallback;

class ProgressWatcher
{
public:
    /*
     *  If the callback is not EMPTY_PROGRESS_CALLBACK, constructs a new
     *  ProgressWatcher and returns it.
     */
    static ProgressWatcher* build(uint32_t total, float offset,
                                  std::function<bool(float)> callback,
                                  std::atomic_bool& done,
                                  std::atomic_bool& cancel);

    void tick(uint32_t i=1);

    /*  On destruction, either done or cancel must be true.
     *  The destructor waits for the worker thread to finish. */
    ~ProgressWatcher();

protected:
    ProgressWatcher(uint32_t total, float offset,
                    std::function<bool(float)> callback,
                    std::atomic_bool& done,
                    std::atomic_bool& cancel);

    std::function<bool(float)> callback;
    std::atomic_bool& done;
    std::atomic_bool& cancel;

    /*  Used to stop the worker thread early, since it otherwise only
     *  updates at a fixed speed (e.g. 200 Hz). */
    std::timed_mutex mut;

    std::atomic_uint32_t counter;
    const uint32_t total;
    const float offset;

    std::future<void> future;
};

}   // namespace Kernel
