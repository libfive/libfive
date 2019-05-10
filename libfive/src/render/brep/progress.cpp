/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include <iostream>
#include <cassert>

#include "libfive/render/brep/progress.hpp"

namespace Kernel {

void EMPTY_PROGRESS_CALLBACK(float) { /* Nothing to do here */ }

ProgressWatcher* ProgressWatcher::build(uint64_t total, float offset,
                                        std::function<void(float)> callback,
                                        std::atomic_bool& done,
                                        std::atomic_bool& cancel)
{
    auto progress_cb_ptr = callback.target<void(*)(float)>();
    const bool has_progress_callback = !progress_cb_ptr ||
        (*progress_cb_ptr != EMPTY_PROGRESS_CALLBACK);

    if (has_progress_callback)
    {
        return new ProgressWatcher(total, offset, callback, done, cancel);
    }
    else
    {
        return nullptr;
    }
}

ProgressWatcher::ProgressWatcher(uint64_t total, float offset,
                                 std::function<void(float)> callback,
                                 std::atomic_bool& done,
                                 std::atomic_bool& cancel)
    : callback(callback), done(done), cancel(cancel),
      counter(0), total(total), offset(offset)
{
    if (!counter.is_lock_free())
    {
        std::cerr << "ProgressWatcher: counter is not lock-free" << std::endl;
    }
    mut.lock();

    future = std::async(std::launch::async,
        [this]()
        {
            if (this->offset == 0.0f)
            {
                this->callback(0.0f);
            }

            uint64_t n = 0;
            while (!this->done.load() && !this->cancel.load())
            {
                const uint64_t next = this->counter.load();
                if (next != n)
                {
                    n = next;
                    this->callback(n / (float)this->total + this->offset);
                }
                // Sleep for 50 ms, returning early if the mutex is
                // unlocked (which happens in the destructor)
                auto b = this->mut.try_lock_for(std::chrono::milliseconds(50));
                (void)b; // Result is unused, but this prevents a warning
            }

            // Once evaluation is finished, report that we're completely done
            if (!this->cancel.load())
            {
                this->callback(1.0f + this->offset);
            }
        });
}

ProgressWatcher::~ProgressWatcher()
{
    assert(done.load() || cancel.load());

    // Speed up the thread's exit condition by releasing the timex mutex
    mut.unlock();
    // Then wait for the thread to finish
    future.wait();
}

void ProgressWatcher::tick(uint64_t i)
{
    counter += i;
}

}   // namespace Kernel
