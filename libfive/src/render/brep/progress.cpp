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

namespace libfive {

ProgressHandler::ProgressHandler()
    : done(false)
{
    // The mutex remains locked until the destructor is called, so that
    // the try_lock_for calls block, preventing the worker thread from
    // spinning too hard.
    timed_mut.lock();
}

ProgressHandler::~ProgressHandler()
{
    finish();
}

void ProgressHandler::run()
{
    assert(phases.size() > 0);

    progress(0.0f);

    float prev = 0.0f;
    while (!done.load()) {
        // Sleep for 50 ms, returning early if the mutex is
        // unlocked (which happens in the destructor)
        auto b = timed_mut.try_lock_for(std::chrono::milliseconds(50));
        (void)b; // Result is unused, but this prevents a warning

        float accum = 0.0f;
        {   // Locked region which accesses current_phase
            std::lock_guard<std::mutex> lock(phase_mut);
            auto itr = phases.begin();
            do {
                if (itr->total) {
                    accum += itr->weight * itr->counter.load() /
                                           (float)itr->total;
                }
            } while (itr++ != current_phase);
        }

        const float next = total_weight ? (accum / total_weight) : 0.0;
        if (next != prev) {
            progress(next);
            prev = next;
        }
    }
}

void ProgressHandler::start(const std::vector<unsigned>& weights)
{
    total_weight = 0;
    for (unsigned i=0; i < weights.size(); ++i) {
        phases.emplace_back(weights[i]);
        total_weight += weights[i];
    }
    current_phase = phases.end();
}

void ProgressHandler::nextPhase(uint64_t total) {
    // When we start the worker thread, we set current_phase = phases.end()
    // to mark that it hasn't yet started.  The first branch of this
    // conditional detects that case and starts the worker thread.
    if (current_phase == phases.end()) {
        current_phase = phases.begin();
        future = std::async(std::launch::async, [this]() { run(); });
    } else {
        std::lock_guard<std::mutex> lock(phase_mut);

        // Consistency checking: we must count up the expected number
        // of ticks, otherwise there's a bug somewhere in the library.
        assert(current_phase->counter == current_phase->total);

        // Move to the next phase while the lock is present
        current_phase++;
    }
    assert(current_phase != phases.end());
    current_phase->total = total;
}

void ProgressHandler::progress(double d)
{
    (void)d;
}

void ProgressHandler::finish()
{
    if (future.valid()) {
        // Set the flag to abort the worker thread
        done = true;

        // Speed up the thread's exit condition by releasing the timex mutex
        timed_mut.unlock();

        // Then wait for the thread to finish
        future.wait();
    }
}

void ProgressHandler::tick(uint64_t i)
{
    current_phase->counter += i;
}

}   // namespace libfive
