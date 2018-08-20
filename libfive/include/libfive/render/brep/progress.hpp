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

bool EMPTY_PROGRESS_CALLBACK(float);

/*
 *  runProgressCallback checks whether the given callback is
 *  EMPTY_PROGRESS_CALLBACK.  If that is not the case, it starts
 *  a separate thread running to handle progress checking.
 *
 *  Values from io are accumulated, and their sum is divided by
 *  total and added to offset then passed into the callback.
 *
 *  The returned future holds this async thread.
 *  If the future is constructed, started is set to true; otherwise,
 *  it is set to false.
 */
template <typename T>
std::future<void> runProgressCallback(
    std::function<bool(float)> cb, boost::lockfree::stack<T>& io,
    T total, int offset, std::atomic_bool& done, std::atomic_bool& cancel,
    bool* started)
{
    auto progress_cb_ptr = cb.target<bool(*)(float)>();
    const bool has_progress_callback = !progress_cb_ptr ||
        (*progress_cb_ptr != EMPTY_PROGRESS_CALLBACK);

    std::future<void> progress_task;

    if (has_progress_callback)
    {
        *started = true;

        progress_task = std::async(std::launch::async,
            [&io, &cb, &done, &cancel, total, offset]()
            {
                T accumulated = 0;
                while (!done.load() && !cancel.load())
                {
                    T next;
                    // Read values from the queue and progress them
                    while (io.pop(next))
                    {
                        accumulated += next;
                    }

                    // Report total progress so far
                    cb(accumulated / (float)total + offset);

                    // Update the progress tracker at 20 Hz, to avoid
                    // using 100% of a core just to track progress.
                    std::this_thread::sleep_for(std::chrono::milliseconds(50));
                }
                // At the end of the process building, report 100% completion.
                if (!cancel.load())
                {
                    cb(1.0f + offset);
                }
            }
        );
    }
    else
    {
        *started = false;
    }
    return progress_task;
}
