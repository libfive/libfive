/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <chrono>
#include <future>

#include <Eigen/Dense>

#include "ao/ui/gl/core.hpp"
#include "ao/kernel/render/heightmap.hpp"

struct Task;

/*
 *  A worker contains all of the data needed for a running render task
 */
struct Worker
{
    /*
     *  Constructs a CPU worker from the given Tree and a task
     *  (higher task divisors produce lower-resolution workers)
     *
     *  depth and norm are target textures in which results are stored
     */
    Worker(const std::vector<Evaluator*>& evaluators, const Task& task);

    /*
     *  On destruction, join the thread
     */
    ~Worker();

    /*
     *  Returns true if the worker is running, false otherwise
     */
    bool running() const;

    /*
     *  Attempts to halt the task by setting the abort flag
     */
    void halt();

    /*
     *  Polls the worker, loading data into the given textures if complete
     *
     *  Returns RUNNING if the worker is still running, DONE if the worker
     *  is done, and ABORTED if the worker was aborted but is complete.
     *
     *  On success, images are deployed to depth and norm textures
     *  (so there needs to be a current OpenGL context)
     */
    enum State { RUNNING, DONE, ABORTED };
    State poll(GLuint depth, GLuint norm);

    /*  Region that is being analyzed  */
    Region region;

    /*  Fun async stuff  */
    std::promise<std::pair<DepthImage, NormalImage>> promise;
    std::future<std::pair<DepthImage, NormalImage>> future;
    std::atomic_bool abort;
    std::thread thread;

    /*  Records how long the render took  */
    std::chrono::duration<double> elapsed;
};
