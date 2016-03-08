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
#include <glm/gtx/transform.hpp>

#include "ao/ui/worker.hpp"
#include "ao/ui/task.hpp"

#include "ao/ui/gl/core.hpp"
#include "ao/ui/gl/texture.hpp"

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

Worker::Worker(Tree* tree, const Task& t)
    : region({-1, 1}, {-1, 1}, {-1, 1},
             t.ni/(1 << t.level), t.nj/(1 << t.level), t.nk/(1 << t.level)),
      future(promise.get_future()), abort(false)
{
    assert(t.level > 0);

    // Generate a transform matrix, applying an extra scaling
    // on the z axis to make the coordinate system match OpenGL
    auto m = glm::scale(glm::inverse(t.mat), glm::vec3(1, 1, -1));

    thread = std::thread([=](){
        auto start_time = std::chrono::system_clock::now();
        auto out = Heightmap::Render(tree, this->region, this->abort, m);

        // Map the depth buffer into the 0 - 1 range, with -inf = 1
        Eigen::ArrayXXf d =
            (out.first == -std::numeric_limits<float>::infinity())
            .select(1, (1 - out.first) / 2);

        promise.set_value({d, out.second});
        elapsed = std::chrono::system_clock::now() - start_time;
        glfwPostEmptyEvent(); });
}

Worker::~Worker()
{
    if (thread.joinable())
    {
        thread.join();
    }
}

bool Worker::running() const
{
    return future.valid() && future.wait_for(std::chrono::seconds(0)) ==
                             std::future_status::timeout;
}

void Worker::halt()
{
    abort.store(true);
}

Worker::State Worker::poll(GLuint depth, GLuint norm)
{
    std::future_status status = future.wait_for(std::chrono::seconds(0));

    if (status != std::future_status::ready)
    {
        return RUNNING;
    }
    else if (!abort.load())
    {
        auto out = future.get();
        toDepthTexture(out.first, depth);
        toNormalTexture(out.second, norm);
        return DONE;
    }
    else
    {
        return ABORTED;
    }
}
