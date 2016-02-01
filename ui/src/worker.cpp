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
        promise.set_value(
                Heightmap::Render(tree, this->region, this->abort, m));
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
