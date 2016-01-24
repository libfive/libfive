#include <glm/gtx/transform.hpp>

#include "ao/gl/core.hpp"
#include "ao/ui/worker.hpp"
#include "ao/ui/task.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/gl/texture.hpp"

Worker::Worker(const Task& t)
    : region({-1, 1}, {-1, 1}, {-1, 1},
             t.ni/(2 << t.level), t.nj/(2 << t.level), t.nk/(2 << t.level)),
      future(promise.get_future()), abort(false)
{
    // Nothing to do here
}

void Worker::start()
{
    start_time = std::chrono::system_clock::now();
}

void Worker::end()
{
    elapsed = std::chrono::system_clock::now() - start_time;
    promise.set_value(!abort.load());
    glfwPostEmptyEvent();
}

Worker::Worker(Tree* tree, const Task& t, GLFWwindow* context,
               GLuint depth, GLuint norm)
    : Worker(t)
{
    assert(t.level > 0);

    // Generate a transform matrix, applying an extra scaling
    // on the z axis to make the coordinate system match OpenGL
    auto m = glm::scale(glm::inverse(t.mat), glm::vec3(1, 1, -1));

    thread = std::thread([=](){
        this->start();
        auto out = Heightmap::Render(tree, this->region, this->abort, m);
        if (!this->abort.load())
        {
            glfwMakeContextCurrent(context);
            toDepthTexture(out.first, depth);
            toNormalTexture(out.second, norm);
            glFinish();
        }
        this->end(); });
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

Worker::State Worker::poll()
{
    std::future_status status = future.wait_for(std::chrono::seconds(0));

    if (status != std::future_status::ready)
    {
        return RUNNING;
    }

    // Get the resulting matrix
    return future.get() ? DONE : ABORTED;
}
