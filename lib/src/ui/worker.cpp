#include <glm/gtx/transform.hpp>

#include "ao/gl/core.hpp"
#include "ao/ui/worker.hpp"
#include "ao/ui/task.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/gl/accelerator.hpp"
#include "ao/gl/texture.hpp"

Worker::Worker(const Task& t)
    : region({-1, 1}, {-1, 1}, {-1, 1},
             t.ni/(2*t.level), t.nj/(2*t.level), t.nk/(2*t.level)),
      future(promise.get_future()), abort(false)
{
    // Nothing to do here
}

Worker::Worker(Evaluator* eval, const Task& t, GLFWwindow* context,
               GLuint depth, GLuint norm)
    : Worker(t)
{
    assert(t.level > 0);

    // Apply the matrix to the tree, applying an extra scaling on
    // the z axis to make the coordinate system match OpenGL
    auto m = glm::scale(glm::inverse(t.mat), glm::vec3(1, 1, -1));
    eval->setMatrix(m);

    thread = std::thread([=](){
        auto out = Heightmap::Render(eval, this->region, this->abort);
        if (!this->abort.load())
        {
            glfwMakeContextCurrent(context);
            toDepthTexture(out.first, depth);
            toNormalTexture(out.second, norm);
            glFinish();
            this->promise.set_value(true);
        }
        else
        {
            this->promise.set_value(false);
        }
        glfwPostEmptyEvent(); });
}

Worker::Worker(Accelerator* accel, const Task& t, GLFWwindow* context,
               GLuint depth, GLuint norm)
    : Worker(t)
{
    // Apply the matrix to the tree, applying an extra scaling on
    // the z axis to make the coordinate system match OpenGL
    auto m = glm::scale(glm::inverse(t.mat), glm::vec3(1, 1, -1));
    accel->setMatrix(m);

    thread = std::thread([=](){
        glfwMakeContextCurrent(context);
        accel->init(this->region, depth, norm);
        accel->Render(this->region, depth, norm);
        glFinish();
        this->promise.set_value(true);
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
