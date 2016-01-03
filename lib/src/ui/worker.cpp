#include <glm/gtx/transform.hpp>

#include "ao/gl/core.hpp"
#include "ao/ui/worker.hpp"
#include "ao/ui/task.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/gl/accelerator.hpp"

Worker::Worker(Evaluator* eval, const Task& t)
    : region({-1, 1}, {-1, 1}, {-1, 1},
             t.ni/(2*t.level), t.nj/(2*t.level), t.nk/(2*t.level)),
      future(promise.get_future()), abort(false)
{
    assert(t.level > 0);

    // Apply the matrix to the tree, applying an extra scaling on
    // the z axis to make the coordinate system match OpenGL
    auto m = glm::scale(glm::inverse(t.mat), glm::vec3(1, 1, -1));
    eval->setMatrix(m);

    thread = std::thread([=](){
            std::pair<DepthImage, NormalImage> out =
                Heightmap::Render(eval, this->region, this->abort);
            this->promise.set_value(out);
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

bool Worker::poll(GLuint depth, GLuint norm)
{
    std::future_status status = future.wait_for(std::chrono::seconds(0));

    if (status != std::future_status::ready)
    {
        return false;
    }

    // Get the resulting matrix
    auto out = future.get();
    Eigen::ArrayXXf d = out.first.cast<float>().transpose();

    // Convert the resulting matrix into an OpenGL texture
    // if the abort flag is not set
    if (!abort.load())
    {
        NormalImage s = out.second.transpose();

        // Pack the Eigen matrices into an OpenGL texture
        glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, depth);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, d.rows(), d.cols(),
                0, GL_RED, GL_FLOAT, d.data());
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        glActiveTexture(GL_TEXTURE0 + 1);
        glBindTexture(GL_TEXTURE_2D, norm);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, s.rows(), s.cols(),
                0, GL_RGBA, GL_UNSIGNED_BYTE, s.data());
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    }

    return true;
}
