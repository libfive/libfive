#include <glm/gtx/transform.hpp>

#include "ao/ui/task.hpp"
#include "ao/ui/worker.hpp"
#include "ao/eval/evaluator.hpp"

Task::Task()
    : eval(nullptr), ni(0), nj(0), nk(0), level(0)
{
    // Nothing to do here
}

Task::Task(Evaluator* eval, const glm::mat4& m,
           size_t ni, size_t nj, size_t nk, int level)
    : eval(eval), mat(m), ni(ni), nj(nj), nk(nk), level(level)
{
    // Nothing to do here
}

void Task::reset()
{
    level = 0;
}

bool Task::valid() const
{
    return level > 0;
}

Worker* Task::start() const
{
    assert(level > 0);

    // Apply the matrix to the tree, applying an extra scaling on
    // the z axis to make the coordinate system match OpenGL
    auto m_ = glm::scale(glm::inverse(mat), glm::vec3(1, 1, -1));
    eval->setMatrix(m_);

    return new Worker(eval, ni, nj, nk, level);
}
