#include "ao/ui/task.hpp"
#include "ao/ui/worker.hpp"
#include "ao/eval/evaluator.hpp"

Task::Task()
    : ni(0), nj(0), nk(0), level(0)
{
    // Nothing to do here
}

Task::Task(const glm::mat4& m, size_t ni, size_t nj, size_t nk, int level)
    : mat(m), ni(ni), nj(nj), nk(nk), level(level)
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
