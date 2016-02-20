/*
 *  Copyright (C) 2016 Matthew Keeter
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include "ao/ui/task.hpp"
#include "ao/ui/worker.hpp"

#include "ao/kernel/eval/evaluator.hpp"

Task::Task()
    : ni(0), nj(0), nk(0), level(0)
{
    // Nothing to do here
}

Task::Task(const glm::mat4& m, size_t ni, size_t nj, size_t nk, size_t level)
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
