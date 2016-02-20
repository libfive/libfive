/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
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
#pragma once

#include <cstdlib>
#include <glm/mat4x4.hpp>

struct Worker;
class Evaluator;

struct Task
{
    /*
     *  The default constructor leaves the task in an invalid state
     */
    Task();
    Task(const glm::mat4& m, size_t ni, size_t nj, size_t nk, size_t level);

    /*
     *  Mark the task as invalid
     */
    void reset();

    /*
     *  Check if the task is valid
     */
    bool valid() const;

    /*  Transform matrix associated with the task  */
    glm::mat4 mat;

    /*  Voxel size associated with the task  */
    size_t ni, nj, nk;

    /*  Subdivision level (1 is highest resolution)  */
    size_t level;
};
