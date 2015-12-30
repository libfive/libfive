#pragma once

#include <cstdlib>
#include <glm/mat4x4.hpp>

struct Worker;
class Tree;

struct Task
{
    /*
     *  The default constructor leaves the task in an invalid state
     */
    Task();
    Task(Tree* t, const glm::mat4& m, size_t ni, size_t nj, size_t nk,
         int level);

    /*
     *  Mark the task as invalid
     */
    void reset();

    /*
     *  Check if the task is valid
     */
    bool valid() const;

    /*
     *  Kick off a render operation in a separate thread
     */
    Worker* start() const;

    /*  Target tree  */
    Tree* tree;

    /*  Transform matrix associated with the task  */
    glm::mat4 mat;

    /*  Voxel size associated with the task  */
    size_t ni, nj, nk;

    /*  Subdivision level (1 is highest resolution)  */
    size_t level;
};
