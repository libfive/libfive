#pragma once

#include <glm/vec3.hpp>

#include "ao/kernel/format/mesh.hpp"

class Tree;
class Octree;
class Region;
class Evaluator;

namespace DC
{
    /*
     *  Run dual contouring on the given evaluator, returning a Mesh
     *
     *  This involves sampling the evaluator on an octree (with QEF
     *  simplification to collapse leaf cells), then using DC to generate
     *  a triangle mesh.
     */
    Mesh Render(Tree* t, const Region& r);
};
