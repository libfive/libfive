#pragma once

#include <array>
#include <memory>
#include <vector>

#include <glm/vec3.hpp>
#include <glm/vec3.hpp>

#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/gradient.hpp"

class Region;
class Subregion;
class Tree;
class Evaluator;

class Octree
{
public:
    static Octree* Render(Tree* t, const Region& r);

    /*
     *  Returns the position of the given corner
     *
     *  Must be in agreement with the Subregion splitting order in octsect
     */
    glm::vec3 pos(uint8_t i) const;

    /*
     *  Look up a corner's value
     */
    bool corner(uint8_t i) const { return corners[i]; }

    /*
     *  Look up a child octree
     */
    const Octree* child(uint8_t i) const
    { return type == BRANCH ? children[i].get() : this; }

    /*  Enumerator to refer to octree axes  */
    enum Axis { AXIS_X = 4,
                AXIS_Y = 2,
                AXIS_Z = 1 };

    /*  Struct to store Hermite intersection data  */
    struct Intersection {
        glm::vec3 pos;
        Gradient value;
    };

protected:
    /*  Pointers to children octrees (either all populated or all null)  */
    std::array<std::unique_ptr<Octree>, 8> children;

    /*  Array of filled states for the cell's corners  */
    std::array<bool, 8> corners;

public:
    /*  Bounds for this octree  */
    const Interval X, Y, Z;

    /*  Cell type  */
    const enum Type { LEAF, BRANCH, EMPTY, FULL } type;

    /*  Intersections where the shape crosses the cell  */
    const std::vector<Intersection> intersections;

protected:
    /*
     *  Constructs an octree recursively from the given subregion
     */
    Octree(Evaluator* e, const Subregion& r);

    /*
     *  Splits a subregion and fills out child pointers
     *
     *  Saves corner gradients in corners array
     *  (either from children or calculated from the evaluator)
     *
     *  Returns the cell's type.
     */
    Type populateChildren(Evaluator* e, const Subregion& r);

    /*
     *  Finds a set of gradients for the given region
     */
    std::vector<Intersection> findIntersections(Evaluator* e) const;

    /*
     *  If all children are of the same type, collapse the node
     *  (returning the correct cell Type: BRANCH, FULL, or EMPTY)
     */
    Type collapseBranch();

    /*
     *  If all corners are of the same sign, convert to FULL or EMPTY
     *  (returning the correct cell Type: LEAF, FULL, or EMPTY)
     */
    Type collapseLeaf();

    /*
     *  Performs binary search along a cube's edge
     *  eval(a) should be < 0 (inside the shape) and eval(b) should be outside
     */
    static Intersection searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* e);

    /*  This is a hard-coded list of axis pairs that represent cell edges  */
    const static std::pair<unsigned, unsigned> cellEdges[12];

    const static int SEARCH_COUNT = 8;
};
