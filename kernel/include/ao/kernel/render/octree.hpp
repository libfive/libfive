#pragma once

#include <array>
#include <memory>

#include <glm/vec3.hpp>

#include "ao/eval/gradient.hpp"
#include "ao/eval/interval.hpp"

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
    Gradient corner(uint8_t i) const { return corners[i]; }

    /*
     *  Look up a child octree
     */
    Octree* child(uint8_t i) const { return children[i].get(); }

protected:
    /*  Pointers to children octrees (either all populated or all null)  */
    std::array<std::unique_ptr<Octree>, 8> children;

    /*  Array of values in the cell's corners  */
    std::array<Gradient, 8> corners;

public:
    /*  Bounds for this octree  */
    const Interval X, Y, Z;

    /*  Cell type  */
    const enum Type { LEAF, BRANCH, EMPTY, FULL } type;

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
     *  If all children are of the same type, collapse the node
     *  (returning the correct cell Type: BRANCH, FULL, or EMPTY)
     */
    Type collapseBranch();

    /*
     *  If all corners are of the same sign, convert to FULL or EMPTY
     *  (returning the correct cell Type: LEAF, FULL, or EMPTY)
     */
    Type collapseLeaf();
};
