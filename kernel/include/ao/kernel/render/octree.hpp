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

    /*  Enumerator that distinguishes between cell types  */
    enum Type { LEAF, BRANCH, EMPTY, FULL };

    /*  Enumerator to refer to octree axes  */
    enum Axis { AXIS_X = 4, AXIS_Y = 2, AXIS_Z = 1 };

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

    /*
     *  Returns this cell's type
     */
    Type getType() const { return type; }

    /*  Struct to store Hermite intersection data  */
    struct Intersection {
        glm::vec3 pos;
        glm::vec3 norm;
    };

    /*
     *  Returns the vector of intersections
     */
    const std::vector<Intersection>& getIntersections() const
    { return intersections; }

    /*
     *  Looks up the vertex position
     */
    glm::vec3 getVertex() const { return vert; }

protected:
    /*
     *  Constructs an octree recursively from the given subregion
     */
    Octree(Evaluator* e, const Subregion& r);

    /*
     *  Splits a subregion and fills out child pointers and cell type
     *
     *  Saves corner gradients in corners array
     *  (either from children or calculated from the evaluator)
     */
    void populateChildren(Evaluator* e, const Subregion& r);

    /*
     *  Stores edge-wise intersections for the cell,
     *  storing them in the intersections vector
     */
    void findIntersections(Evaluator* e);

    /*
     *  If all children are of the same type, collapse the node
     *  (returning the correct cell Type: BRANCH, FULL, or EMPTY)
     */
    void collapseBranch();

    /*
     *  If all corners are of the same sign, convert to FULL or EMPTY
     *  (returning the correct cell Type: LEAF, FULL, or EMPTY)
     */
    void collapseLeaf();

    /*
     *  Finds a feature vertex by solving a least-squares fit to intersections
     *
     *  The resulting vertex is stored in vert; the residual is returned
     */
    float findVertex();

    /*
     *  Checks to make sure that the fine contour is topologically equivalent
     *  to the coarser contour by comparing signs in edges and faces
     */
    bool leafTopology() const;

    /*
     *  Performs binary search along a cube's edge
     *
     *  The resulting Intersection's normal is of unit length
     *
     *  eval(a) should be < 0 (inside the shape) and eval(b) should be outside
     */
    static Intersection searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* e);

    /*  Bounds for this octree  */
    const Interval X, Y, Z;

    /*  Cell type  */
    Type type;

    /*  Intersections where the shape crosses the cell  */
    std::vector<Intersection> intersections;

    /*  Pointers to children octrees (either all populated or all null)  */
    std::array<std::unique_ptr<Octree>, 8> children;

    /*  Array of filled states for the cell's corners  */
    std::array<bool, 8> corners;

    /*  Feature vertex located in the cell  */
    glm::vec3 vert;

    /*  This is a hard-coded list of axis pairs that represent cell edges  */
    const static std::pair<unsigned, unsigned> cellEdges[12];
    const static int SEARCH_COUNT = 8;
};
