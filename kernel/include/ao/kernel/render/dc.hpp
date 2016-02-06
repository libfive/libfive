#pragma once

#include <Eigen/Dense>
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

    /*
     *  A QEF structure represents an quadratic error function
     */
    struct QEF
    {
        /*  The A matrix is of the form
         *  [n1x, n1y, n1z]
         *  [n2x, n2y, n2z]
         *  [n3x, n3y, n3z]
         *  ...
         *  (with one row for each Hermite intersection)
         */
        Eigen::Matrix<float, Eigen::Dynamic, 3> A;

        /*  The B matrix is of the form
         *  [p1x . n1x]
         *  [p2x . n2x]
         *  [p3x . n3x]
         *  ...
         *  (with one row for each Hermite intersection)
         */
        Eigen::Matrix<float, Eigen::Dynamic, 1> b;

        /*
         *  operator() evaluates the QEF
         */
        Eigen::Vector3f operator()(const Eigen::Vector3f& pt) const;

        /*
         *  minimize solves the QEF for a value near the given center
         */
        Eigen::Vector3f minimize(const Eigen::Vector3f& center) const;
    };

    /*
     *  Returns a quadratic error function based on surface intersections
     *  and normals along the octree's edges
     */
    QEF getQEF(Octree* o, Evaluator* e);

    /*
     *  Checks to see whether a cell should be collapsed (using its QEF)
     */
    bool shouldCollapse(Octree* o);

    /*
     *  Returns the point at the center of the cell (using its QEF)
     */
    glm::vec3 center(Octree* o);
};
