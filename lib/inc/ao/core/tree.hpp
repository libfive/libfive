#pragma once

#include <array>
#include <vector>
#include <list>
#include <cstdlib>

#include <glm/mat4x4.hpp>

#include "ao/core/interval.hpp"
#include "ao/core/gradient.hpp"
#include "ao/core/region.hpp"
#include "ao/core/row.hpp"

class Atom;
class Store;
class Token;

/*
 *  A tree represents a math expression that can be evaluated in various ways
 */
class Tree
{
public:
    /*
     *  Construct a tree from the given Store
     */
    explicit Tree(Store* s, Token* root);

    /*
     *  In destructor, delete all of the data that this Tree owns
     */
    ~Tree();

    /*
     *  Sets the matrix constants to the given matrix
     */
    void setMatrix(const glm::mat4& m);

    /*
     *  Single-argument evaluation
     */
    template <class T>
    T eval(T x, T y, T z);

    /*
     *  Vectorized evaluation (defined below)
     */
    template <class T>
    std::vector<T> eval(const std::vector<T>& x,
                        const std::vector<T>& y,
                        const std::vector<T>& z);

    /*
     *  Evaluate across a flattened region
     */
    const double* eval(const Region& r);

    /*
     *  Pushes into a subinterval, disabling inactive nodes
     */
    void push();

    /*
     *  Pops out of interval evaluation, re-enabling disabled nodes
     */
    void pop();

protected:
    /*
     *  Creates a row of the transform matrix
     *
     *  Requires X, Y, Z to be populated
     *  Fills 12 spots in the data array
     */
    Atom* buildMatrixRow(size_t i);

    /*
     *  Evaluates the first 'count' atoms in the list
     */
    template <class T>
    void evalAtom(Atom* a, size_t count);

    /*
     *  Performs the core evaluation sweep (across rows and atoms),
     *  assuming that 'count' locations have been loaded
     */
    template <class T>
    void evalCore(size_t count);

    /*
     *  Prepares for evaluation on the given type, filling constants
     *  with values of the appropriate shape
     */
    template <class T>
    void setMode();

    /*
     *  Prepares the transform matrix's constants arrays with its values
     */
    template <class T>
    void fillConstants();

    /*
     *  Prepares the constant result arrays with its mutable_values
     */
    template <class T>
    void fillMatrix();

    /*
     *  Prepares the most recent set of disabled nodes from mutable_values
     */
    template <class T>
    void fillDisabled();

    /*  All operations live in a set of rows sorted by weight */
    std::vector<Row> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Atom *X, *Y, *Z;

    /*  matrix is a pointer to a 4x3 transform matrix  */
    std::array<Atom*, 12> matrix;

    /*  Pointers to constants live in this vector  */
    std::vector<Atom*> constants;

    /*  This is the top atom of the tree  */
    Atom* root;

    /*  This flag stores how result unions are currently configured  *
     *  There's a compile-time check in atom.cpp that ensures these  *
     *  values are distinct                                          */
    enum Mode { MODE_NONE=0,
                MODE_DOUBLE=sizeof(double),
                MODE_INTERVAL=sizeof(Interval),
                MODE_GRADIENT=sizeof(Gradient) } mode;

    /*  Big bag-o-data that contains this tree's atoms  */
    Atom* data;

    /*  Pointer to the current location in the data array */
    Atom* ptr;
};

////////////////////////////////////////////////////////////////////////////////

#define TREE_INCLUDE_IPP
#include "tree.ipp"
#undef TREE_INCLUDE_IPP
