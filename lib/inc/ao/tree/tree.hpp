#pragma once

#include <array>
#include <vector>
#include <list>
#include <cstdlib>

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

    /*  Pointer to a parent (used to decide who destroys the tree)  */
    void* parent=nullptr;

protected:
    /*
     *  Creates a row of the transform matrix
     *
     *  Requires X, Y, Z to be populated
     *  Fills 12 spots in the data array
     */
    Atom* buildMatrixRow(size_t i);

    /*  All operations live in a set of rows sorted by weight */
    std::vector<std::vector<Atom*>> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Atom *X, *Y, *Z;

    /*  matrix is a pointer to a 4x3 transform matrix  */
    std::array<Atom*, 12> matrix;

    /*  Pointers to constants live in this vector  */
    std::vector<Atom*> constants;

    /*  This is the top atom of the tree  */
    Atom* root;

    friend class Accelerator;
    friend class Evaluator;
};
