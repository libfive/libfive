#pragma once

#include <array>
#include <unordered_map>
#include <vector>

#include <glm/mat4x4.hpp>

#include "ao/eval/row.hpp"

////////////////////////////////////////////////////////////////////////////////

class Atom;
class Clause;
class Tree;
class Region;

class Evaluator
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    explicit Evaluator(const Tree* t);
    ~Evaluator();

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
     *  Evaluate across a flattened region
     */
    const double* eval(const Region& r);

    /*
     *  Performs the core evaluation sweep (across rows and atoms),
     *  assuming that 'count' locations have been loaded
     */
    template <class T>
    const T* evalCore(size_t count);

    /*
     *  Sets the evaluation target at the given index
     */
    template <class T>
    void setPoint(T x, T y, T z, size_t index);

    /*
     *  Pushes into a subinterval, disabling inactive nodes
     */
    void push();

    /*
     *  Pops out of interval evaluation, re-enabling disabled nodes
     */
    void pop();
    /*
     *  Evaluates the first 'count' result slots in the given atom
     */
    template <class T>
    void evalClause(Clause* a, size_t count);

protected:
    /*
     *  Constructs a new clause in the data array and increments ptr
     */
    Clause* newClause(const Atom* m,
                      std::unordered_map<const Atom*, Clause*>& clauses);

    /*  All operations live in a set of rows sorted by weight */
    std::vector<Row> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Clause *X, *Y, *Z;

    /*  matrix is a pointer to a 4x3 transform matrix  */
    std::array<Clause*, 12> matrix;

    /*  Pointers to constants live in this vector  */
    std::vector<Clause*> constants;

    /*  This is the top atom of the tree  */
    Clause* root;

    /*  Bag-o-data that stores clauses  */
    Clause* data;
    Clause* ptr;
};

////////////////////////////////////////////////////////////////////////////////

#define EVALUATOR_INCLUDE_IPP
#include "evaluator.ipp"
#undef EVALUATOR_INCLUDE_IPP
