#pragma once

#include <array>
#include <unordered_map>
#include <vector>

#include <glm/mat4x4.hpp>

#include "ao/kernel/eval/row.hpp"
#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/clause.hpp"

////////////////////////////////////////////////////////////////////////////////

class Atom;
class Clause;
class Tree;

class Evaluator
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    explicit Evaluator(const Tree* t);
    explicit Evaluator(const Tree* t, const glm::mat4& m);
    ~Evaluator();

    /*
     *  Sets the matrix constants to the given matrix
     */
    void setMatrix(const glm::mat4& m);

    /*
     *  Single-argument evaluation
     */
    float eval(float x, float y, float z);
    Interval eval(Interval x, Interval y, Interval z);

    /*
     *  Evaluates a set of floating-point results
     *  (which have been loaded with set)
     */
    const float* values(size_t count);

    /*
     *  Evaluate a set of gradients, returning a tuple
     *      value, dx, dy, dz
     *
     *  Values must have been previously loaded by set
     */
    std::tuple<const float*, const float*,
               const float*, const float*> derivs(size_t count);

    /*
     *  Evaluates a single interval (stored with set)
     */
    Interval interval();

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(float x, float y, float z, size_t index)
    {
        X->result.set(x, index);
        Y->result.set(y, index);
        Z->result.set(z, index);
    }

    /*
     *  Stores the given interval in the result objects
     */
    void set(Interval X, Interval Y, Interval Z);

    /*
     *  Pushes into a subinterval, disabling inactive nodes
     */
    void push();

    /*
     *  Pops out of interval evaluation, re-enabling disabled nodes
     */
    void pop();

    /*
     *  Returns the fraction active / total nodes
     *  (to check how well disabling is working)
     */
    double utilization() const;

#ifdef __AVX__
    /*
     *  Pack values from the X, Y, Z float arrays to the AVX array
     */
    void packAVX();

    /*
     *  Unpack values from the result array to the float array
     */
    const float* unpackAVX();
#endif

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
