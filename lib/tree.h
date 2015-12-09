#pragma once

#include <stack>
#include <vector>
#include <list>
#include <cstdlib>

#include "interval.h"

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
    Tree(Store* s, Token* root);

    /*
     *  In destructor, delete all of the data that this Tree owns
     */
    ~Tree();

    /*
     *  Single-argument evaluation
     */
    double eval(double x, double y, double z);
    Interval eval(Interval x, Interval y, Interval z);

    /*
     *  Vectorized evaluation
     */
    std::vector<double> eval(const std::vector<double>& x,
                             const std::vector<double>& y,
                             const std::vector<double>& z);
    std::vector<Interval> eval(const std::vector<Interval>& x,
                               const std::vector<Interval>& y,
                               const std::vector<Interval>& z);
protected:
    /*
     *  The Row subclass stores a row of atoms (of a particular weight)
     */
    class Row : public std::vector<Atom*>
    {
    public:
        /*
         *  Applies the given flag to every Atom in the row
         */
        void setFlag(uint8_t flag);

        /*
         *  Push a new disabled count onto the stack
         */
        void push();

        /*
         *  Re-enable nodes that were disabled since the last push
         */
        void pop();

        /*
         *  Disables the i'th atom in the list by swapping it to the back
         *  and decrementing the active count by 1
         *
         *  Also increments the disabled count by one; must be called after at
         *  least one push() so that there's something on the disabled stack
         */
        void disable(size_t i);

        /*  active is the number of atoms to evaluate in each pass  */
        size_t active;

        /*  disabled stores the number of atoms disabled in each call to  *
         *  Row::disable.  It's a stack with values that are popped off   *
         *  with Row::pop                                                 */
        std::stack<size_t> disabled;
    };

    /*
     *  Sets the given flag for every node in the tree
     */
    void setFlag(uint8_t flag);

    /*
     *  Prepare for evaluation on a set of doubles
     *
     *  Throws an assertion if there are more doubles than ATOM_ARRAY_SIZE
     *  or if the vectors are of different sizes.
     */
    template <class T>
    void setPos(const std::vector<T>& x,
                const std::vector<T>& y,
                const std::vector<T>& z);

    /*  All operations live in a set of rows sorted by weight */
    std::list<Row> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Atom *X, *Y, *Z;

    /*  Pointers to constants live in this vector  */
    std::vector<Atom*> constants;

    /*  This is the top atom of the tree  */
    Atom* root;

    /*  Big bag-o-data that contains this tree's atoms  */
    Atom* data;
};
