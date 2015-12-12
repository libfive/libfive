#pragma once

#include <stack>
#include <vector>
#include <list>
#include <cstdlib>

#include "interval.hpp"
#include "gradient.hpp"

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
    template <class T>
    T eval(T x, T y, T z);

    /*
     *  Vectorized evaluation (defined below)
     */
    template <class T>
    std::vector<T> eval(const std::vector<T>& x,
                        const std::vector<T>& y,
                        const std::vector<T>& z);

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
     *  If the given atom is present, load vs[index] through vs[index + count]
     */
    template <class T>
    void loadIf(Atom* a, const std::vector<T>& vs, size_t index, size_t count);

    /*
     *  Evaluates a specific atom (with a switch statement on the opcode)
     */
    template <class T>
    void evalAtom(Atom* a, size_t i);

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
     *  Sets the given flag for every node in the tree
     */
    void setFlag(uint8_t flag);

    /*  All operations live in a set of rows sorted by weight */
    std::list<Row> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Atom *X, *Y, *Z;

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
};

#include "tree.ipp"
