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
     *  Prepares for evaluation on a set of doubles by filling constant
     *  results with their value
     *
     *  count is the number of slots to fill or 0 to fill all
     */
    void modeDouble(size_t count);

    /*
     *  Prepares for evaluation on a set of doubles by filling constant
     *  results with their value
     *
     *  count is the number of slots to fill or 0 to fill all
     */
    void modeInterval(size_t count);

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
    void load_if(Atom* a, const std::vector<T>& vs, size_t index, size_t count);

    /*
     *  Evaluates a specific atom (with a switch statement on the opcode)
     */
    template <class T>
    void eval_atom(Atom* a, size_t i);

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

    /*  Big bag-o-data that contains this tree's atoms  */
    Atom* data;
};

////////////////////////////////////////////////////////////////////////////////

#include "atom.h"

template <class T>
inline void Tree::load_if(Atom* a, const std::vector<T>& vs,
                          size_t index, size_t count)
{
    if (a)
    {
        a->result.set(&vs[index], count);
    }
}

template <class T>
inline void Tree::eval_atom(Atom* m, size_t i)
{
    switch (m->op) {
        case OP_ADD:
            m->result.set<T>(m->a->result.get<T>(i) +
                             m->a->result.get<T>(i), i);
        case OP_MUL:
            m->result.set<T>(m->a->result.get<T>(i) *
                             m->a->result.get<T>(i), i);
        case OP_MIN:
            m->result.set<T>(std::min(m->a->result.get<T>(i),
                                      m->a->result.get<T>(i)), i);
        case OP_MAX:
            m->result.set<T>(std::max(m->a->result.get<T>(i),
                                      m->a->result.get<T>(i)), i);
        case OP_SUB:
            m->result.set<T>(m->a->result.get<T>(i) -
                             m->a->result.get<T>(i), i);
        case OP_DIV:
            m->result.set<T>(m->a->result.get<T>(i) /
                             m->a->result.get<T>(i), i);
        case OP_SQRT:
            m->result.set<T>(sqrt(m->a->result.get<T>(i)), i);
        case OP_NEG:
            m->result.set<T>(-m->a->result.get<T>(i), i);
        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}

template <class T>
inline std::vector<T> Tree::eval(const std::vector<T>& x,
                                 const std::vector<T>& y,
                                 const std::vector<T>& z)
{
    assert(x.size() == y.size() && x.size() == z.size());

    size_t remaining = x.size();

    std::vector<T> out;
    out.resize(remaining);
    size_t index = 0;

    while (remaining)
    {
        const size_t count = std::min(remaining, ATOM_ARRAY_BYTES / sizeof(T));

        load_if(X, x, index, count);
        load_if(Y, y, index, count);
        load_if(Z, z, index, count);

        for (const auto& row : rows)
        {
            for (auto& m : row)
            {
                for (size_t i=0; i < count; ++i)
                {
                    eval_atom<T>(m, i);
                }
            }
        }
        remaining -= count;
        root->result.copy_to(&out[index], count);
        index += count;
    }
    return out;
}

template <class T>
inline T Tree::eval(T x, T y, T z)
{
    return eval(std::vector<T>(1, x),
                std::vector<T>(1, y),
                std::vector<T>(1, z))[0];
}
