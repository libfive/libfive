#pragma once

#include <vector>

#include "interval.h"
#include "opcode.h"
#include "gradient.h"

class Token;

// This is the storage size allocated to each array (in bytes)
//
// For efficiency, it should be sized to fit an non-fractional number of
// doubles, Intervals, and Gradient objects.  An error will be thrown
// at compile-time if this condition is not met.
#define ATOM_ARRAY_BYTES ((size_t)256)

#define ATOM_DOUBLE_COUNT   (ATOM_ARRAY_BYTES / sizeof(double))
#define ATOM_INTERVAL_COUNT (ATOM_ARRAY_BYTES / sizeof(Interval))
#define ATOM_GRADIENT_COUNT (ATOM_ARRAY_BYTES / sizeof(Gradient))

#define ATOM_FLAG_MASK 0xff
#define ATOM_FLAG_IGNORED 1
#define ATOM_FLAG_BOOLEAN 2

/*
 *  An Atom represent an operation in a math tree.
 */
class Atom
{
    /*
     *  Construct an atom from the given token, setting the token's
     *  atom pointer to this.
     *
     *  Requires that the token's children be packed into Atoms
     *  beforehand (otherwise will throw an assertion)
     */
    Atom(Token* t);

protected:
    const Opcode op;

    /*  Populated for OP_CONST atoms */
    const double value;

    /*  Flags are set during evaluation for various purposes  */
    uint8_t flags;

    /*  Populated for operators with arguments */
    Atom* const a;
    Atom* const b;

    /*  Results are stored in this union */
    union Result {
        Result() { /* Provide default constructor */ }

        /*
         *  Set the values to the given vector
         */
        template <class T> void set(const std::vector<T>& ds)
        {
            set(&ds[0], ds.size());
        }

        /*
         *  Specialized setters for specific types of values
         */
        void set(const double* ds, size_t count=ATOM_DOUBLE_COUNT);
        void set(const Interval* ds, size_t count=ATOM_INTERVAL_COUNT);

        double d[ATOM_DOUBLE_COUNT];
        Interval i[ATOM_INTERVAL_COUNT];
        Gradient g[ATOM_GRADIENT_COUNT];
    } result;

    friend class Tree;
};
