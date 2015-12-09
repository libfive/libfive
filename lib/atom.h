#pragma once

#include <vector>

#include "interval.h"
#include "opcode.h"
#include "gradient.h"

class Token;

#define ATOM_ARRAY_SIZE 64

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
         *  (throwing an assertion if there are too many)
         */
        void set(const std::vector<double>& ds);
        void set(const std::vector<Interval>& is);

        double d[ATOM_ARRAY_SIZE];
        Interval i[ATOM_ARRAY_SIZE/2];
        Gradient g[ATOM_ARRAY_SIZE / 4];
    } result;

    friend class Tree;
};
