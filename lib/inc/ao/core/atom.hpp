#pragma once

#include <vector>
#include <iostream>

#include "ao/core/opcode.hpp"
#include "ao/core/result.hpp"

class Token;

#define ATOM_FLAG_IGNORED 1

/*
 *  An Atom represent an operation in a math tree.
 */
class Atom
{
public:
    /*
     *  Construct an atom from the given token, setting the token's
     *  atom pointer to this.
     *
     *  Requires that the token's children be packed into Atoms
     *  beforehand (otherwise will throw an assertion)
     */
    explicit Atom(Token* t);

    /*
     *  Construct an operation atom from scratch
     */
    explicit Atom(Opcode op, Atom* a=NULL, Atom* b=NULL);

    /*
     *  Construct an OP_MUTABLE atom from scratch
     */
    explicit Atom(double d);

    /*
     *  Flag manipulation functions
     */
    void setFlag(uint8_t f)     { flags |=  f; }
    void clearFlag(uint8_t f)   { flags &= ~f; }
    void clearFlags()           { flags  =  0; }

    /*
     *  If the ATOM_FLAG_IGNORED flag is set:
     *      Saves the interval result to mutable_value
     *      Clears the flag
     *      Returns true.
     *
     *  Otherwise, propagates the IGNORED flag to its children,
     *  properly handling min and max operations (which may only
     *  leave one child active), returning false.
     */
    bool checkDisabled();

    /*
     *  Print an Atom to an ostream
     */
    friend std::ostream& operator<<(std::ostream& os, const Atom& atom);

protected:
    const Opcode op;

    /*  Populated for OP_CONST atoms */
    const double value;

    /*  Populated for OP_MUTABLE atoms  */
    double mutable_value;

    /*  Flags are set during evaluation for various purposes  */
    uint8_t flags=0;

    /*  Populated for operators with arguments */
    Atom* const a;
    Atom* const b;

    /*  Results are stored in a union */
    Result result;

    friend class Tree;
};

std::ostream& operator<<(std::ostream& os, const Atom& atom);
