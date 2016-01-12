#pragma once

#include <vector>
#include <iostream>
#include <unordered_map>

#include "ao/tree/opcode.hpp"

class Token;

/*
 *  An Atom represent an operation in a math tree.
 */
class Atom
{
public:
    /*
     *  Construct an atom from the given token, storing atoms[t] = this
     *
     *  Requires that the token's children be packed into Atoms
     *  beforehand (otherwise will throw an assertion)
     */
    explicit Atom(const Token* t,
                  std::unordered_map<const Token*, Atom*>& atoms);

    /*
     *  Construct an operation atom from scratch
     */
    explicit Atom(Opcode op, Atom* a=NULL, Atom* b=NULL);

    /*
     *  Constructs an OP_MUTABLE atom with the given value
     */
    explicit Atom(float d);

    /*
     *  Print an Atom to an ostream
     */
    friend std::ostream& operator<<(std::ostream& os, const Atom& atom);

protected:
    /*  Opcode for this atom  */
    const Opcode op;

    /*  Populated for OP_CONST atoms */
    const float value;

    /*  Populated for operators with arguments */
    Atom* const a;
    Atom* const b;

    friend class Tree;
    friend class Clause;
};

std::ostream& operator<<(std::ostream& os, const Atom& atom);
