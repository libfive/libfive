#pragma once

#include <cstdlib>

#include "ao/core/opcode.hpp"

class Atom;

/*
 *  A token represents a single expression (with up to two arguments)
 */
class Token
{
public:
    /*
     *  Constructs a token for an operation
     */
    Token(Opcode op, Token* a=nullptr, Token* b=nullptr);

    /*
     *  Constructs a token for a constant
     */
    Token(double v);

    /*
     *  Returns the number of arguments for the given token
     */
    static size_t args(Opcode op);

    /*
     *  Returns the found flag
     */
    bool isFound() const { return found; }

protected:
    const Opcode op;
    const size_t weight;

    /*  If this token is a constant, value is populated  */
    const double value;

    /*  Otherwise, pointers to arguments are stored in a and b  */
    Token* const a;
    Token* const b;

    /*  found is used to detect which tokens are in the tree  */
    bool found;

    /*  When packed into a Tree, store a pointer to the resulting Atom  */
    Atom* atom;

    friend class Store;
    friend class Tree;
    friend class Atom;
};
