#pragma once

#include <cstdlib>

#include "ao/kernel/tree/opcode.hpp"

/*
 *  A token represents a single expression (with up to two arguments)
 */
class Token
{
public:
    /*
     *  Constructs a token for an operation
     */
    explicit Token(Opcode op, Token* a=nullptr, Token* b=nullptr);

    /*
     *  Constructs a token for a constant
     */
    explicit Token(float v);

    /*
     *  Returns the number of arguments for the given token
     */
    static size_t args(Opcode op);

    /*
     *  Returns the found flag
     */
    bool isFound() const { return found; }

    /*  Member variables  */
    const Opcode op;
    const size_t weight;

    /*  If this token is a constant, value is populated  */
    const float value;

    /*  Otherwise, pointers to arguments are stored in a and b  */
    Token* const a;
    Token* const b;

protected:
    /*  found is used to detect which tokens are in the tree  */
    bool found=false;

    friend class Store;
    friend class Tree;
    friend class Atom;
};
