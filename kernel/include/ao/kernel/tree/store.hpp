#pragma once

#include <map>
#include <vector>
#include <array>

#include "ao/tree/opcode.hpp"

class Token;

/*
 *  A Store contains a set of Tokens with efficient routines for lookups
 */
class Store
{
public:
    /*
     *  In destructor, delete all Tokens associated with this Store
     */
    ~Store();

    /*
     *  Returns a token for the given constant
     */
    Token* constant(float v);

    /*
     *  Returns a token for the given operation
     *
     *  Arguments should be filled in from left to right
     *  (i.e. a must not be null if b is not null)
     */
    Token* operation(Opcode op, Token* a=nullptr, Token* b=nullptr);

    /*
     *  Return tokens for base variables
     */
    Token* X() { return operation(OP_X); }
    Token* Y() { return operation(OP_Y); }
    Token* Z() { return operation(OP_Z); }

    /*
     *  Set found in every token descending from root
     */
    void markFound(Token* root);

protected:
    /*
     *  Set the found member of each token to false
     */
    void clearFound();

    typedef std::pair<Token*, Token*> Key;
    typedef std::array<std::map<Key, Token*>, LAST_OP> Cache;

    /*  Constants are indexed solely by value  */
    std::map<float, Token*> constants;

    /*  Operators are indexed by weight, opcode, and arguments  */
    std::vector<Cache> ops;

    friend class Tree;
};
