#pragma once

#include <map>
#include <string>

#include "opcodes.h"

////////////////////////////////////////////////////////////////////////////////

class Node;
struct Token;

typedef std::map<Token*, Node*> TokenMap;

////////////////////////////////////////////////////////////////////////////////

struct Token
{
    virtual void toNode(Node* const n, const TokenMap& tokens) const=0;
};

struct VarToken : public Token
{
    VarToken(std::string v);
    void toNode(Node* const n, const TokenMap& tokens) const override;
    std::string var;
};

struct ConstToken : public Token
{
    ConstToken(double v);
    void toNode(Node* const n, const TokenMap& tokens) const override;
    double value;
};

struct OpToken : public Token
{
    OpToken(Opcode op, Token* a, Token* b);
    void toNode(Node* const n, const TokenMap& tokens) const override;
    Opcode op;
    Token* a;
    Token* b;
};
