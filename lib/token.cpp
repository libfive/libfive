#include "token.h"

VarToken::VarToken(std::string v)
    : var(v)
{
    // Nothing to do here
}

void VarToken::toNode(Node* const n, const TokenMap& tokens) const
{
}

////////////////////////////////////////////////////////////////////////////////

ConstToken::ConstToken(double v)
    : value(v)
{
    // Nothing to do here
}

void ConstToken::toNode(Node* const n, const TokenMap& tokens) const
{
}

////////////////////////////////////////////////////////////////////////////////

OpToken::OpToken(Opcode op, Token* a, Token* b)
    : op(op), a(a), b(b)
{
    // Nothing to do here
}

void OpToken::toNode(Node* const n, const TokenMap& tokens) const
{
}
