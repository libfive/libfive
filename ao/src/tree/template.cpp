#include <iostream>

#include "ao/tree/template.hpp"

namespace Kernel
{

std::vector<uint8_t> Template::serialize() const
{
    static_assert(Opcode::LAST_OP <= 255, "Too many opcodes");

    std::vector<uint8_t> out;
    out.push_back('T');
    serializeString(name, out);
    serializeString(doc, out);

    std::map<Tree::Id, uint32_t> ids;

    for (auto& n : tree.ordered())
    {
        out.push_back(n->op);
        ids.insert({n.id(), ids.size()});

        // Write constants as raw bytes
        if (n->op == Opcode::CONST)
        {
            serializeBytes(n->value, out);
        }
        else if (n->op == Opcode::VAR)
        {
            auto a = vars.find(n.id());
            serializeString(a == vars.end() ? "" : a->second, out);
        }
        switch (Opcode::args(n->op))
        {
            case 2:  serializeBytes(ids.at(n->rhs.get()), out);    // FALLTHROUGH
            case 1:  serializeBytes(ids.at(n->lhs.get()), out);    // FALLTHROUGH
            default: break;
        }
    }
    return out;
}

void Template::serializeString(const std::string& s, std::vector<uint8_t>& out)
{
    out.push_back('"');
    for (auto& c : s)
    {
        if (c == '"' || c == '\\')
        {
            out.push_back('\\');
        }
        out.push_back(c);
    }
    out.push_back('"');
}

Template Template::deserialize(const std::vector<uint8_t>& data)
{
    auto cData = data;
    const uint8_t* pos = &(*cData.begin());
    const uint8_t* end = &(cData.back());
    end++;
    Template out(Tree::Invalid());

#define REQUIRE(cond) \
    if (!(cond)) \
    { \
        std::cerr << "Template::deserialize: expected " << #cond \
                  << " at " << __LINE__ << std::endl; \
        return out; \
    }
#define CHECK_POS() REQUIRE(pos != end)

    CHECK_POS();
    REQUIRE(*pos++ == 'T');
    CHECK_POS();

    {   // Store name and docstring, though they may be blank
        out.name = deserializeString(pos, end);
        CHECK_POS();
        out.doc = deserializeString(pos, end);
    }

    std::map<uint32_t, Tree> ts;
    while (pos != end)
    {
        CHECK_POS();
        auto op = Opcode::Opcode(*pos++);
        REQUIRE(op > Opcode::INVALID);
        REQUIRE(op < Opcode::LAST_OP);
        CHECK_POS();

        auto args = Opcode::args(op);
        auto next = ts.size();
        if (op == Opcode::CONST)
        {
            float v = deserializeBytes<float>(pos, end);
            ts.insert({next, Tree(v)});
        }
        else if (op == Opcode::VAR)
        {
            std::string var = deserializeString(pos, end);
            auto v = Tree(op);
            out.vars.insert({v.id(), var});
            ts.insert({next, v});
        }
        else if (args == 2)
        {
            auto rhs = deserializeBytes<uint32_t>(pos, end);
            auto lhs = deserializeBytes<uint32_t>(pos, end);
            ts.insert({next, Tree(op, ts.at(lhs), ts.at(rhs))});
        }
        else if (args == 1)
        {
            auto lhs = deserializeBytes<uint32_t>(pos, end);
            ts.insert({next, Tree(op, ts.at(lhs))});
        }
        else
        {
            ts.insert({next, Tree(op)});
        }
    }

    out.tree = ts.at(ts.size() - 1);
    return out;
}

std::string Template::deserializeString(const uint8_t*& pos, const uint8_t* end)
{
    std::string out;
    if (pos == end)
    {
        std::cerr << "Template::deserializeString: EOF at beginning of string"
                  << std::endl;
    }
    else if (*pos++ != '"')
    {
        std::cerr << "Template::deserializeString: expected opening \""
                  << std::endl;
    }
    else
    {
        while (pos != end)
        {
            auto c = *pos++;
            if (c == '"')
            {
                break;
            }
            else if (c == '\\')
            {
                if (pos != end)
                {
                    out.push_back(*pos++);
                }
            }
            else
            {
                out.push_back(c);
            }
        }
    }
    return out;
}

}   // namespace Kernel
