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
            auto a = var_names.find(n.id());
            auto d = var_docs.find(n.id());
            serializeString(a == var_names.end() ? "" : a->second, out);
            serializeString(d == var_docs.end()  ? "" : d->second, out);
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
    (void)data;
    return Template(Tree::X());
}

}   // namespace Kernel
