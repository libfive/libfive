#include "ao/core/atom.hpp"
#include "ao/core/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Atom::Atom(Token* t)
    : op(t->op), value(t->value), mutable_value(nan("")),
      a(t->a ? t->a->atom : nullptr),
      b(t->b ? t->b->atom : nullptr),
      cond(t->cond ? t->cond->atom : nullptr)
{

    // Assert that children have atom pointers populated
    assert(t->a ? t->a->atom != nullptr : true);
    assert(t->b ? t->b->atom != nullptr : true);
    assert(t->cond ? t->cond->atom != nullptr : true);

    // Assert that this token hasn't already been added to the tree
    assert(t->atom == nullptr);

    // If this is a constant Atom, fill the result arrays with its value
    if (op == OP_CONST)
    {
        result.fill(value);
    }

    t->atom = this;
}

Atom::Atom(Opcode op, Atom* a, Atom* b)
    : op(op), value(nan("")), mutable_value(nan("")), a(a), b(b), cond(nullptr)
{
    // Nothing to do here
}

Atom::Atom(double value)
    : op(OP_MUTABLE), value(nan("")), mutable_value(value),
      a(nullptr), b(nullptr), cond(nullptr)
{
    result.fill(value);
}

////////////////////////////////////////////////////////////////////////////////

bool Atom::checkDisabled()
{
    if (flags & ATOM_FLAG_IGNORED)
    {
        clearFlags();
        mutable_value = result.get<Interval>(0).lower();
        return true;
    }

    // For min and max operations, we may only need to keep one branch
    // active if it is decisively above or below the other branch.
    if (op == OP_MAX)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    else if (op == OP_MIN)
    {
        if (a->result.get<Interval>(0).lower() >=
            b->result.get<Interval>(0).upper())
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
        else if (b->result.get<Interval>(0).lower() >=
                 a->result.get<Interval>(0).upper())
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        else
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    // For other operations, we keep both branches active
    else
    {
        if (a)
        {
            a->clearFlag(ATOM_FLAG_IGNORED);
        }
        if (b)
        {
            b->clearFlag(ATOM_FLAG_IGNORED);
        }
    }
    return false;
}

////////////////////////////////////////////////////////////////////////////////

void Atom::cacheResult()
{
    result.fill(result.get<Interval>(0).lower());
}

////////////////////////////////////////////////////////////////////////////////

std::string Atom::toShader(size_t index,
                           std::unordered_map<const Atom*, size_t>* atoms) const
{
    // Each atom should be stored into the hashmap only once.
    // There's a special case for the tree's root, which is pre-emptively
    // inserted into the hashmap at index 0 (to make the end of the shader
    // easy to write).
    assert(atoms->count(this) == 0 || (*atoms)[this] == 0);

    // Store this atom in the array if it is not already present;
    // otherwise, update the index from the hashmap
    if (!atoms->count(this))
    {
        (*atoms)[this] = index;
    }
    else
    {
        index = (*atoms)[this];
    }

    std::string out = "    float m" + std::to_string(index) + " = ";

    auto get = [&](Atom* m){
        if (m)
        {
            auto itr = atoms->find(m);
            if (itr != atoms->end())
            {
                return "m" + std::to_string(itr->second);
            }
            else switch (m->op)
            {
                case OP_X: return std::string("x");
                case OP_Y: return std::string("y");
                case OP_Z: return std::string("z");
                case OP_CONST:  return std::to_string(m->value) + "f";
                default:        return std::to_string(m->mutable_value) + "f";
            }
        }
        return std::string(); };
    std::string sa = get(a);
    std::string sb = get(b);

    switch (op)
    {
        case OP_ADD:    out += "(" + sa + " + " + sb + ")";     break;
        case OP_MUL:    out += "(" + sa + " * " + sb + ")";     break;
        case OP_MIN:    out += "min(" + sa + ", " + sb + ")";   break;
        case OP_MAX:    out += "max(" + sa + ", " + sb + ")";   break;
        case OP_SUB:    out += "(" + sa + " - " + sb + ")";     break;
        case OP_DIV:    out += "(" + sa + " / " + sb + ")";     break;
        case OP_SQRT:   out += "sqrt(" + sa + ", " + sb + ")";  break;
        case OP_NEG:    out += "(-" + sa + ", " + sb + ")";     break;

        case OP_X:  // Fallthrough!
        case OP_Y:
        case OP_Z:
        case LAST_OP:
        case OP_CONST:
        case OP_MUTABLE:
        case INVALID:   assert(false);
    }

    return out + ";\n";
}

////////////////////////////////////////////////////////////////////////////////

std::ostream& operator<<(std::ostream& os, const Atom& atom)
{
    switch (atom.op)
    {
        case OP_ADD:    os << "(" << *atom.a << " + " << *atom.b << ")"; break;
        case OP_MUL:    os << "(" << *atom.a << " * " << *atom.b << ")"; break;
        case OP_MIN:    os << "min(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_MAX:    os << "max(" << *atom.a << ", " << *atom.b << ")"; break;
        case OP_SUB:    os << "(" << *atom.a << " - " << *atom.b << ")"; break;
        case OP_DIV:    os << "(" << *atom.a << " / " << *atom.b << ")"; break;
        case OP_SQRT:   os << "sqrt(" << *atom.a << ")"; break;
        case OP_NEG:    os << "(-" << *atom.a << ")"; break;
        case OP_CONST:  os << atom.value; break;
        case OP_MUTABLE:  os << atom.mutable_value; break;
        case OP_X:      os << "X"; break;
        case OP_Y:      os << "Y"; break;
        case OP_Z:      os << "Z"; break;

        case LAST_OP:   // Fallthrough!
        case INVALID:   assert(false);
    }
    return os;
}
