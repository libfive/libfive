#include "tree.hpp"
#include "atom.hpp"

template <class T>
inline void Tree::load_if(Atom* a, const std::vector<T>& vs,
                          size_t index, size_t count)
{
    if (a)
    {
        a->result.set(&vs[index], count);
    }
}

template <class T>
inline void Tree::eval_atom(Atom* m, size_t i)
{
    switch (m->op) {
        case OP_ADD:
            m->result.set<T>(m->a->result.get<T>(i) +
                             m->b->result.get<T>(i), i);
            break;
        case OP_MUL:
            m->result.set<T>(m->a->result.get<T>(i) *
                             m->b->result.get<T>(i), i);
            break;
        case OP_MIN:
            m->result.set<T>(std::min(m->a->result.get<T>(i),
                                      m->b->result.get<T>(i)), i);
            break;
        case OP_MAX:
            m->result.set<T>(std::max(m->a->result.get<T>(i),
                                      m->b->result.get<T>(i)), i);
            break;
        case OP_SUB:
            m->result.set<T>(m->a->result.get<T>(i) -
                             m->b->result.get<T>(i), i);
            break;
        case OP_DIV:
            m->result.set<T>(m->a->result.get<T>(i) /
                             m->b->result.get<T>(i), i);
            break;
        case OP_SQRT:
            m->result.set<T>(sqrt(m->a->result.get<T>(i)), i);
            break;
        case OP_NEG:
            m->result.set<T>(-m->a->result.get<T>(i), i);
            break;
        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}

template <class T>
inline void Tree::eval_core(size_t count)
{
    for (const auto& row : rows)
    {
        for (auto& m : row)
        {
            for (size_t i=0; i < count; ++i)
            {
                eval_atom<T>(m, i);
            }
        }
    }
}

template <class T>
inline std::vector<T> Tree::eval(const std::vector<T>& x,
                                 const std::vector<T>& y,
                                 const std::vector<T>& z)
{
    assert(x.size() == y.size() && x.size() == z.size());

    size_t remaining = x.size();

    std::vector<T> out(remaining);
    size_t index = 0;

    while (remaining)
    {
        const size_t count = std::min(remaining, ATOM_ARRAY_BYTES / sizeof(T));

        load_if(X, x, index, count);
        load_if(Y, y, index, count);
        load_if(Z, z, index, count);

        eval_core<T>(count);

        remaining -= count;
        root->result.copy_to(&out[index], count);
        index += count;
    }
    return out;
}

template <class T>
inline T Tree::eval(T x, T y, T z)
{
    return eval(std::vector<T>(1, x),
                std::vector<T>(1, y),
                std::vector<T>(1, z))[0];
}

template <class T>
void Tree::mode()
{
    const size_t count = ATOM_ARRAY_BYTES / sizeof(T);
    for (auto c : constants)
    {
        c->result.set(std::vector<T>(count, T(c->value)));
    }
}
