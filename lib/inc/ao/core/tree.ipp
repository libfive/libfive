#include "ao/core/tree.hpp"
#include "ao/core/atom.hpp"

#ifndef TREE_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

#define TREE_ATOM_LOOP for (size_t i=0; i < count; ++i)

template <class T>
inline void Tree::evalAtom(Atom* m, size_t count)
{
    switch (m->op) {
        case OP_ADD:
            TREE_ATOM_LOOP
            m->result.set<T>(m->a->result.get<T>(i) +
                             m->b->result.get<T>(i), i);
            break;
        case OP_MUL:
            TREE_ATOM_LOOP
            m->result.set<T>(m->a->result.get<T>(i) *
                             m->b->result.get<T>(i), i);
            break;
        case OP_MIN:
            TREE_ATOM_LOOP
            m->result.set<T>(std::min(m->a->result.get<T>(i),
                                      m->b->result.get<T>(i)), i);
            break;
        case OP_MAX:
            TREE_ATOM_LOOP
            m->result.set<T>(std::max(m->a->result.get<T>(i),
                                      m->b->result.get<T>(i)), i);
            break;
        case OP_SUB:
            TREE_ATOM_LOOP
            m->result.set<T>(m->a->result.get<T>(i) -
                             m->b->result.get<T>(i), i);
            break;
        case OP_DIV:
            TREE_ATOM_LOOP
            m->result.set<T>(m->a->result.get<T>(i) /
                             m->b->result.get<T>(i), i);
            break;
        case OP_SQRT:
            TREE_ATOM_LOOP
            m->result.set<T>(sqrt(m->a->result.get<T>(i)), i);
            break;
        case OP_NEG:
            TREE_ATOM_LOOP
            m->result.set<T>(-m->a->result.get<T>(i), i);
            break;
        case INVALID:
        case OP_CONST:
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}

template <class T>
inline void Tree::evalCore(size_t count)
{
    for (const auto& row : rows)
    {
        for (auto& m : row)
        {
            evalAtom<T>(m, count);
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

    setMode<T>();
    while (remaining)
    {
        const size_t count = std::min(remaining, Result::count<T>());

        X->result.set<T>(&x[index], count);
        Y->result.set<T>(&y[index], count);
        Z->result.set<T>(&z[index], count);

        evalCore<T>(count);

        remaining -= count;
        root->result.copyTo(&out[index], count);
        index += count;
    }
    return out;
}

template <class T>
inline T Tree::eval(T x, T y, T z)
{
    setMode<T>();

    X->result.set<T>(x, 0);
    Y->result.set<T>(y, 0);
    Z->result.set<T>(z, 0);

    evalCore<T>(1);
    return root->result.get<T>(0);
}

template <class T>
void Tree::setMode()
{
    // Skip this function if mode is already set correctly
    if (mode != sizeof(T))
    {
        fillConstants<T>();
        fillMatrix<T>();
        mode = static_cast<Mode>(sizeof(T));
    }
}

template <class T>
void Tree::fillConstants()
{
    const size_t count = Result::count<T>();
    for (auto c : constants)
    {
        c->result.set(std::vector<T>(count, T(c->value)));
    }
}

template <class T>
void Tree::fillMatrix()
{
    const size_t count = Result::count<T>();
    for (auto m : matrix)
    {
        m->result.set(std::vector<T>(count, T(m->mutable_value)));
    }
}
