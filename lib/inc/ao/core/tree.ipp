#include "ao/core/tree.hpp"
#include "ao/core/atom.hpp"

#ifndef TREE_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

////////////////////////////////////////////////////////////////////////////////
/*
 *  std::min and std::max misbehave when given Intervals, so we overload
 *  those functions with our own _min and _max (defined below for doubles)
 */
inline double _min(const double& a, const double& b)
{
    return std::min(a, b);
}

inline double _max(const double& a, const double& b)
{
    return std::max(a, b);
}

////////////////////////////////////////////////////////////////////////////////

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
            m->result.set<T>(_min(m->a->result.get<T>(i),
                                  m->b->result.get<T>(i)), i);
            break;
        case OP_MAX:
            TREE_ATOM_LOOP
            m->result.set<T>(_max(m->a->result.get<T>(i),
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
        case COND_LZ:
            TREE_ATOM_LOOP
            m->result.set<T>(m->cond->result.get<T>(i) < 0
                              ? m->a->result.get<T>(i)
                              : m->b->result.get<T>(i), i);
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
inline const T* Tree::evalCore(size_t count)
{
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            evalAtom<T>(row[i], count);
        }
    }
    return root->result.ptr<T>();
}

template <class T>
inline T Tree::eval(T x, T y, T z)
{
    X->result.set<T>(x, 0);
    Y->result.set<T>(y, 0);
    Z->result.set<T>(z, 0);

    return evalCore<T>(1)[0];
}

template <class T>
inline void Tree::setPoint(T x, T y, T z, size_t index)
{
    X->result.set<T>(x, index);
    Y->result.set<T>(y, index);
    Z->result.set<T>(z, index);
}
