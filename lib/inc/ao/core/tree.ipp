#include "ao/core/tree.hpp"
#include "ao/core/atom.hpp"

#ifndef TREE_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

#define TREE_ATOM_LOOP for (size_t i=0; i < count; ++i)

////////////////////////////////////////////////////////////////////////////////
/*
 *  std::min and std::max misbehave when given Intervals, so we overload
 *  those functions with partial template specialization here
 */
template<class T>
inline T _min(const T& a, const T& b)
{
    return std::min(a, b);
}

template<class T>
inline T _max(const T& a, const T& b)
{
    return std::max(a, b);
}

template <>
inline Interval _min<Interval>(const Interval& a, const Interval& b)
{
    return boost::numeric::min(a, b);
}

template <>
inline Interval _max<Interval>(const Interval& a, const Interval& b)
{
    return boost::numeric::max(a, b);
}

////////////////////////////////////////////////////////////////////////////////

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
        for (size_t i=0; i < row.active; ++i)
        {
            evalAtom<T>(row[i], count);
        }
    }
}

template <class T>
inline T Tree::eval(T x, T y, T z)
{
    X->result.set<T>(x, 0);
    Y->result.set<T>(y, 0);
    Z->result.set<T>(z, 0);

    evalCore<T>(1);
    return root->result.get<T>(0);
}
