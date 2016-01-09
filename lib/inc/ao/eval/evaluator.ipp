#include "ao/eval/evaluator.hpp"
#include "ao/eval/clause.hpp"
#include "ao/render/region.hpp"

#ifndef EVALUATOR_INCLUDE_IPP
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

inline double _abs(const double& a)
{
    return std::abs(a);
}

////////////////////////////////////////////////////////////////////////////////

#define EVAL_LOOP for (size_t i=0; i < count; ++i)

template <class T>
inline void Evaluator::evalClause(Clause* m, size_t count)
{
    switch (m->op) {
        case OP_ADD:
            EVAL_LOOP
            m->result.set<T>(m->a->get<T>(i) +
                             m->b->get<T>(i), i);
            break;
        case OP_MUL:
            EVAL_LOOP
            m->result.set<T>(m->a->get<T>(i) *
                             m->b->get<T>(i), i);
            break;
        case OP_MIN:
            EVAL_LOOP
            m->result.set<T>(_min(m->a->get<T>(i),
                                  m->b->get<T>(i)), i);
            break;
        case OP_MAX:
            EVAL_LOOP
            m->result.set<T>(_max(m->a->get<T>(i),
                                  m->b->get<T>(i)), i);
            break;
        case OP_SUB:
            EVAL_LOOP
            m->result.set<T>(m->a->get<T>(i) -
                             m->b->get<T>(i), i);
            break;
        case OP_DIV:
            EVAL_LOOP
            m->result.set<T>(m->a->get<T>(i) /
                             m->b->get<T>(i), i);
            break;
        case OP_SQRT:
            EVAL_LOOP
            m->result.set<T>(sqrt(m->a->get<T>(i)), i);
            break;
        case OP_NEG:
            EVAL_LOOP
            m->result.set<T>(-m->a->get<T>(i), i);
            break;
        case OP_ABS:
            EVAL_LOOP
            m->result.set<T>(_abs(m->a->get<T>(i)), i);
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
inline const T* Evaluator::evalCore(size_t count)
{
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            evalClause<T>(row[i], count);
        }
    }
    return root->result.ptr<T>();
}

template <class T>
inline T Evaluator::eval(T x, T y, T z)
{
    X->result.set<T>(x, 0);
    Y->result.set<T>(y, 0);
    Z->result.set<T>(z, 0);

    return evalCore<T>(1)[0];
}

template <class T>
inline void Evaluator::setPoint(T x, T y, T z, size_t index)
{
    X->result.set<T>(x, index);
    Y->result.set<T>(y, index);
    Z->result.set<T>(z, index);
}

