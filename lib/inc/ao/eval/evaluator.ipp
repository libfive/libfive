#include "ao/eval/evaluator.hpp"
#include "ao/eval/clause.hpp"
#include "ao/render/region.hpp"

#ifndef EVALUATOR_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

////////////////////////////////////////////////////////////////////////////////
/*
 *  std::min and std::max misbehave when given Intervals, so we overload
 *  those functions with our own _min and _max (defined below for floats)
 */
inline float _min(const float& a, const float& b)
{
    return std::min(a, b);
}

inline float _max(const float& a, const float& b)
{
    return std::max(a, b);
}

inline float _abs(const float& a)
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
#undef EVAL_LOOP

// Partial template specialization for SIMD evaluation
#define EVAL_LOOP for (size_t i=0; i < 32; ++i)
template <>
inline void Evaluator::evalClause<__m256>(Clause* m, size_t count)
{
    switch (m->op) {
        case OP_ADD:
            EVAL_LOOP
            m->result.set(_mm256_add_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_MUL:
            EVAL_LOOP
            m->result.set(_mm256_mul_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_MIN:
            EVAL_LOOP
            m->result.set(_mm256_min_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_MAX:
            EVAL_LOOP
            m->result.set(_mm256_max_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_SUB:
            EVAL_LOOP
            m->result.set(_mm256_sub_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_DIV:
            EVAL_LOOP
            m->result.set(_mm256_div_ps(m->a->get<__m256>(i),
                                        m->b->get<__m256>(i)), i);
            break;
        case OP_SQRT:
            EVAL_LOOP
            m->result.set(_mm256_sqrt_ps(m->a->get<__m256>(i)), i);
            break;
            /*
        case OP_NEG:
            EVAL_LOOP
            m->result.m[i] = _mm256_neg_ps(m->a->result.m[i]);
            break;
        case OP_ABS:
            EVAL_LOOP
            m->result.m[i] = _mm256_abs_ps(m->a->result.m[i]);
            break;
            */
        case INVALID:
        case OP_CONST:
        case OP_MUTABLE:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case LAST_OP: assert(false);
    }
}
#undef EVAL_LOOP

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

