#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"
#include "ao/kernel/render/region.hpp"

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
inline void clause(Opcode op, T* __restrict a, T* __restrict b,
                   T* __restrict out, size_t count)
{
    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            out[i] = a[i] + b[i];
            break;
        case OP_MUL:
            EVAL_LOOP
            out[i] = a[i] * b[i];
            break;
        case OP_MIN:
            EVAL_LOOP
            out[i] = _min(a[i], b[i]);
            break;
        case OP_MAX:
            EVAL_LOOP
            out[i] = _max(a[i], b[i]);
            break;
        case OP_SUB:
            EVAL_LOOP
            out[i] = a[i] - b[i];
            break;
        case OP_DIV:
            EVAL_LOOP
            out[i] = a[i] / b[i];
            break;
        case OP_SQRT:
            EVAL_LOOP
            out[i] = sqrt(a[i]);
            break;
        case OP_NEG:
            EVAL_LOOP
            out[i] = -a[i];
            break;
        case OP_ABS:
            EVAL_LOOP
            out[i] = _abs(a[i]);
            break;
        case OP_A:
            EVAL_LOOP
            out[i] = a[i];
            break;
        case OP_B:
            EVAL_LOOP
            out[i] = b[i];
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

#ifdef __AVX__
// Partial template specialization for SIMD evaluation
#define EVAL_LOOP for (size_t i=0; i <= (count - 1)/8; ++i)
template <>
inline void clause<__m256>(Opcode op, __m256* __restrict a, __m256* __restrict b,
                           __m256* __restrict out, size_t count)
{
    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            out[i] = _mm256_add_ps(a[i], b[i]);
            break;
        case OP_MUL:
            EVAL_LOOP
            out[i] = _mm256_mul_ps(a[i], b[i]);
            break;
        case OP_MIN:
            EVAL_LOOP
            out[i] = _mm256_min_ps(a[i], b[i]);
            break;
        case OP_MAX:
            EVAL_LOOP
            out[i] = _mm256_max_ps(a[i], b[i]);
            break;
        case OP_SUB:
            EVAL_LOOP
            out[i] = _mm256_sub_ps(a[i], b[i]);
            break;
        case OP_DIV:
            EVAL_LOOP
            out[i] = _mm256_div_ps(a[i], b[i]);
            break;
        case OP_SQRT:
            EVAL_LOOP
            out[i] = _mm256_sqrt_ps(a[i]);
            break;
        case OP_NEG:
            EVAL_LOOP
            out[i] = _mm256_mul_ps(a[i], _mm256_set1_ps(-1));
            break;
        case OP_ABS:
            EVAL_LOOP
            out[i] = _mm256_andnot_ps(a[i], _mm256_set1_ps(-0.0f));
            break;
        case OP_A:
            EVAL_LOOP
            out[i] = a[i];
            break;
        case OP_B:
            EVAL_LOOP
            out[i] = b[i];
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
#endif

template <class T>
inline const T* Evaluator::evalCore(size_t count)
{
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;
            T* a = nullptr;
            T* b = nullptr;

            // Customize the opcode and result pointers depending on which
            // branches of the tree are enabled.
            if (row[i]->a)
            {
                a = row[i]->a->result.ptr<T>();
                if (row[i]->a->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_B;
                }
            }
            if (row[i]->b)
            {
                b = row[i]->b->result.ptr<T>();
                if (row[i]->b->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_A;
                }
            }

            clause<T>(op, a, b, row[i]->result.ptr<T>(), count);
        }
    }
    return root->result.ptr<T>();
}

template <class T>
inline T Evaluator::eval(T x, T y, T z)
{
    setPoint(x, y, z, 0);
    return evalCore<T>(1)[0];
}

template <class T>
inline void Evaluator::setPoint(T x, T y, T z, size_t index)
{
    X->result.set<T>(x, index);
    Y->result.set<T>(y, index);
    Z->result.set<T>(z, index);
}

