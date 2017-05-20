#ifdef __AVX__

#include "ao/eval/evaluator_avx.hpp"

namespace Kernel {

#define EVAL_LOOP for (Result::Index i=0; i < count; ++i)
void EvaluatorAVX::eval_clause_values(Opcode::Opcode op,
        const __m256* __restrict a, const __m256* __restrict b,
              __m256* __restrict out, Result::Index count)
{
    switch (op) {
        case Opcode::ADD:
            EVAL_LOOP
            out[i] = _mm256_add_ps(a[i], b[i]);
            break;
        case Opcode::MUL:
            EVAL_LOOP
            out[i] = _mm256_mul_ps(a[i], b[i]);
            break;
        case Opcode::MIN:
            EVAL_LOOP
            out[i] = _mm256_min_ps(a[i], b[i]);
            break;
        case Opcode::MAX:
            EVAL_LOOP
            out[i] = _mm256_max_ps(a[i], b[i]);
            break;
        case Opcode::SUB:
            EVAL_LOOP
            out[i] = _mm256_sub_ps(a[i], b[i]);
            break;
        case Opcode::DIV:
            EVAL_LOOP
            out[i] = _mm256_div_ps(a[i], b[i]);
            break;

        case Opcode::SQUARE:
            EVAL_LOOP
            out[i] = _mm256_mul_ps(a[i], a[i]);
            break;
        case Opcode::SQRT:
            EVAL_LOOP
            out[i] = _mm256_sqrt_ps(a[i]);
            break;
        case Opcode::NEG:
            EVAL_LOOP
            out[i] = _mm256_sub_ps(_mm256_setzero_ps(), a[i]);
            break;

        case Opcode::CONST_VAR:
            EVAL_LOOP
            out[i] = a[i];
            break;

        // Trig functions don't have AVX equivalents, so fall back to
        // default clause evaluation
        case Opcode::ATAN2:
        case Opcode::SIN:
        case Opcode::COS:
        case Opcode::TAN:
        case Opcode::ASIN:
        case Opcode::ACOS:
        case Opcode::ATAN:
        case Opcode::EXP:
        case Opcode::POW:
        case Opcode::NTH_ROOT:
        case Opcode::MOD:
        case Opcode::NANFILL:
            EvaluatorBase::eval_clause_values(op,
                    reinterpret_cast<const float*>(a),
                    reinterpret_cast<const float*>(b),
                    reinterpret_cast<float*>(out), count*8);
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
}

//  We'll use this comparison operator, which is
//      less than
//      ordered (which defines how it handles NaNs)
//      quiet (meaning it doesn't signal on NaN)
#define CMP_LT_OQ 17

void EvaluatorAVX::eval_clause_derivs(Opcode::Opcode op,
        const __m256* __restrict av,  const __m256* __restrict adx,
        const __m256* __restrict ady, const __m256* __restrict adz,

        const __m256* __restrict bv,  const __m256* __restrict bdx,
        const __m256* __restrict bdy, const __m256* __restrict bdz,

        __m256* __restrict ov,  __m256* __restrict odx,
        __m256* __restrict ody, __m256* __restrict odz,
        Result::Index count)
{
    // Evaluate the base operations in a single pass
    eval_clause_values(op, av, bv, ov, count);

    switch (op) {
        case Opcode::ADD:
            EVAL_LOOP
            {
                odx[i] = _mm256_add_ps(adx[i], bdx[i]);
                ody[i] = _mm256_add_ps(ady[i], bdy[i]);
                odz[i] = _mm256_add_ps(adz[i], bdz[i]);
            }
            break;
        case Opcode::MUL:
            EVAL_LOOP
            {   // Product rule
                odx[i] = _mm256_add_ps(_mm256_mul_ps(av[i], bdx[i]),
                                       _mm256_mul_ps(adx[i], bv[i]));
                ody[i] = _mm256_add_ps(_mm256_mul_ps(av[i], bdy[i]),
                                       _mm256_mul_ps(ady[i], bv[i]));
                odz[i] = _mm256_add_ps(_mm256_mul_ps(av[i], bdz[i]),
                                       _mm256_mul_ps(adz[i], bv[i]));
            }
            break;
        case Opcode::MIN:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], bv[i], CMP_LT_OQ);
                odx[i] = _mm256_blendv_ps(bdx[i], adx[i], cmp);
                ody[i] = _mm256_blendv_ps(bdy[i], ady[i], cmp);
                odz[i] = _mm256_blendv_ps(bdz[i], adz[i], cmp);
            }
            break;
        case Opcode::MAX:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], bv[i], CMP_LT_OQ);
                odx[i] = _mm256_blendv_ps(adx[i], bdx[i], cmp);
                ody[i] = _mm256_blendv_ps(ady[i], bdy[i], cmp);
                odz[i] = _mm256_blendv_ps(adz[i], bdz[i], cmp);
            }
            break;
        case Opcode::SUB:
            EVAL_LOOP
            {
                odx[i] = _mm256_sub_ps(adx[i], bdx[i]);
                ody[i] = _mm256_sub_ps(ady[i], bdy[i]);
                odz[i] = _mm256_sub_ps(adz[i], bdz[i]);
            }
            break;
        case Opcode::DIV:
            EVAL_LOOP
            {
                const __m256 p = _mm256_mul_ps(bv[i], bv[i]);
                odx[i] = _mm256_div_ps(
                          _mm256_sub_ps(_mm256_mul_ps(bv[i], adx[i]),
                                        _mm256_mul_ps(av[i], bdx[i])), p);
                ody[i] = _mm256_div_ps(
                          _mm256_sub_ps(_mm256_mul_ps(bv[i], ady[i]),
                                        _mm256_mul_ps(av[i], bdy[i])), p);
                odz[i] = _mm256_div_ps(
                          _mm256_sub_ps(_mm256_mul_ps(bv[i], adz[i]),
                                        _mm256_mul_ps(av[i], bdz[i])), p);
            }
            break;
        case Opcode::SQUARE:
            EVAL_LOOP
            {
                odx[i] = _mm256_mul_ps(_mm256_set1_ps(2),
                                       _mm256_mul_ps(av[i], adx[i]));
                ody[i] = _mm256_mul_ps(_mm256_set1_ps(2),
                                       _mm256_mul_ps(av[i], ady[i]));
                odz[i] = _mm256_mul_ps(_mm256_set1_ps(2),
                                       _mm256_mul_ps(av[i], adz[i]));
            }
            break;
        case Opcode::SQRT:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], _mm256_setzero_ps(), CMP_LT_OQ);

                // Calculate the common denominator
                __m256 den = _mm256_mul_ps(ov[i], _mm256_set1_ps(2));

                // If the value is less than zero, clamp the derivative at zero
                odx[i] = _mm256_blendv_ps(
                        _mm256_div_ps(adx[i], den), _mm256_setzero_ps(), cmp);
                ody[i] = _mm256_blendv_ps(
                        _mm256_div_ps(ady[i], den), _mm256_setzero_ps(), cmp);
                odz[i] = _mm256_blendv_ps(
                        _mm256_div_ps(adz[i], den), _mm256_setzero_ps(), cmp);
            }
            break;
        case Opcode::NEG:
            EVAL_LOOP
            {
                odx[i] = _mm256_sub_ps(_mm256_setzero_ps(), adx[i]);
                ody[i] = _mm256_sub_ps(_mm256_setzero_ps(), ady[i]);
                odz[i] = _mm256_sub_ps(_mm256_setzero_ps(), adz[i]);
            }
            break;
        case Opcode::CONST_VAR:
            EVAL_LOOP
            {
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;

        // Trig functions don't have AVX equivalents, so fall back to
        // default clause evaluation
        case Opcode::ATAN2:
        case Opcode::SIN:
        case Opcode::COS:
        case Opcode::TAN:
        case Opcode::ASIN:
        case Opcode::ACOS:
        case Opcode::ATAN:
        case Opcode::EXP:
        case Opcode::POW:
        case Opcode::NTH_ROOT:
        case Opcode::MOD:
        case Opcode::NANFILL:
            EvaluatorBase::eval_clause_derivs(
                       op, reinterpret_cast<const float*>(av),
                       reinterpret_cast<const float*>(adx),
                       reinterpret_cast<const float*>(ady),
                       reinterpret_cast<const float*>(adz),

                       reinterpret_cast<const float*>(bv),
                       reinterpret_cast<const float*>(bdx),
                       reinterpret_cast<const float*>(bdy),
                       reinterpret_cast<const float*>(bdz),

                       reinterpret_cast<float*>(ov),
                       reinterpret_cast<float*>(odx),
                       reinterpret_cast<float*>(ody),
                       reinterpret_cast<float*>(odz), count*8);
            break;

        case Opcode::INVALID:
        case Opcode::CONST:
        case Opcode::VAR_X:
        case Opcode::VAR_Y:
        case Opcode::VAR_Z:
        case Opcode::VAR:
        case Opcode::LAST_OP: assert(false);
    }
}

const float* EvaluatorAVX::values(Result::Index count)
{
    count = (count - 1)/8 + 1;

    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        eval_clause_values(itr->op,
                &result.mf[itr->a][0], &result.mf[itr->b][0],
                &result.mf[itr->id][0], count);
    }

    return &result.f[tape->i][0];
}

EvaluatorBase::Derivs EvaluatorAVX::derivs(Result::Index count)
{
    Result::Index vc = (count - 1)/8 + 1;

    for (auto itr = tape->t.rbegin(); itr != tape->t.rend(); ++itr)
    {
        eval_clause_derivs(itr->op,
               &result.mf[itr->a][0], &result.mdx[itr->a][0],
               &result.mdy[itr->a][0], &result.mdz[itr->a][0],

               &result.mf[itr->b][0], &result.mdx[itr->b][0],
               &result.mdy[itr->b][0], &result.mdz[itr->b][0],

               &result.mf[itr->id][0], &result.mdx[itr->id][0],
               &result.mdy[itr->id][0], &result.mdz[itr->id][0],
               vc);
    }

    // Apply the inverse matrix transform to our normals
    // TODO: we could SIMD this as well!
    auto o = Mi * glm::vec4(0,0,0,1);
    const auto index = tape->i;
    for (size_t i=0; i < count; ++i)
    {
        auto n = Mi * glm::vec4(result.dx[index][i],
                                result.dy[index][i],
                                result.dz[index][i], 1) - o;
        result.dx[index][i] = n.x;
        result.dy[index][i] = n.y;
        result.dz[index][i] = n.z;
    }

    return { &result.f[index][0],  &result.dx[index][0],
             &result.dy[index][0], &result.dz[index][0] };
}

void EvaluatorAVX::applyTransform(Result::Index count)
{
#define MM256_LOADDUP(a) _mm256_set_ps(a,a,a,a,a,a,a,a)
    __m256 M00 = MM256_LOADDUP(M[0][0]);
    __m256 M10 = MM256_LOADDUP(M[1][0]);
    __m256 M20 = MM256_LOADDUP(M[2][0]);
    __m256 M30 = MM256_LOADDUP(M[3][0]);

    __m256 M01 = MM256_LOADDUP(M[0][1]);
    __m256 M11 = MM256_LOADDUP(M[1][1]);
    __m256 M21 = MM256_LOADDUP(M[2][1]);
    __m256 M31 = MM256_LOADDUP(M[3][1]);

    __m256 M02 = MM256_LOADDUP(M[0][2]);
    __m256 M12 = MM256_LOADDUP(M[1][2]);
    __m256 M22 = MM256_LOADDUP(M[2][2]);
    __m256 M32 = MM256_LOADDUP(M[3][2]);

    for (size_t i=0; i < (count + 7) / 8; ++i)
    {
        __m256 x = result.mf[X][i];
        __m256 y = result.mf[Y][i];
        __m256 z = result.mf[Z][i];
        result.mf[X][i] = _mm256_add_ps(
            _mm256_add_ps(
                _mm256_mul_ps(x, M00), _mm256_mul_ps(y, M10)),
            _mm256_add_ps(_mm256_mul_ps(z, M20), M30));
        result.mf[Y][i] = _mm256_add_ps(
            _mm256_add_ps(
                _mm256_mul_ps(x, M01), _mm256_mul_ps(y, M11)),
            _mm256_add_ps(_mm256_mul_ps(z, M21), M31));
        result.mf[Z][i] = _mm256_add_ps(
            _mm256_add_ps(
                _mm256_mul_ps(x, M02), _mm256_mul_ps(y, M12)),
            _mm256_add_ps(_mm256_mul_ps(z, M22), M32));
    }
}

#endif

}   // namespace Kernel
