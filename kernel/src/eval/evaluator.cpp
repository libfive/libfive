/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <numeric>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

////////////////////////////////////////////////////////////////////////////////

Evaluator::Evaluator(const Tree* tree)
{
    // Count up the number of Atoms in the Tree
    size_t count =  std::accumulate(tree->rows.begin(), tree->rows.end(),
            3                           // X, Y, Z
            + tree->matrix.size()       // Transform matrix
            + tree->constants.size(),   // Constants
            [](size_t i, const std::vector<Atom*>& r){ return i + r.size(); });

    // Then, allocate space for them (ensuring alignment if AVX is used)
    Clause* ptr;
#if __AVX__
    {   // Ensure that we have 32-byte alignment for Clauses and Results
        size_t alignment = 32;
        size_t bytes = sizeof(Clause) * count + alignment;
        void* buf = malloc(bytes);
        data = static_cast<Clause*>(buf);
        ptr = static_cast<Clause*>(
                std::align(alignment, sizeof(Clause) * count, buf, bytes));
    }
#else
    data = static_cast<Clause*>(malloc(sizeof(Clause) * count));
    ptr = data;
#endif

    // Helper function to create a new clause in the data array
    std::unordered_map<const Atom*, Clause*> clauses;
    auto newClause = [&ptr, &clauses](const Atom* m)
        { return new (ptr++) Clause(m, clauses); };

    // Load constants into the array first
    for (auto m : tree->constants)
    {
        constants.push_back(newClause(m));
    }

    // Create base clauses X, Y, Z
    X = newClause(tree->X);
    Y = newClause(tree->Y);
    Z = newClause(tree->Z);

    // Set derivatives for X, Y, Z (since these never change)
    X->result.deriv(1, 0, 0);
    Y->result.deriv(0, 1, 0);
    Z->result.deriv(0, 0, 1);

    // Create matrix clauses
    assert(tree->matrix.size() == matrix.size());
    for (size_t i=0; i < tree->matrix.size(); ++i)
    {
        matrix[i] = newClause(tree->matrix[i]);
    }

    // Finally, create the rest of the Tree's clauses
    for (auto row : tree->rows)
    {
        rows.push_back(Row());
        for (auto atom : row)
        {
            rows.back().push_back(newClause(atom));
        }
        rows.back().setSize();
    }

    assert(clauses[tree->root]);
    root = clauses[tree->root];
}

Evaluator::Evaluator(const Tree* t, const glm::mat4& m)
    : Evaluator(t)
{
    setMatrix(m);
}

Evaluator::~Evaluator()
{
    free(data);
}

////////////////////////////////////////////////////////////////////////////////

void Evaluator::setMatrix(const glm::mat4& m)
{
    // Though the matrix values are of opcode OP_CONST, we're going to
    // hot-patch the result arrays (which are actually used in computation
    // to apply the given transform matrix).
    size_t index = 0;
    for (int i=0; i < 3; ++i)
    {
        for (int j=0; j < 4; ++j)
        {
            matrix[index++]->result.fill(m[j][i]);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

float Evaluator::eval(float x, float y, float z)
{
    set(x, y, z, 0);
    return values(1)[0];
}

Interval Evaluator::eval(Interval x, Interval y, Interval z)
{
    set(x, y, z);
    return interval();
}

void Evaluator::set(Interval x, Interval y, Interval z)
{
    X->result.set(x);
    Y->result.set(y);
    Z->result.set(z);
}

////////////////////////////////////////////////////////////////////////////////

void Evaluator::push()
{
    // Walk up the tree, marking every atom with ATOM_FLAG_IGNORED
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            row[i]->setFlag(CLAUSE_FLAG_IGNORED);
        }
    }

    // Clear the IGNORED flag on the root
    root->clearFlag(CLAUSE_FLAG_IGNORED);

    // Walk down the tree, clearing IGNORED flags as appropriate
    // and disabling atoms that still have IGNORED flags set.
    for (auto itr = rows.rbegin(); itr != rows.rend(); ++itr)
    {
        itr->push();
    }
}

void Evaluator::pop()
{
    for (auto& row : rows)
    {
        row.pop();
    }
}

////////////////////////////////////////////////////////////////////////////////

#define EVAL_LOOP for (size_t i=0; i < count; ++i)
static void clause(Opcode op,
        const float* __restrict a, const float* __restrict b,
        float* __restrict out, size_t count)
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
            out[i] = fmin(a[i], b[i]);
            break;
        case OP_MAX:
            EVAL_LOOP
            out[i] = fmax(a[i], b[i]);
            break;
        case OP_SUB:
            EVAL_LOOP
            out[i] = a[i] - b[i];
            break;
        case OP_DIV:
            EVAL_LOOP
            out[i] = a[i] / b[i];
            break;
        case OP_ATAN2:
            EVAL_LOOP
            out[i] = atan2(a[i], b[i]);
            break;
        case OP_MOD:
            EVAL_LOOP
            {
                out[i] = std::fmod(a[i], b[i]);
                while (out[i] < 0)
                {
                    out[i] += b[i];
                }
            }
            break;
        case OP_NANFILL:
            EVAL_LOOP
            out[i] = isnan(a[i]) ? b[i] : a[i];
            break;

        case OP_SQUARE:
            EVAL_LOOP
            out[i] = a[i] * a[i];
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
            out[i] = fabs(a[i]);
            break;
        case OP_SIN:
            EVAL_LOOP
            out[i] = sin(a[i]);
            break;
        case OP_COS:
            EVAL_LOOP
            out[i] = cos(a[i]);
            break;
        case OP_TAN:
            EVAL_LOOP
            out[i] = tan(a[i]);
            break;
        case OP_ASIN:
            EVAL_LOOP
            out[i] = asin(a[i]);
            break;
        case OP_ACOS:
            EVAL_LOOP
            out[i] = acos(a[i]);
            break;
        case OP_ATAN:
            EVAL_LOOP
            out[i] = atan(a[i]);
            break;
        case OP_EXP:
            EVAL_LOOP
            out[i] = exp(a[i]);
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
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
}

static void clause(Opcode op,
        const float* __restrict av,  const float* __restrict adx,
        const float* __restrict ady, const float* __restrict adz,

        const float* __restrict bv,  const float* __restrict bdx,
        const float* __restrict bdy, const float* __restrict bdz,

        float* __restrict ov,  float* __restrict odx,
        float* __restrict ody, float* __restrict odz,
        size_t count)
{
    // Evaluate the base operations in a single pass
    clause(op, av, bv, ov, count);

    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            {
                odx[i] = adx[i] + bdx[i];
                ody[i] = ady[i] + bdy[i];
                odz[i] = adz[i] + bdz[i];
            }
            break;
        case OP_MUL:
            EVAL_LOOP
            {   // Product rule
                odx[i] = av[i]*bdx[i] + adx[i]*bv[i];
                ody[i] = av[i]*bdy[i] + ady[i]*bv[i];
                odz[i] = av[i]*bdz[i] + adz[i]*bv[i];
            }
            break;
        case OP_MIN:
            EVAL_LOOP
            {
                if (av[i] < bv[i])
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
                else
                {
                    odx[i] = bdx[i];
                    ody[i] = bdy[i];
                    odz[i] = bdz[i];
                }
            }
            break;
        case OP_MAX:
            EVAL_LOOP
            {
                if (av[i] < bv[i])
                {
                    odx[i] = bdx[i];
                    ody[i] = bdy[i];
                    odz[i] = bdz[i];
                }
                else
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
            }
            break;
        case OP_SUB:
            EVAL_LOOP
            {
                odx[i] = adx[i] - bdx[i];
                ody[i] = ady[i] - bdy[i];
                odz[i] = adz[i] - bdz[i];
            }
            break;
        case OP_DIV:
            EVAL_LOOP
            {
                const float p = pow(bv[i], 2);
                odx[i] = (bv[i]*adx[i] - av[i]*bdx[i]) / p;
                ody[i] = (bv[i]*ady[i] - av[i]*bdy[i]) / p;
                odz[i] = (bv[i]*adz[i] - av[i]*bdz[i]) / p;
            }
            break;
        case OP_ATAN2:
            EVAL_LOOP
            {
                const float d = pow(av[i], 2) + pow(bv[i], 2);
                odx[i] = (adx[i]*bv[i] - av[i]*bdx[i]) / d;
                ody[i] = (ady[i]*bv[i] - av[i]*bdy[i]) / d;
                odz[i] = (adz[i]*bv[i] - av[i]*bdz[i]) / d;
            }
            break;
        case OP_MOD:
            EVAL_LOOP
            {
                // This isn't quite how partial derivatives of mod work,
                // but close enough normals rendering.
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;
        case OP_NANFILL:
            EVAL_LOOP
            {
                odx[i] = isnan(av[i]) ? bdx[i] : adx[i];
                ody[i] = isnan(av[i]) ? bdy[i] : ady[i];
                odz[i] = isnan(av[i]) ? bdz[i] : adz[i];
            }
            break;

        case OP_SQUARE:
            EVAL_LOOP
            {
                odx[i] = 2 * av[i] * adx[i];
                ody[i] = 2 * av[i] * ady[i];
                odz[i] = 2 * av[i] * adz[i];
            }
            break;
        case OP_SQRT:
            EVAL_LOOP
            {
                if (av[i] < 0)
                {
                    odx[i] = 0;
                    ody[i] = 0;
                    odz[i] = 0;
                }
                else
                {
                    odx[i] = adx[i] / (2 * ov[i]);
                    ody[i] = ady[i] / (2 * ov[i]);
                    odz[i] = adz[i] / (2 * ov[i]);
                }
            }
            break;
        case OP_NEG:
            EVAL_LOOP
            {
                odx[i] = -adx[i];
                ody[i] = -ady[i];
                odz[i] = -adz[i];
            }
            break;
        case OP_ABS:
            EVAL_LOOP
            {
                if (av[i] < 0)
                {
                    odx[i] = -adx[i];
                    ody[i] = -ady[i];
                    odz[i] = -adz[i];
                }
                else
                {
                    odx[i] = adx[i];
                    ody[i] = ady[i];
                    odz[i] = adz[i];
                }
            }
            break;
        case OP_SIN:
            EVAL_LOOP
            {
                const float c = cos(av[i]);
                odx[i] = adx[i] * c;
                ody[i] = ady[i] * c;
                odz[i] = adz[i] * c;
            }
            break;
        case OP_COS:
            EVAL_LOOP
            {
                const float s = -sin(av[i]);
                odx[i] = adx[i] * s;
                ody[i] = ady[i] * s;
                odz[i] = adz[i] * s;
            }
            break;
        case OP_TAN:
            EVAL_LOOP
            {
                const float s = pow(1/cos(av[i]), 2);
                odx[i] = adx[i] * s;
                ody[i] = ady[i] * s;
                odz[i] = adz[i] * s;
            }
            break;
        case OP_ASIN:
            EVAL_LOOP
            {
                const float d = sqrt(1 - pow(av[i], 2));
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case OP_ACOS:
            EVAL_LOOP
            {
                const float d = -sqrt(1 - pow(av[i], 2));
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case OP_ATAN:
            EVAL_LOOP
            {
                const float d = pow(av[i], 2) + 1;
                odx[i] = adx[i] / d;
                ody[i] = ady[i] / d;
                odz[i] = adz[i] / d;
            }
            break;
        case OP_EXP:
            EVAL_LOOP
            {
                const float e = exp(av[i]);
                odx[i] = e * adx[i];
                ody[i] = e * ady[i];
                odz[i] = e * adz[i];
            }
            break;

        case OP_A:
            EVAL_LOOP
            {
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;
        case OP_B:
            EVAL_LOOP
            {
                odx[i] = bdx[i];
                ody[i] = bdy[i];
                odz[i] = bdz[i];
            }
            break;
        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
}

#ifdef __AVX__
static void clause(Opcode op,
        const __m256* __restrict a, const __m256* __restrict b,
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

        case OP_SQUARE:
            EVAL_LOOP
            out[i] = _mm256_mul_ps(a[i], a[i]);
            break;
        case OP_SQRT:
            EVAL_LOOP
            out[i] = _mm256_sqrt_ps(a[i]);
            break;
        case OP_NEG:
            EVAL_LOOP
            out[i] = _mm256_sub_ps(_mm256_setzero_ps(), a[i]);
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

        // Trig functions don't have AVX equivalents, so fall back to
        // default clause evaluation
        case OP_ATAN2:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_EXP:
        case OP_MOD:
        case OP_NANFILL:
            clause(op, reinterpret_cast<const float*>(a),
                       reinterpret_cast<const float*>(b),
                       reinterpret_cast<float*>(out), count*8);
            break;

        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
}

//  We'll use this comparison operator, which is
//      less than
//      ordered (which defines how it handles NaNs)
//      quiet (meaning it doesn't signal on NaN)
#define CMP_LT_OQ 17

static void clause(Opcode op,
        const __m256* __restrict av,  const __m256* __restrict adx,
        const __m256* __restrict ady, const __m256* __restrict adz,

        const __m256* __restrict bv,  const __m256* __restrict bdx,
        const __m256* __restrict bdy, const __m256* __restrict bdz,

        __m256* __restrict ov,  __m256* __restrict odx,
        __m256* __restrict ody, __m256* __restrict odz,
        size_t count)
{
    // Evaluate the base operations in a single pass
    clause(op, av, bv, ov, count);

    switch (op) {
        case OP_ADD:
            EVAL_LOOP
            {
                odx[i] = _mm256_add_ps(adx[i], bdx[i]);
                ody[i] = _mm256_add_ps(ady[i], bdy[i]);
                odz[i] = _mm256_add_ps(adz[i], bdz[i]);
            }
            break;
        case OP_MUL:
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
        case OP_MIN:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], bv[i], CMP_LT_OQ);
                odx[i] = _mm256_blendv_ps(bdx[i], adx[i], cmp);
                ody[i] = _mm256_blendv_ps(bdy[i], ady[i], cmp);
                odz[i] = _mm256_blendv_ps(bdz[i], adz[i], cmp);
            }
            break;
        case OP_MAX:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], bv[i], CMP_LT_OQ);
                odx[i] = _mm256_blendv_ps(adx[i], bdx[i], cmp);
                ody[i] = _mm256_blendv_ps(ady[i], bdy[i], cmp);
                odz[i] = _mm256_blendv_ps(adz[i], bdz[i], cmp);
            }
            break;
        case OP_SUB:
            EVAL_LOOP
            {
                odx[i] = _mm256_sub_ps(adx[i], bdx[i]);
                ody[i] = _mm256_sub_ps(ady[i], bdy[i]);
                odz[i] = _mm256_sub_ps(adz[i], bdz[i]);
            }
            break;
        case OP_DIV:
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
        case OP_SQUARE:
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
        case OP_SQRT:
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
        case OP_NEG:
            EVAL_LOOP
            {
                odx[i] = _mm256_sub_ps(_mm256_setzero_ps(), adx[i]);
                ody[i] = _mm256_sub_ps(_mm256_setzero_ps(), ady[i]);
                odz[i] = _mm256_sub_ps(_mm256_setzero_ps(), adz[i]);
            }
            break;
        case OP_ABS:
            EVAL_LOOP
            {
                __m256 cmp = _mm256_cmp_ps(av[i], _mm256_setzero_ps(), CMP_LT_OQ);

                // If a value is less than zero, negate its derivative
                odx[i] = _mm256_blendv_ps(
                        adx[i], _mm256_sub_ps(_mm256_setzero_ps(), adx[i]), cmp);
                ody[i] = _mm256_blendv_ps(
                        ady[i], _mm256_sub_ps(_mm256_setzero_ps(), ady[i]), cmp);
                odz[i] = _mm256_blendv_ps(
                        adz[i], _mm256_sub_ps(_mm256_setzero_ps(), adz[i]), cmp);
            }
            break;
        case OP_A:
            EVAL_LOOP
            {
                odx[i] = adx[i];
                ody[i] = ady[i];
                odz[i] = adz[i];
            }
            break;
        case OP_B:
            EVAL_LOOP
            {
                odx[i] = bdx[i];
                ody[i] = bdy[i];
                odz[i] = bdz[i];
            }
            break;

        // Trig functions don't have AVX equivalents, so fall back to
        // default clause evaluation
        case OP_ATAN2:
        case OP_SIN:
        case OP_COS:
        case OP_TAN:
        case OP_ASIN:
        case OP_ACOS:
        case OP_ATAN:
        case OP_EXP:
        case OP_MOD:
        case OP_NANFILL:
            clause(op, reinterpret_cast<const float*>(av),
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

        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
}
#endif

static Interval clause(Opcode op, const Interval& a, const Interval& b)
{
    switch (op) {
        case OP_ADD:
            return a + b;
        case OP_MUL:
            return a * b;
        case OP_MIN:
            return boost::numeric::min(a, b);
        case OP_MAX:
            return boost::numeric::max(a, b);
        case OP_SUB:
            return a - b;
        case OP_DIV:
            return a / b;
        case OP_ATAN2:
            return atan2(a, b);
        case OP_MOD:
            return Interval(0.0f, b.upper()); // YOLO
        case OP_NANFILL:
            return (isnan(a.lower()) || isnan(a.upper())) ? b : a;

        case OP_SQUARE:
            return boost::numeric::square(a);
        case OP_SQRT:
            return boost::numeric::sqrt(a);
        case OP_NEG:
            return -a;
        case OP_ABS:
            return boost::numeric::abs(a);
        case OP_SIN:
            return boost::numeric::sin(a);
        case OP_COS:
            return boost::numeric::cos(a);
        case OP_TAN:
            return boost::numeric::tan(a);
        case OP_ASIN:
            return boost::numeric::asin(a);
        case OP_ACOS:
            return boost::numeric::acos(a);
        case OP_ATAN:
            return boost::numeric::atan(a);
        case OP_EXP:
            return boost::numeric::exp(a);

        case OP_A:
            return a;
        case OP_B:
            return b;
        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
    return Interval();
}

////////////////////////////////////////////////////////////////////////////////

#ifdef __AVX__
const float* Evaluator::values(size_t count, bool vectorize)
{
    if (vectorize)
    {
        count = (count - 1)/8 + 1;

        for (const auto& row : rows)
        {
            for (size_t i=0; i < row.active; ++i)
            {
                auto op = row[i]->op;

                // Modify the opcode if parts of the tree are disabled
                if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_B;
                }
                if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_A;
                }

                clause(op, row[i]->ptrs.a.mf, row[i]->ptrs.b.mf,
                           row[i]->result.mf, count);
            }
        }

        return root->result.f;
    }
#else
const float* Evaluator::values(size_t count)
{
#endif
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            // Modify the opcode if parts of the tree are disabled
            if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            clause(op, row[i]->ptrs.a.f, row[i]->ptrs.b.f,
                       row[i]->result.f, count);
        }
    }
    return root->result.f;
}

#ifdef __AVX__
std::tuple<const float*, const float*,
           const float*, const float*> Evaluator::derivs(size_t count,
                                                         bool vectorize)
{
    if (vectorize)
    {
        count = (count - 1)/8 + 1;

        for (const auto& row : rows)
        {
            for (size_t i=0; i < row.active; ++i)
            {
                auto op = row[i]->op;

                // Modify the opcode if parts of the tree are disabled
                if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_B;
                }
                if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
                {
                    op = OP_A;
                }

                clause(op, row[i]->ptrs.a.mf, row[i]->ptrs.a.mdx,
                           row[i]->ptrs.a.mdy, row[i]->ptrs.a.mdz,

                           row[i]->ptrs.b.mf, row[i]->ptrs.b.mdx,
                           row[i]->ptrs.b.mdy, row[i]->ptrs.b.mdz,

                           row[i]->result.mf, row[i]->result.mdx,
                           row[i]->result.mdy, row[i]->result.mdz,
                       count);
            }
        }
        return {root->result.f, root->result.dx, root->result.dy, root->result.dz};
    }
#else
std::tuple<const float*, const float*,
           const float*, const float*> Evaluator::derivs(size_t count)
{
#endif
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            // Modify the opcode if parts of the tree are disabled
            if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            clause(op, row[i]->ptrs.a.f, row[i]->ptrs.a.dx,
                       row[i]->ptrs.a.dy, row[i]->ptrs.a.dz,

                       row[i]->ptrs.b.f, row[i]->ptrs.b.dx,
                       row[i]->ptrs.b.dy, row[i]->ptrs.b.dz,

                       row[i]->result.f, row[i]->result.dx,
                       row[i]->result.dy, row[i]->result.dz,
                   count);
        }
    }
    return {root->result.f, root->result.dx, root->result.dy, root->result.dz};
}

Interval Evaluator::interval()
{
    for (const auto& row : rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            auto op = row[i]->op;

            Interval a = row[i]->a ? row[i]->a->result.i : Interval();
            Interval b = row[i]->b ? row[i]->b->result.i : Interval();

            // Modify the opcode if parts of the tree are disabled
            if (row[i]->a && row[i]->a->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_B;
            }
            if (row[i]->b && row[i]->b->flags & CLAUSE_FLAG_DISABLED)
            {
                op = OP_A;
            }

            row[i]->result.i = clause(op, a, b);
        }
    }
    return root->result.i;
}

////////////////////////////////////////////////////////////////////////////////

double Evaluator::utilization() const
{
    double total = 0;
    double active = 0;

    for (const auto& r : rows)
    {
        total += r.size();
        active += r.active;
    }

    return active / total;
}
