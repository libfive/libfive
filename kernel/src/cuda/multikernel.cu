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
#include <cstdio>
#include <numeric>

#include "ao/kernel/cuda/multikernel.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

// Helpful macros that are repeated in every kernel
#define GET_INDEX int i = threadIdx.x + blockIdx.x * blockDim.x;\
                  if (i >= MultikernelAccelerator::N)  return
#define KERNEL(name) __global__ void name(float* __restrict__ a, float* __restrict__ b, float* __restrict__ out)

////////////////////////////////////////////////////////////////////////////////
// Floating-point kernels
KERNEL(add_f)
{
    GET_INDEX;
    out[i] = a[i] + b[i];
}

KERNEL(mul_f)
{
    GET_INDEX;
    out[i] = a[i] * b[i];
}

KERNEL(min_f)
{
    GET_INDEX;
    out[i] = min(a[i], b[i]);
}

KERNEL(max_f)
{
    GET_INDEX;
    out[i] = max(a[i], b[i]);
}

KERNEL(sub_f)
{
    GET_INDEX;
    out[i] = a[i] - b[i];
}

KERNEL(div_f)
{
    GET_INDEX;
    out[i] = a[i] / b[i];
}

KERNEL(atan2_f)
{
    GET_INDEX;
    out[i] = atan2(a[i], b[i]);
}

KERNEL(mod_f)
{
    GET_INDEX;
    out[i] = fmod(a[i], b[i]);
    while (out[i] < 0)
    {
        out[i] += b[i];
    }
}

KERNEL(nanfill_f)
{
    GET_INDEX;
    out[i] = isnan(a[i]) ? b[i] : a[i];
}

KERNEL(square_f)
{
    GET_INDEX;
    out[i] = a[i] * a[i];
}

KERNEL(sqrt_f)
{
    GET_INDEX;
    out[i] = sqrt(a[i]);
}

KERNEL(neg_f)
{
    GET_INDEX;
    out[i] = -a[i];
}

KERNEL(abs_f)
{
    GET_INDEX;
    out[i] = fabs(a[i]);
}

KERNEL(sin_f)
{
    GET_INDEX;
    out[i] = sin(a[i]);
}

KERNEL(cos_f)
{
    GET_INDEX;
    out[i] = cos(a[i]);
}

KERNEL(tan_f)
{
    GET_INDEX;
    out[i] = tan(a[i]);
}

KERNEL(asin_f)
{
    GET_INDEX;
    out[i] = asin(a[i]);
}

KERNEL(acos_f)
{
    GET_INDEX;
    out[i] = acos(a[i]);
}

KERNEL(atan_f)
{
    GET_INDEX;
    out[i] = atan(a[i]);
}

KERNEL(exp_f)
{
    GET_INDEX;
    out[i] = exp(a[i]);
}

KERNEL(a_f)
{
    GET_INDEX;
    out[i] = a[i];
}

KERNEL(b_f)
{
    GET_INDEX;
    out[i] = b[i];
}

////////////////////////////////////////////////////////////////////////////////

// Pointers are into device memory, not host memory!
static void clause(Opcode op, float* a, float* b, float* out,
                   int blocks, int threads)
{
    switch (op) {
        case OP_ADD:    add_f<<<blocks, threads>>>(a, b, out); break;
        case OP_MUL:    mul_f<<<blocks, threads>>>(a, b, out); break;
        case OP_MIN:    min_f<<<blocks, threads>>>(a, b, out); break;
        case OP_MAX:    max_f<<<blocks, threads>>>(a, b, out); break;
        case OP_SUB:    sub_f<<<blocks, threads>>>(a, b, out); break;
        case OP_DIV:    div_f<<<blocks, threads>>>(a, b, out); break;
        case OP_ATAN2:  atan2_f<<<blocks, threads>>>(a, b, out); break;
        case OP_MOD:    mod_f<<<blocks, threads>>>(a, b, out); break;
        case OP_NANFILL:    nanfill_f<<<blocks, threads>>>(a, b, out); break;

        case OP_SQUARE: square_f<<<blocks, threads>>>(a, b, out); break;
        case OP_SQRT: sqrt_f<<<blocks, threads>>>(a, b, out); break;
        case OP_NEG: neg_f<<<blocks, threads>>>(a, b, out); break;
        case OP_ABS: abs_f<<<blocks, threads>>>(a, b, out); break;
        case OP_SIN: sin_f<<<blocks, threads>>>(a, b, out); break;
        case OP_COS: cos_f<<<blocks, threads>>>(a, b, out); break;
        case OP_TAN: tan_f<<<blocks, threads>>>(a, b, out); break;
        case OP_ASIN: asin_f<<<blocks, threads>>>(a, b, out); break;
        case OP_ACOS: acos_f<<<blocks, threads>>>(a, b, out); break;
        case OP_ATAN: atan_f<<<blocks, threads>>>(a, b, out); break;
        case OP_EXP: exp_f<<<blocks, threads>>>(a, b, out); break;

        case OP_A: a_f<<<blocks, threads>>>(a, b, out); break;
        case OP_B: b_f<<<blocks, threads>>>(a, b, out); break;

        case INVALID:
        case OP_CONST:
        case OP_X:
        case OP_Y:
        case OP_Z:
        case AFFINE:
        case LAST_OP: assert(false);
    }
}

////////////////////////////////////////////////////////////////////////////////

MultikernelAccelerator::MultikernelAccelerator(Evaluator* e)
    : Accelerator(e)
{
    // Count up the number of clauses in the evaluator
    size_t count =  std::accumulate(e->rows.begin(), e->rows.end(),
            3                           // X, Y, Z
            + e->constants.size(),      // Constants
            [](size_t i, const Row& r){ return i + r.size(); });

    auto out = cudaMalloc((void**)&data, N * count * sizeof(float));

    // Populate X, Y, Z device pointers
    X_d = devPtr(evaluator->X);
    Y_d = devPtr(evaluator->Y);
    Z_d = devPtr(evaluator->Z);

    // If this is a constant operation, fill with the constant value
    for (auto c : e->constants)
    {
        std::fill(buf.begin(), buf.end(), c->value);
        cudaMemcpy(devPtr(c), &buf[0], N * sizeof(float),
                   cudaMemcpyHostToDevice);
    }
}

MultikernelAccelerator::~MultikernelAccelerator()
{
    cudaFree(data);
}

////////////////////////////////////////////////////////////////////////////////

float* MultikernelAccelerator::devPtr(const Clause* c)
{
    // Clauses are allocated in the data array of the evaluator
    // (using placement new); this function finds the clause's
    // location in that array then maps to the device data array
    return &data[(c - evaluator->data) * N];
}

////////////////////////////////////////////////////////////////////////////////

float* MultikernelAccelerator::values(size_t count)
{
    int threads = 256;
    int blocks = (count + threads - 1) / threads;

    for (const auto& row : evaluator->rows)
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
            clause(op, devPtr(row[i]->a), devPtr(row[i]->b), devPtr(row[i]),
                   blocks, threads);
        }
    }

    return devPtr(evaluator->root);
}
