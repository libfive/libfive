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

#include "ao/kernel/eval/accelerator.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

// Helpful macros that are repeated in every kernel
#define GET_INDEX int i = blockIdx.x
#define KERNEL(name) __global__ void name(float* a, float* b, float* out)

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
static void clause(Opcode op, float* a, float* b, float* out, size_t count)
{
    switch (op) {
        case OP_ADD:    add_f<<<count,1>>>(a, b, out); break;
        case OP_MUL:    mul_f<<<count,1>>>(a, b, out); break;
        case OP_MIN:    min_f<<<count,1>>>(a, b, out); break;
        case OP_MAX:    max_f<<<count,1>>>(a, b, out); break;
        case OP_SUB:    sub_f<<<count,1>>>(a, b, out); break;
        case OP_DIV:    div_f<<<count,1>>>(a, b, out); break;
        case OP_ATAN2:  atan2_f<<<count,1>>>(a, b, out); break;
        case OP_MOD:    mod_f<<<count,1>>>(a, b, out); break;
        case OP_NANFILL:    nanfill_f<<<count,1>>>(a, b, out); break;

        case OP_SQUARE: square_f<<<count,1>>>(a, b, out); break;
        case OP_SQRT: sqrt_f<<<count,1>>>(a, b, out); break;
        case OP_NEG: neg_f<<<count,1>>>(a, b, out); break;
        case OP_ABS: abs_f<<<count,1>>>(a, b, out); break;
        case OP_SIN: sin_f<<<count,1>>>(a, b, out); break;
        case OP_COS: cos_f<<<count,1>>>(a, b, out); break;
        case OP_TAN: tan_f<<<count,1>>>(a, b, out); break;
        case OP_ASIN: asin_f<<<count,1>>>(a, b, out); break;
        case OP_ACOS: acos_f<<<count,1>>>(a, b, out); break;
        case OP_ATAN: atan_f<<<count,1>>>(a, b, out); break;
        case OP_EXP: exp_f<<<count,1>>>(a, b, out); break;

        case OP_A: a_f<<<count,1>>>(a, b, out); break;
        case OP_B: b_f<<<count,1>>>(a, b, out); break;

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

Accelerator::Accelerator(Evaluator* e)
    : evaluator(e)
{
    for (auto c : {e->X, e->Y, e->Z})
    {
        allocate(c);
    }
    for (auto c : e->matrix)
    {
        allocate(c);
    }
    for (auto c : e->constants)
    {
        allocate(c);
    }
    for (auto row : e->rows)
    {
        for (auto c : row)
        {
            allocate(c);
        }
    }
}

Accelerator::~Accelerator()
{
    for (auto m : mem)
    {
        cudaFree(m.second);
    }
}

////////////////////////////////////////////////////////////////////////////////

void Accelerator::allocate(Clause* c)
{
    const size_t bytes = N * sizeof(float);
    cudaMalloc((void**)&mem[c], bytes);

    // If this is a constant operation, fill with the constant value
    if (c->op == OP_CONST)
    {
        std::fill(buf.begin(), buf.end(), c->value);
        cudaMemcpy(mem[c], &buf[0], bytes, cudaMemcpyHostToDevice);
    }
}

float* Accelerator::values(size_t count)
{
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

            clause(op, mem[row[i]->a], mem[row[i]->b], mem[row[i]], count);
        }
    }

    return mem[evaluator->root];
}

void Accelerator::toDevice()
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(mem[evaluator->X], &X[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(mem[evaluator->Y], &Y[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(mem[evaluator->Z], &Z[0], bytes, cudaMemcpyHostToDevice);
}

float* Accelerator::fromDevice(float* ptr_d)
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(&buf[0], ptr_d, bytes, cudaMemcpyDeviceToHost);
    return &buf[0];
}
