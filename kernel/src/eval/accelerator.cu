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

#include "ao/kernel/eval/accelerator.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"
#include "ao/kernel/render/subregion.hpp"

// Helpful macros that are repeated in every kernel
#define GET_INDEX int i = threadIdx.x + blockIdx.x * blockDim.x;\
                  if (i >= Accelerator::N)  return
#define KERNEL(name) __global__ void name(float* __restrict__ a, float* __restrict__ b, float* __restrict__ out)

#define RENDER_PARAMS count/1024,1024
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

__global__ void flatten_region(float* x, float xmin, float xmax, int ni,
                               float* y, float ymin, float ymax, int nj,
                               float* z, float zmin, float zmax, int nk)
{
    int index = blockIdx.x;
    int i = index / (nj * nk);
    int j = (index / nk) % nj;
    int k = index % nk;

    if (i <= ni)
    {
        float frac = (i + 0.5f) / ni;
        x[index] = xmin * (1.0f - frac) + xmax * frac;
    }
    if (j <= nj)
    {
        float frac = (j + 0.5f) / nj;
        y[index] = ymin * (1.0f - frac) + ymax * frac;
    }
    if (k <= nk)
    {
        float frac = (k + 0.5f) / nk;
        z[index] = zmin * (1.0f - frac) + zmax * frac;
    }
}

////////////////////////////////////////////////////////////////////////////////

// Pointers are into device memory, not host memory!
static void clause(Opcode op, float* a, float* b, float* out, size_t count)
{
    switch (op) {
        case OP_ADD:    add_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_MUL:    mul_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_MIN:    min_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_MAX:    max_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_SUB:    sub_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_DIV:    div_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_ATAN2:  atan2_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_MOD:    mod_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_NANFILL:    nanfill_f<<<RENDER_PARAMS>>>(a, b, out); break;

        case OP_SQUARE: square_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_SQRT: sqrt_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_NEG: neg_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_ABS: abs_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_SIN: sin_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_COS: cos_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_TAN: tan_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_ASIN: asin_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_ACOS: acos_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_ATAN: atan_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_EXP: exp_f<<<RENDER_PARAMS>>>(a, b, out); break;

        case OP_A: a_f<<<RENDER_PARAMS>>>(a, b, out); break;
        case OP_B: b_f<<<RENDER_PARAMS>>>(a, b, out); break;

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
    // Count up the number of clauses in the evaluator
    size_t count =  std::accumulate(e->rows.begin(), e->rows.end(),
            3                           // X, Y, Z
            + e->constants.size(),      // Constants
            [](size_t i, const Row& r){ return i + r.size(); });

    auto out = cudaMalloc((void**)&data, N * count * sizeof(float));

    // If this is a constant operation, fill with the constant value
    for (auto c : e->constants)
    {
        std::fill(buf.begin(), buf.end(), c->value);
        cudaMemcpy(devPtr(c), &buf[0], N * sizeof(float),
                   cudaMemcpyHostToDevice);
    }
}

Accelerator::~Accelerator()
{
    cudaFree(data);
}

////////////////////////////////////////////////////////////////////////////////

float* Accelerator::devPtr(const Clause* c)
{
    // Clauses are allocated in the data array of the evaluator
    // (using placement new); this function finds the clause's
    // location in that array then maps to the device data array
    return &data[(c - evaluator->data) * N];
}

////////////////////////////////////////////////////////////////////////////////

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
            clause(op, devPtr(row[i]->a), devPtr(row[i]->b), devPtr(row[i]), count);
        }
    }

    return devPtr(evaluator->root);
}

void Accelerator::toDevice()
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(devPtr(evaluator->X), &X[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(devPtr(evaluator->Y), &Y[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(devPtr(evaluator->Z), &Z[0], bytes, cudaMemcpyHostToDevice);
}

float* Accelerator::fromDevice(float* ptr_d)
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(&buf[0], ptr_d, bytes, cudaMemcpyDeviceToHost);
    return &buf[0];
}

void Accelerator::warmup()
{
    float* a = new float[N];
    float* b = new float[N];
    float* out = new float[N];

    float* a_d = nullptr;
    float* b_d = nullptr;
    float* out_d = nullptr;

    auto bytes = N * sizeof(float);
    cudaMalloc((void**)&a_d, bytes);
    cudaMalloc((void**)&b_d, bytes);
    cudaMalloc((void**)&out_d, bytes);

    cudaMemcpy(&a_d, a, bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(&b_d, b, bytes, cudaMemcpyHostToDevice);

    add_f<<<N, 1>>>(a_d, b_d, out_d);
    cudaMemcpy(out, out_d, bytes, cudaMemcpyDeviceToHost);

    delete [] a;
    delete [] b;
    delete [] out;

    cudaFree(a_d);
    cudaFree(b_d);
    cudaFree(out_d);
}

void Accelerator::setRegion(const Subregion& r)
{
    flatten_region<<<r.voxels(), 1>>>(
        devPtr(evaluator->X), r.X.lower(), r.X.upper(), r.X.size,
        devPtr(evaluator->Y), r.Y.lower(), r.Y.upper(), r.Y.size,
        devPtr(evaluator->Z), r.Z.lower(), r.Z.upper(), r.Z.size);
}
