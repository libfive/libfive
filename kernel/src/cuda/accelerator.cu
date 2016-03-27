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
#include "ao/kernel/cuda/accelerator.hpp"
#include "ao/kernel/render/subregion.hpp"

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

Accelerator::Accelerator(Evaluator* e)
    : evaluator(e)
{

}

float* Accelerator::fromDevice(float* ptr_d)
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(&buf[0], ptr_d, bytes, cudaMemcpyDeviceToHost);
    return &buf[0];
}

__global__ void _warmup(float* a, float * b)
{
    int i = threadIdx.x + blockIdx.x * blockDim.x;
    b[i] = a[i] * 2;
}

void Accelerator::warmup()
{
    float* a = new float[N];
    float* b = new float[N];

    float* a_d = nullptr;
    float* b_d = nullptr;

    auto bytes = N * sizeof(float);
    cudaMalloc((void**)&a_d, bytes);
    cudaMalloc((void**)&b_d, bytes);

    cudaMemcpy(&a_d, a, bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(&b_d, b, bytes, cudaMemcpyHostToDevice);

    _warmup<<<128, 32>>>(a, b);
    cudaMemcpy(b, b_d, bytes, cudaMemcpyDeviceToHost);

    delete [] a;
    delete [] b;

    cudaFree(a_d);
    cudaFree(b_d);
}

void Accelerator::toDevice()
{
    const size_t bytes = N * sizeof(float);
    cudaMemcpy(X_d, &X[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(Y_d, &Y[0], bytes, cudaMemcpyHostToDevice);
    cudaMemcpy(Z_d, &Z[0], bytes, cudaMemcpyHostToDevice);
}


void Accelerator::setRegion(const Subregion& r)
{
    flatten_region<<<r.voxels(), 1>>>(
        X_d, r.X.lower(), r.X.upper(), r.X.size,
        Y_d, r.Y.lower(), r.Y.upper(), r.Y.size,
        Z_d, r.Z.lower(), r.Z.upper(), r.Z.size);
}
