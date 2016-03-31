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
#include <iostream>

#include "ao/kernel/cuda/tape.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"
#include "ao/kernel/render/region.hpp"

#define ARG_A_CONST 0x100
#define ARG_A_MEM   0x200

#define ARG_B_CONST 0x1000
#define ARG_B_MEM   0x2000

/*  This is the tape that the evaluator uses  */
__constant__ int tape_d[TapeAccelerator::NUM_CLAUSES * 2];
__constant__ float constants_d[TapeAccelerator::NUM_CONSTANTS];

__constant__ uint32_t clause_count_d;
__constant__ uint32_t tree_root_d;

/*
 *  Evaluate a single point (see eval below for details)
 */
__device__ float eval_single(float x, float y, float z)
{
    // This is our local slice of memory used to store clause results
    float local[TapeAccelerator::NUM_CLAUSES];

    local[0] = x;
    local[1] = y;
    local[2] = z;

    // Make sure that the tape is ready for use
    __syncthreads();

    // First three clauses are dummies for X, Y, Z coordinates
    for (int clause_index=3; clause_index < clause_count_d; clause_index++)
    {
        // Grab the next opcode from the tape
        uint32_t opcode = tape_d[2 * clause_index];
        uint32_t   addr = tape_d[2 * clause_index + 1];

        uint16_t a_addr = addr & 0xFFFF;
        uint16_t b_addr = (addr >> 16) & 0xFFFF;

        // These are the values that we'll do math on
        float a, b;

        // Read arguments if present, selecting either from an immediate
        // argument (i.e. an inline float) or an address in the local mem
        if (opcode & ARG_A_CONST)
        {
            a = constants_d[a_addr];
        }
        else if (opcode & ARG_A_MEM)
        {
            a = local[a_addr];
        }

        if (opcode & ARG_B_CONST)
        {
            b = constants_d[b_addr];
        }
        else if (opcode & ARG_B_MEM)
        {
            b = local[b_addr];
        }

        switch (opcode & 0xFF)
        {
            case OP_ADD:    local[clause_index] = a + b; break;
            case OP_MUL:    local[clause_index] = a * b; break;

            case OP_MIN:    local[clause_index] = fmin(a, b); break;
            case OP_MAX:    local[clause_index] = fmax(a, b); break;
            case OP_SUB:    local[clause_index] = a - b; break;
            case OP_DIV:    local[clause_index] = a / b; break;
            case OP_ATAN2:  local[clause_index] = atan2(a, b); break;
            case OP_MOD:    local[clause_index] = fmod(a, b);
                            while (local[clause_index] < 0)
                            {
                                local[clause_index] += b;
                            }
                            break;
            case OP_NANFILL:    local[clause_index] = isnan(a) ? b : a; break;

            case OP_SQUARE: local[clause_index] = a * a; break;
            case OP_SQRT:   local[clause_index] = sqrt(a); break;
            case OP_NEG:    local[clause_index] = -a; break;
            case OP_ABS:    local[clause_index] = fabs(a); break;
            case OP_SIN:    local[clause_index] = sin(a); break;
            case OP_COS:    local[clause_index] = cos(a); break;
            case OP_TAN:    local[clause_index] = tan(a); break;
            case OP_ASIN:   local[clause_index] = asin(a); break;
            case OP_ACOS:   local[clause_index] = acos(a); break;
            case OP_ATAN:   local[clause_index] = atan(a); break;
            case OP_EXP:    local[clause_index] = exp(a); break;
        }
    }

    return local[tree_root_d];
}

/*
 *  eval is a kernel that executes an instruction tape in parallel.
 *  We use tape_d as a global constant tape to read
 *      tape_d is a list of opcodes enums or'd with supporting data
 *          The lowest byte of each uint32 is the opcode itself
 *          The third and fourth nibbles are both
 *                  0x0 if this argument isn't relevant
 *                  0x1 if an immediate argument follows
 *                  0x2 if a memory address is given
 *              for the a and b arguments
 *  The arguments are as follows:
 *      X, Y, Z are pointers into device memory storing coordinates
 *      out is a pointer into device memory for the output
 *      clauses is the number of clauses to be evaluated
 *      root is the clause number to be copied to output
 */
__global__ void eval(float const* X, float const* Y, float const* Z,
                     float* out)
{
    // Index of this piece of work in the global space
    int index = threadIdx.x + blockIdx.x * blockDim.x;

    // Evaluate the expression on the target coordinates
    out[index] = eval_single(X[index], Y[index], Z[index]);
}

__global__ void eval_region(float xmin, float xmax, int imin, int ni,
                            float ymin, float ymax, int jmin, int nj,
                            float zmin, float zmax, int kmin, int nk,
                            uint32_t* image, uint32_t stride)
{
    // Index of this piece of work in the global space
    int index = threadIdx.x + blockIdx.x * blockDim.x;

    int i = index / (nj * nk);
    int j = (index / nk) % nj;
    int k = index % nk;

    // Abort if we're too beyond the bounds of reason
    if (i > ni)
    {
        return;
    }

    // Find our global coordinates
    float x_frac = (i + 0.5f) / ni;
    float x = xmin * (1.0f - x_frac) + xmax * x_frac;

    float y_frac = (j + 0.5f) / nj;
    float y = ymin * (1.0f - y_frac) + ymax * y_frac;

    float z_frac = (k + 0.5f) / nk;
    float z = zmin * (1.0f - z_frac) + zmax * z_frac;

    // Evaluate the expression on the target coordinates
    float out = eval_single(x, y, z);

    // If this reading is less than zero, update the heightmap
    if (out < 0)
    {
        atomicMax(&image[imin + i + (jmin + j) * stride], k);
    }
}

////////////////////////////////////////////////////////////////////////////////

TapeAccelerator::TapeAccelerator(Evaluator* e)
    : Accelerator(e)
{
    // Allocate space for the input and output arrays
    cudaMalloc((void**)&X_d, N * sizeof(float));
    cudaMalloc((void**)&Y_d, N * sizeof(float));
    cudaMalloc((void**)&Z_d, N * sizeof(float));
    cudaMalloc((void**)&out_d, N * sizeof(float));

    reloadTape();
}

void TapeAccelerator::reloadTape()
{
    // Construct the tape!
    std::vector<uint32_t> tape = {OP_X, 0, OP_Y, 0, OP_Z, 0};
    std::vector<float> constants;

    uint32_t clause_count = 3;

    // clause_addr stores addresses of normal clauses
    std::unordered_map<Clause*, uint32_t> clause_addr =
        {{evaluator->X, 0}, {evaluator->Y, 1}, {evaluator->Z, 2}};

    // const_addr maps from constant values to addresses
    std::unordered_map<float, uint32_t> const_addr;

    for (const auto& r : evaluator->rows)
    {
        for (size_t i=0; i < r.active; ++i)
        {
            Clause* c = r[i];

            uint32_t op = c->op;
            uint32_t addr = 0;

            clause_addr[c] = clause_count++;

            if (c->a)
            {
                if (c->a->op == OP_CONST)
                {
                    if (const_addr.count(c->a->value) == 0)
                    {
                        const_addr[c->a->value] = constants.size();
                        constants.push_back(c->a->value);
                    }
                    op |= ARG_A_CONST;
                    addr |= const_addr[c->a->value];
                }
                else
                {
                    op |= ARG_A_MEM;
                    addr |= clause_addr[c->a];
                }
            }

            if (c->b)
            {
                if (c->b->op == OP_CONST)
                {
                    if (const_addr.count(c->b->value) == 0)
                    {
                        const_addr[c->b->value] = constants.size();
                        constants.push_back(c->b->value);
                    }
                    op |= ARG_B_CONST;
                    addr |= (const_addr[c->b->value] << 16);
                }
                else
                {
                    op |= ARG_B_MEM;
                    addr |= (clause_addr[c->b] << 16);
                }
            }

            tape.push_back(op);
            tape.push_back(addr);
        }
    }

    assert(clause_addr.count(evaluator->root));

    // Make sure that we didn't run out space in our global arrays
    assert(clause_count <= NUM_CLAUSES);
    assert(constants.size() <= NUM_CONSTANTS);

    // Copy the tape over to the GPU
    cudaMemcpyToSymbol(tape_d, &tape[0], tape.size() * sizeof(uint32_t));

    // Copy the constant array to the GPU
    cudaMemcpyToSymbol(constants_d, &constants[0],
                       constants.size() * sizeof(float));

    // Deploy clause count and root address to GPU memory
    uint32_t root = clause_addr[evaluator->root];
    cudaMemcpyToSymbol(clause_count_d, &clause_count, sizeof(clause_count));
    cudaMemcpyToSymbol(tree_root_d, &root, sizeof(root));
}

TapeAccelerator::~TapeAccelerator()
{
    for (auto& ptr : {X_d, Y_d, Z_d, out_d})
    {
        cudaFree(ptr);
    }

    if (image_d)
    {
        cudaFree(image_d);
    }
}

void TapeAccelerator::allocateImage(const Region& r)
{
    image_dims = {r.X.values.size(), r.Y.values.size(), r.Z.values.size()};
    image_min = {r.X.bounds.lower(), r.Y.bounds.lower(), r.Z.bounds.lower()};
    image_max = {r.X.bounds.upper(), r.Y.bounds.upper(), r.Z.bounds.upper()};

    size_t bytes = image_dims.x * image_dims.y * sizeof(uint32_t);
    cudaMalloc(&image_d, bytes);
    cudaMemset(image_d, 0,  bytes);
}

void TapeAccelerator::render(const Subregion& r)
{
    assert(r.voxels() <= N);

    int blocks = (r.voxels() + THREADS_PER_BLOCK - 1) / THREADS_PER_BLOCK;
    eval_region<<<blocks, THREADS_PER_BLOCK>>>(
        r.X.lower(), r.X.upper(), r.X.min, r.X.size,
        r.Y.lower(), r.Y.upper(), r.Y.min, r.Y.size,
        r.Z.lower(), r.Z.upper(), r.Z.min, r.Z.size,
        image_d, image_dims.x);
}

void TapeAccelerator::getImage() const
{
    auto out = new uint32_t[image_dims.x * image_dims.y];

    cudaMemcpy(out, image_d,
               image_dims.x * image_dims.y * sizeof(uint32_t),
               cudaMemcpyDeviceToHost);

    int k=0;
    for (int i=0; i < image_dims.y; ++i)
    {
        for (int j=0; j < image_dims.x; ++j)
            std::cout << out[k++] << ' ';
        std::cout << '\n';
    }
}

float* TapeAccelerator::values(size_t count)
{
    int blocks = (count + THREADS_PER_BLOCK - 1) / THREADS_PER_BLOCK;
    eval<<<blocks, THREADS_PER_BLOCK>>>(X_d, Y_d, Z_d, out_d);

    return out_d;
}
