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
#include "ao/kernel/cuda/tape.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/clause.hpp"

#define ARG_A_IMM 0x100
#define ARG_A_MEM 0x200

#define ARG_B_IMM 0x1000
#define ARG_B_MEM 0x2000
/*
 *  eval is a kernel that executes an instruction tape in parallel.
 *  The arguments are as follows:
 *      tape is a list of opcodes enums or'd with supporting data
 *          The lowest byte of each uint32 is the opcode itself
 *          The third and fourth nibbles are both
 *                  0x0 if this argument isn't relevant
 *                  0x1 if an immediate argument follows
 *                  0x2 if a memory address is given
 *              for the a and b arguments
 *      X, Y, Z are pointers into device memory storing coordinates
 *      out is a pointer into device memory for the output
 *      mem is a pointer into device memory for scratch results
 *          It should be clauses * blocks * threads * sizeof(float) in size
 *      clauses is the number of clauses to be evaluated
 */
__global__ void eval(uint32_t const* tape,
                     float const* X, float const* Y, float const* Z,
                     float* out, float* mem, int clauses, uint32_t root)
{
    int index = threadIdx.x + blockIdx.x * blockDim.x;

    // This is our local slice of the big memory buffer
    float* local = &mem[index * clauses];

    // Load coordinates into the buffer
    local[0] = X[index];
    local[1] = Y[index];
    local[2] = Z[index];

    // First three opcodes are dummies for X, Y, Z coordinates
    int i=3;
    int j=3;
    while(tape[i])
    {
        // Grab the next opcode from the tape
        uint32_t opcode = tape[i++];

        // These are the values that we'll do math on
        float a, b;

        // Read arguments if present, selecting either from an immediate
        // argument (i.e. an inline float) or an address in the local mem
        if (opcode & ARG_A_IMM)
        {
            a = ((float*)tape)[i++];
        }
        else if (opcode & ARG_A_MEM)
        {
            a = local[tape[i++]];
        }

        if (opcode & ARG_B_IMM)
        {
            b = ((float*)tape)[i++];
        }
        else if (opcode & ARG_B_MEM)
        {
            b = local[tape[i++]];
        }

        switch (opcode & 0xFF)
        {
            case OP_ADD:    local[j] = a + b; break;
            case OP_MUL:    local[j] = a * b; break;

            case OP_MIN:    local[j] = fmin(a, b); break;
            case OP_MAX:    local[j] = fmax(a, b); break;
            case OP_SUB:    local[j] = a - b; break;
            case OP_DIV:    local[j] = a / b; break;
            case OP_ATAN2:  local[j] = atan2(a, b); break;
            case OP_MOD:    local[j] = fmod(a, b);
                            while (local[j] < 0)
                            {
                                local[j] += b;
                            }
                            break;
            case OP_NANFILL:    local[j] = isnan(a) ? b : a; break;

            case OP_SQUARE: local[j] = a * a; break;
            case OP_SQRT:   local[j] = sqrt(a); break;
            case OP_NEG:    local[j] = -a; break;
            case OP_ABS:    local[j] = fabs(a); break;
            case OP_SIN:    local[j] = sin(a); break;
            case OP_COS:    local[j] = cos(a); break;
            case OP_TAN:    local[j] = tan(a); break;
            case OP_ASIN:   local[j] = asin(a); break;
            case OP_ACOS:   local[j] = acos(a); break;
            case OP_ATAN:   local[j] = atan(a); break;
            case OP_EXP:    local[j] = exp(a); break;
        }
        j++;
    }

    // Collect the resulting value and put it into the output array
    out[index] = local[root];
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


    // Construct the tape!
    std::vector<uint32_t> tape;

    tape.push_back(OP_X);
    tape.push_back(OP_Y);
    tape.push_back(OP_Z);
    clauses = 3;

    std::unordered_map<Clause*, uint32_t> addr =
        {{e->X, 0}, {e->Y, 1}, {e->Z, 2}};

    for (const auto& r : e->rows)
    {
        for (size_t i=0; i < r.active; ++i)
        {
            Clause* c = r[i];
            uint32_t op = c->op;
            addr[c] = clauses++;

            if (c->a)
            {
                if (c->a->op == OP_CONST)
                {
                    op |= ARG_A_IMM;
                }
                else
                {
                    op |= ARG_A_MEM;
                }
            }

            if (c->b)
            {
                if (c->b->op == OP_CONST)
                {
                    op |= ARG_B_IMM;
                }
                else
                {
                    op |= ARG_B_MEM;
                }
            }

            tape.push_back(op);
            if (op & ARG_A_IMM)
            {
                tape.push_back(*(uint32_t*)&c->a->value);
            }
            else if (op & ARG_A_MEM)
            {
                tape.push_back(addr[c->a]);
            }

            if (op & ARG_B_IMM)
            {
                tape.push_back(*(uint32_t*)&c->b->value);
            }
            else if (op & ARG_B_MEM)
            {
                tape.push_back(addr[c->b]);
            }
        }
    }
    tape.push_back(0);

    assert(addr.count(e->root));
    root = addr[e->root];

    // Allocate the tape and copy it over to the GPU
    size_t tape_bytes = tape.size() * sizeof(uint32_t);
    cudaMalloc((void**)&tape_d, tape_bytes);
    cudaMemcpy(tape_d, &tape[0], tape_bytes, cudaMemcpyHostToDevice);

    // Allocate the working memory buffer
    // (it's empty to begin with and populated by the kernel)
    cudaMalloc((void**)&mem_d, clauses * N * sizeof(float));
}

TapeAccelerator::~TapeAccelerator()
{
    for (auto& ptr : {X_d, Y_d, Z_d, out_d, out_d})
    {
        cudaFree(ptr);
    }
    cudaFree(tape_d);
}

float* TapeAccelerator::values(size_t count)
{
    int threads = 256;
    int blocks = (count + threads - 1) / threads;

    eval<<<blocks, threads>>>(tape_d, X_d, Y_d, Z_d, out_d, mem_d,
                              clauses, root);

    return out_d;
}
