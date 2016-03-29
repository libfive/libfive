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
 *      clauses is the number of clauses to be evaluated
 *      root is the clause number to be copied to output
 */
__global__ void eval(uint32_t const* tape_,
                     float const* X, float const* Y, float const* Z,
                     float* out, uint32_t tape_size, uint32_t root)
{
    int index = threadIdx.x + blockIdx.x * blockDim.x;

    // This is our local slice of memory used to store clause results
    float local[TapeAccelerator::NUM_CLAUSES];

    // Use the power of friendship to quickly copy the tape into local memory
    __shared__ uint32_t tape[TapeAccelerator::NUM_CLAUSES * 3];
    {
        // How many items from the tape must each thread copy over?
        // (rounded up, so we'll need to truncate later)
        unsigned num = (tape_size + TapeAccelerator::THREADS_PER_BLOCK - 1) /
                        TapeAccelerator::THREADS_PER_BLOCK;

        // This is the starting point in tape memory assigned to this thread
        unsigned start = threadIdx.x * num;
        unsigned end = start + num;
        if (end > tape_size)
        {
            end = tape_size;
        }
        for (unsigned i=start; i < end; ++i)
        {
            tape[i] = tape_[i];
        }
    }
    __syncthreads();

    // Load coordinates into the buffer
    local[0] = X[index];
    local[1] = Y[index];
    local[2] = Z[index];

    // First three opcodes are dummies for X, Y, Z coordinates
    for (int tape_index=3, clause_index=3; tape_index < tape_size;
         tape_index++, clause_index++)
    {
        // Grab the next opcode from the tape
        uint32_t opcode = tape[tape_index];

        // These are the values that we'll do math on
        float a, b;

        // Read arguments if present, selecting either from an immediate
        // argument (i.e. an inline float) or an address in the local mem
        if (opcode & ARG_A_IMM)
        {
            a = ((float*)tape)[++tape_index];
        }
        else if (opcode & ARG_A_MEM)
        {
            a = local[tape[++tape_index]];
        }

        if (opcode & ARG_B_IMM)
        {
            b = ((float*)tape)[++tape_index];
        }
        else if (opcode & ARG_B_MEM)
        {
            b = local[tape[++tape_index]];
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

    // In the worst case, every clause in the tape has two operands
    cudaMalloc((void**)&tape_d, NUM_CLAUSES * sizeof(uint32_t) * 3);

    reloadTape();
}

void TapeAccelerator::reloadTape()
{
    // Construct the tape!
    std::vector<uint32_t> tape;

    tape.push_back(OP_X);
    tape.push_back(OP_Y);
    tape.push_back(OP_Z);
    size_t clauses = 3;

    std::unordered_map<Clause*, uint32_t> addr =
        {{evaluator->X, 0}, {evaluator->Y, 1}, {evaluator->Z, 2}};

    for (const auto& r : evaluator->rows)
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

    assert(addr.count(evaluator->root));
    root = addr[evaluator->root];

    // Save the tape's size and copy it over to the GPU
    tape_size = tape.size();
    size_t tape_bytes = tape_size * sizeof(uint32_t);
    cudaMemcpy(tape_d, &tape[0], tape_bytes, cudaMemcpyHostToDevice);
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
    int blocks = (count + THREADS_PER_BLOCK - 1) / THREADS_PER_BLOCK;

    eval<<<blocks, THREADS_PER_BLOCK>>>(
            tape_d, X_d, Y_d, Z_d, out_d, tape_size, root);

    return out_d;
}
