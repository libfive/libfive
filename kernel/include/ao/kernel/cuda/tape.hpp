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
#pragma once

#include "ao/kernel/cuda/accelerator.hpp"

class TapeAccelerator : public Accelerator
{
public:
    TapeAccelerator(Evaluator* e);
    ~TapeAccelerator();

    /*
     *  Evaluates a certain number of values, returning a
     *  pointer to device memory.
     */
    float* values(size_t count) override;

    /*
     *  Reloads the tape from the evaluator's current state
     *  Updates root, clauses and the data stored at tape_d
     */
    void reloadTape();

    /*  This is the maximum number of clauses we can evaluate in a single
     *  pass (due to limited local per-thread memory)   */
    static constexpr size_t NUM_CLAUSES = 2048;
    static constexpr size_t NUM_CONSTANTS = 2048;

    /*  This is the number of threads executing in one CUDA block  */
    static constexpr size_t THREADS_PER_BLOCK = 1024;

protected:
    /*  Shorter data array used to store results  */
    float* out_d;
};
