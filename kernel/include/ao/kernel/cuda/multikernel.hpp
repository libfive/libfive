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

class Clause;

#include "ao/kernel/cuda/accelerator.hpp"

class MultikernelAccelerator : public Accelerator
{
public:
    MultikernelAccelerator(Evaluator* e);
    ~MultikernelAccelerator();

    /*
     *  Evaluates a certain number of values, returning a
     *  pointer to device memory.
     */
    float* values(size_t count) override;

protected:
    /*
     *  Returns the device pointer for the given clause
     *  (which must be associated with the parent Evaluator)
     */
    float* devPtr(const Clause* c);

    /*  Bag-o-data allocated on the target device  */
    float* data;
};
