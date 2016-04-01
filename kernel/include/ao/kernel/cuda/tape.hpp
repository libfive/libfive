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

#include <vector>

#include "glm/vec3.hpp"

#include "ao/kernel/cuda/accelerator.hpp"

class Region;
class Subregion;

class TapeAccelerator : public Accelerator
{
public:
    TapeAccelerator(Evaluator* e);
    ~TapeAccelerator();

    /*
     *  Allocates a width x height image for the given region
     *
     *  The device memory is stored in image_d and the image's stride
     *  (number of points per row) is in image_stride
     */
    void allocateImage(const Region& r);

    /*
     *  Fills the given subregion
     */
    void fill(const Subregion& r);

    /*
     *  Renders a particular subregion to image_d
     */
    void render(const Subregion& r);

    /*
     *  We can't compile Eigen with nvcc, so this returns a vector of
     *  floats instead of a matrix.
     */
    std::vector<float> getImage() const;

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

    /*  Target image (allocated memory on the device) */
    uint32_t* image_d=nullptr;

    /*  Image corners in viewport space  */
    glm::vec3 image_min, image_max;

    /*  Image dimensions in voxels  */
    glm::ivec3 image_dims;
};
