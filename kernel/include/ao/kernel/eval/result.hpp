#pragma once

#include <array>
#include <vector>

#include <immintrin.h>

#include "ao/kernel/eval/interval.hpp"

struct Result {
    /*
     *  Sets a particular value in the array
     */
    void set(float v, float dx, float dy, float dz, size_t index);

    /*
     *  Sets the interval value in the array
     */
    void set(Interval V);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, float, Gradient, and __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     */
    void fill(float v);

#ifdef __AVX__
    /*
     *  Packs values from the float array into the AVX array
     */
    void packAVX();

    /*
     *  Unpacks values from the AVX array into the float array
     */
    void unpackAVX();
#endif

protected:
    float f[256];
    float dx[256];
    float dy[256];
    float dz[256];

    Interval i;

#ifdef __AVX__
    __m256 m[32];
#endif

    friend class Evaluator;
    friend class Clause;
};
