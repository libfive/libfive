#pragma once

#include <array>
#include <vector>

#include <immintrin.h>

#include "ao/eval/interval.hpp"
#include "ao/eval/gradient.hpp"

struct Result {
    /*
     *  Returns the array size for the given type
     */
    template <class T>
    static constexpr size_t count();

    /*
     *  Set the value at index to v
     */
    template <class T>
    void set(T v, size_t index);

    /*
     *  Sets all of the values to the given float
     *  (across the Interval, float, Gradient, and __m256 arrays)
     */
    void fill(float v);

    /*
     *  Set values 0 through count from the given array
     */
    template <class T>
    void set(const T* ts, size_t n=count<T>());

    /*
     *  Template for lookups by type (used in eval's inner loop)
     */
    template <class T>
    T get(size_t index) const;

    /*
     *  Template to look up the base pointer for a particular type
     *  (specialized inline below)
     */
    template <class T>
    T* ptr() const;

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
    Gradient g[256];

#ifdef __AVX__
    __m256 m[32];
#endif

    Interval i;
};

#define RESULT_INCLUDE_IPP
#include "result.ipp"
#undef RESULT_INCLUDE_IPP
