#pragma once

#include <array>
#include <vector>

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
     *  Sets all of the values to the given double
     *  (across the Interval, double, and Gradient arrays)
     */
    void fill(double v);

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

protected:
    double d[256];
    Gradient g[256];

    Interval i;
};

#define RESULT_INCLUDE_IPP
#include "result.ipp"
#undef RESULT_INCLUDE_IPP
