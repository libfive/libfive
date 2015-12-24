#pragma once

#include <array>
#include <vector>

#include "ao/core/interval.hpp"
#include "ao/core/gradient.hpp"
#include "ao/core/atom.hpp"

// This is the storage size allocated to each array (in bytes)
//
// For efficiency, it must be sized to fit an non-fractional number of
// doubles, Intervals, and Gradient objects.  An error will be thrown
// at compile-time if this condition is not met.
#define RESULT_ARRAY_BYTES ((size_t)1024)
#define RESULT_COUNT(T) (RESULT_ARRAY_BYTES / (sizeof(T)))

union Result {
    explicit Result() { /* Provide default constructor */ }

    /*
     *  Returns the array size for the given type
     */
    template <class T>
    static constexpr size_t count() { return RESULT_COUNT(T); }

    /*
     *  Inline setters for base types (used in eval's inner loop)
     */
    template <class T>
    void set(T v, size_t index);

    /*
     *  Specialized setters for specific types of values
     */
    template <class T>
    void set(const T* ts, size_t n=count<T>());

    /*
     *  Setter for a vector of a given type
     *  (using the pointer-based setters above)
     */
    template <class T>
    void set(const std::vector<T>& vs);

    /*
     *  Template for lookups by type (used in eval's inner loop)
     */
    template <class T>
    T get(size_t index) const;

    /*
     *  Template to copy out of result (specialized inline below)
     */
    template <class T>
    void copyTo(T* target, size_t n) const;

    /*
     *  Template to look up the base pointer for a particular type
     *  (specialized inline below)
     */
    template <class T>
    T* ptr() const;

protected:
    template <class T>
    using SizedArray = std::array<T, RESULT_COUNT(T)>;

    SizedArray<double>   d;
    SizedArray<Interval> i;
    SizedArray<Gradient> g;
};

#define RESULT_INCLUDE_IPP
#include "result.ipp"
#undef RESULT_INCLUDE_IPP
