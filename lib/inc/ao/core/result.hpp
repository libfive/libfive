#pragma once

#include <vector>

#include "ao/core/interval.hpp"
#include "ao/core/gradient.hpp"
#include "ao/core/atom.hpp"

union Result {
    explicit Result() { /* Provide default constructor */ }

    /*
     *  Inline setters for base types (used in eval's inner loop)
     */
    template <class T>
    void set(T v, size_t index);

    /*
     *  Specialized setters for specific types of values
     */
    template <class T>
    void set(const T* ts, size_t count=(ATOM_ARRAY_BYTES / sizeof(T)));

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
    void copyTo(T* target, size_t count) const;

    /*
     *  Template to look up the base pointer for a particular type
     *  (specialized inline below)
     */
    template <class T>
    T* ptr() const;

protected:
    double d[ATOM_DOUBLE_COUNT];
    Interval i[ATOM_INTERVAL_COUNT];
    Gradient g[ATOM_GRADIENT_COUNT];
};

#define RESULT_INCLUDE_IPP
#include "result.ipp"
#undef RESULT_INCLUDE_IPP
