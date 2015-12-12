#pragma once

#include <vector>

#include "interval.hpp"
#include "gradient.hpp"
#include "atom.hpp"

union Result {
    Result() { /* Provide default constructor */ }

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
    void copy_to(T* target, size_t count) const;

protected:
    /*
     *  Template to look up the base pointer for a particular type
     *  (specialized inline below)
     */
    template <class T>
    T* ptr() const;

    double d[ATOM_DOUBLE_COUNT];
    Interval i[ATOM_INTERVAL_COUNT];
    Gradient g[ATOM_GRADIENT_COUNT];
};

#include "result.ipp"
