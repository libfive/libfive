#include <immintrin.h>

#include "ao/eval/result.hpp"

#ifndef RESULT_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

////////////////////////////////////////////////////////////////////////////////

template <>
constexpr size_t Result::count<double>()
{
    return sizeof(Result::d) / sizeof(Result::d[0]);
}

template <>
constexpr size_t Result::count<Gradient>()
{
    return sizeof(Result::g) / sizeof(Result::g[0]);
}

template <>
constexpr size_t Result::count<Interval>()
{
    return 1;
}

template <>
constexpr size_t Result::count<__m256>()
{
    return 32;
}

////////////////////////////////////////////////////////////////////////////////

template <>
inline double* Result::ptr<double>() const
{
    return const_cast<double*>(d);
}

template <>
inline Interval* Result::ptr<Interval>() const
{
    return const_cast<Interval*>(&i);
}

template <>
inline Gradient* Result::ptr<Gradient>() const
{
    return const_cast<Gradient*>(g);
}

template <>
inline __m256* Result::ptr<__m256>() const
{
    return const_cast<__m256*>(m);
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
inline void Result::set(T v, size_t index)
{
    ptr<T>()[index] = v;
}

template <class T>
inline void Result::set(const T* ts, size_t n)
{
    assert(n <= count<T>());
    std::copy(ts, ts + n, ptr<T>());
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
inline T Result::get(size_t index) const
{
    return ptr<T>()[index];
}
