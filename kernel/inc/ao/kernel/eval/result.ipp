#include <immintrin.h>

#include "ao/eval/result.hpp"

#ifndef RESULT_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

////////////////////////////////////////////////////////////////////////////////

template <>
constexpr size_t Result::count<float>()
{
    return sizeof(Result::f) / sizeof(Result::f[0]);
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

#ifdef __AVX__
template <>
constexpr size_t Result::count<__m256>()
{
    return 32;
}
#endif

////////////////////////////////////////////////////////////////////////////////

template <>
inline float* Result::ptr<float>() const
{
    return const_cast<float*>(f);
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

#ifdef __AVX__
template <>
inline __m256* Result::ptr<__m256>() const
{
    return const_cast<__m256*>(m);
}
#endif

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
