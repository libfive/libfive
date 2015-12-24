#include "ao/core/result.hpp"

#ifndef RESULT_INCLUDE_IPP
#error "Cannot include .ipp file on its own"
#endif

////////////////////////////////////////////////////////////////////////////////

template <>
inline double* Result::ptr<double>() const
{
    return const_cast<double*>(&d[0]);
}

template <>
inline Interval* Result::ptr<Interval>() const
{
    return const_cast<Interval*>(&i[0]);
}

template <>
inline Gradient* Result::ptr<Gradient>() const
{
    return const_cast<Gradient*>(&g[0]);
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

template <class T>
inline void Result::set(const std::vector<T>& vs)
{
    set(&vs[0], vs.size());
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
inline T Result::get(size_t index) const
{
    return ptr<T>()[index];
}

template <class T>
inline void Result::copyTo(T* target, size_t n) const
{
    assert(n <= count<T>());
    std::copy(ptr<T>(), ptr<T>() + n, target);
}
