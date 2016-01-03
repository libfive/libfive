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

inline void Result::fill(double v)
{
    for (size_t i=0; i < count<double>(); ++i)
    {
        set<double>(v, i);
    }
    for (size_t i=0; i < count<Gradient>(); ++i)
    {
        set<Gradient>(Gradient(v), i);
    }
    for (size_t i=0; i < count<Interval>(); ++i)
    {
        set<Interval>(Interval(v), i);
    }
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
inline T Result::get(size_t index) const
{
    return ptr<T>()[index];
}
