#include "ao/core/result.hpp"

template <>
inline double* Result::ptr<double>() const
{
    return const_cast<double*>(d);
}

template <>
inline Interval* Result::ptr<Interval>() const
{
    return const_cast<Interval*>(i);
}

////////////////////////////////////////////////////////////////////////////////

template <class T>
inline void Result::set(T v, size_t index)
{
    ptr<T>()[index] = v;
}

template <class T>
inline void Result::set(const T* ts, size_t count)
{
    assert(count <= (ATOM_ARRAY_BYTES / sizeof(T)));
    std::copy(ts, ts + count, ptr<T>());
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
inline void Result::copyTo(T* target, size_t count) const
{
    std::copy(ptr<T>(), ptr<T>() + count, target);
}

