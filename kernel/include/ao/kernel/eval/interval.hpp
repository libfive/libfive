#pragma once

#include <boost/numeric/interval.hpp>

typedef boost::numeric::interval<float> Interval;

/*
 *  Define overloaded min and max for tree evaluation
 */
inline Interval _min(const Interval& a, const Interval& b)
{
    return boost::numeric::min(a, b);
}

inline Interval _max(const Interval& a, const Interval& b)
{
    return boost::numeric::max(a, b);
}

inline Interval _abs(const Interval& a)
{
    return boost::numeric::abs(a);
}
