#pragma once

#include <boost/numeric/interval.hpp>

typedef boost::numeric::interval<double> Interval;

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

inline Interval _cond_nz(const Interval& cond,
                         const Interval& a,
                         const Interval& b)
{
    if (cond.upper() < 0)
    {
        return a;
    }
    else if (cond.lower() >= 0)
    {
        return b;
    }
    else
    {
        return Interval(a.lower(), b.upper());
    }
}
