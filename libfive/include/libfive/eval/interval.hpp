/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

/*  The Boost interval library hits MSVC warning 4244 (casting to a narrower 
 *  type) when it creates an interval of a narrower type from objects of a 
 *  wider type.  We don't want to just cast beforehand, as Boost performs the
 *  narrowing in an interval-safe manner, so we need to disable the warning.
 */
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244)
#endif
#include <boost/numeric/interval.hpp>
#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace libfive {

/*  The Interval class wraps a boost::numeric::interval<float>, plus a single
 *  boolean flag to indicate whether the value may also be NaN.  It performs
 *  internal arithmetic, plus checking of the function domains to update the
 *  maybe_nan flag.  */
class Interval {
public:
    Interval(float lower, float upper)
        : i(lower, upper), maybe_nan(std::isnan(lower) || std::isnan(upper))
    { /* Nothing to do here */ }

    Interval(float f) : Interval(f, f)
    { /* Nothing to do here */ }

    Interval(float lower, float upper, bool maybe_nan)
        : i(lower, upper), maybe_nan(maybe_nan)
    { /* Nothing to do here */ }

    Interval(double lower, double upper)
        : i(lower, upper), maybe_nan(std::isnan(lower) || std::isnan(upper))
    { /* Nothing to do here */ }

    Interval(double d) : Interval(d, d)
    { /* Nothing to do here */ }

    Interval()
        : i(), maybe_nan(false)
    {
        // Nothing to do here
    }

    enum State { EMPTY, FILLED, AMBIGUOUS, UNKNOWN };

    bool isSafe() const { return !maybe_nan; }

    float lower() const { return i.lower(); }
    float upper() const { return i.upper(); }

    inline bool isFilled() const { return i.upper() < 0; }
    inline bool isEmpty() const  { return i.lower() > 0; }
    inline State state() const
    {
        if (maybe_nan) {
            return AMBIGUOUS;
        } else if (isEmpty()) {
            return EMPTY;
        } else if (isFilled()) {
            return FILLED;
        } else {
            return AMBIGUOUS;
        }
    }

    friend Interval operator+(const Interval& a, const Interval& b);
    friend Interval operator*(const Interval& a, const Interval& b);

    static Interval min(const Interval& a, const Interval& b)
    {
        auto i = boost::numeric::min(a.i, b.i);
        // Eigen uses std::min (via a using declaration), which returns NaN if 
        // and only if the first input is NaN.  Thus, if the second input can 
        // be NaN, we need to include the range of the first, but not vice 
        // versa; we likewise can copy the maybe_nan from the first input.  If
        // it is later changed to use the approach of fmin and fmax (returning
        // the non-NaN value if one of the inputs is NaN), we can switch to
        // the other form (including each range if the other can be NaN, but
        // returning a maybe_nan that is the conjunction of the inputs').
#define LIBFIVE_USES_STD_MIN_AND_MAX true
#if LIBFIVE_USES_STD_MIN_AND_MAX
        if (b.maybe_nan) {
            i = hull(i, a.i);
        }
        return Interval(i, a.maybe_nan);
#else
        if (a.maybe_nan) {
            i = hull(i, b.i);
        }
        if (b.maybe_nan) {
            i = hull(i, a.i);
        }
        return Interval(i, a.maybe_nan && b.maybe_nan);
#endif
    }

    static Interval max(const Interval& a, const Interval& b)
    {
        auto i = boost::numeric::max(a.i, b.i);

        // std::max returns NaN iff the first input NaN the same way as 
        // std::min.
#if LIBFIVE_USES_STD_MIN_AND_MAX
        if (b.maybe_nan) {
            i = hull(i, a.i);
        }
        return Interval(i, a.maybe_nan);
#else
        if (a.maybe_nan) {
            i = hull(i, b.i);
        }
        if (b.maybe_nan) {
            i = hull(i, a.i);
        }
        return Interval(i, a.maybe_nan && b.maybe_nan);
#endif
#undef LIBFIVE_USES_STD_MIN_AND_MAX
    }

    friend Interval operator-(const Interval& a, const Interval& b);
    friend Interval operator/(const Interval& a, const Interval& b);

    static Interval atan2(const Interval& y, const Interval& x)
    {
        const bool u = y.maybe_nan || x.maybe_nan ||
            (x.lower() <= 0.0f && x.upper() >= 0.0f &&
             y.lower() <= 0.0f && y.upper() >= 0.0f);

        // There are 9 possible cases for interval atan2:
        // - Completely within a quadrant (4 cases)
        // - Completely within two quadrants (4 cases)
        // - Containing the origin (1 case)

        if (x.lower() > 0)
        {   // Right half of the plane
            if (y.lower() > 0)
            {   // 1st quadrant
                return Interval(::atan2(y.lower(), x.upper()),
                                ::atan2(y.upper(), x.lower()), u);
            }
            else if (y.upper() < 0)
            {   // 4th quadrant
                return Interval(::atan2(y.lower(), x.lower()),
                                ::atan2(y.upper(), x.upper()), u);
            }
            else
            {   // Crossing the X axis
                return Interval(::atan2(y.lower(), x.lower()),
                                ::atan2(y.upper(), x.lower()), u);
            }
        }
        else if (x.upper() < 0)
        {   // Left half of the plane
            if (y.lower() > 0)
            {   // 2nd quadrant
                return Interval(::atan2(y.upper(), x.upper()),
                                ::atan2(y.lower(), x.lower()), u);
            }
            else if (y.upper() < 0)
            {   // 3rd quadrant
                return Interval(::atan2(y.upper(), x.lower()),
                                ::atan2(y.lower(), x.upper()), u);
            }
            else
            {   // Branch cut
                return Interval(-float(M_PI), float(M_PI), u);
            }
        }
        else
        {  // Both sides of the plane
            if (y.lower() > 0)
            {   // Top half of the plane
                return Interval(::atan2(y.lower(), x.upper()),
                                ::atan2(y.lower(), x.lower()), u);
            }
            else if (y.upper() < 0)
            {
                // Bottom half of the plane
                return Interval(::atan2(y.upper(), x.lower()),
                                ::atan2(y.upper(), x.upper()), u);
            }
            else
            {
                // Contains the origin
                return Interval(-float(M_PI), float(M_PI), u);
            }
        }
    }

    static Interval pow(const Interval& a, const Interval& b)
    {
        auto bPt = int(b.lower());

        auto out = boost::numeric::pow(a.i, bPt);
        // The behavior of raising zero to a negative power is
        // implementation-defined; it may raise a domain error (and thus
        // return NaN) or a pole error (and thus return an infinite value);
        // We can use a static const variable to test for this.

        // Other cases that produce NaN are 0^0, or a negative number to a
        // noninteger; we can use std::pow explicitly to test whether b is
        // considered an integer as far as std::pow is concerned.
        static const auto nanOnZeroToNegative =
            std::isnan(std::pow(0.0f, -1.0f));

        // Store whether a contains 0, to simplify the expression below.
        const bool a_zero = a.lower() <= 0.0f && a.upper() >= 0.0f;

        const bool u = a.maybe_nan || b.maybe_nan ||
           (a_zero && (bPt == 0.0f || (bPt < 0.0f && nanOnZeroToNegative))) ||
           (a.lower() < 0 && std::isnan(std::pow(-1.0f, bPt)));
        return Interval(out, u);
    }

    static Interval nth_root(const Interval& a, const Interval& b)
    {
        auto bPt = int(b.lower());
        auto i = boost::numeric::nth_root(a.i, bPt);

        // We can only take multiples-of-two nth roots on negative values
        const bool u = a.maybe_nan || b.maybe_nan ||
            (a.lower() <= 0.0f && !(bPt & 2));
        return Interval(i, u);
    }

    static Interval mod(const Interval& a, const Interval& b)
    {
        I out(fmin(b.lower(), 0.0f), fmax(0.0f, b.upper()));
        if (std::isfinite(a.upper()) && std::isfinite(a.lower()))
        {
            // We may be able to do better: Divide into cases, based on whether
            // b is above, below, or crossing 0.
            auto position = (b.upper() >= 0.0f) + 2 * (b.lower() <= 0.0f);
            auto usedA = a.i; // This will become a sgn(b)
            switch (position)
            {
            case 2: usedA *= -1; // fallthrough
            case 1:
            {
                // We will try to find better bounds for
                //      usedA mod abs(b)
                // and then negate in case 2 (in which b is always negative)
                auto absB = boost::numeric::abs(b.i);
                auto quotients = usedA / absB;

                // We take the floor after dividing, so that it will round
                // toward -INFINITY instead of 0; this matches the array
                // evaluator for positive b.
                auto quotientInt = static_cast<int>
                    (std::floor(quotients.lower()));
                if (quotientInt == static_cast<int>
                        (std::floor(quotients.upper())))
                {
                  out = a.i - b.i * float(quotientInt);
                }
            }
            case 3:
                break;
            case 0: //Can only happen if b is guaranteed NaN.
                out = I::empty();
                break;
            default:
                assert(false);
            }
        }
        const bool u = b.upper() >= 0.0f && b.lower() <= 0.0f;
        return Interval(out, u);
    }

    static Interval nanfill(const Interval& a, const Interval& b)
    {
        if (a.maybe_nan) {
            return Interval(hull(a.i, b.i), b.maybe_nan);
        } else {
            return Interval(a.i, false);
        }
    }

    static Interval compare(const Interval& a, const Interval& b)
    {
        if (a.upper() < b.lower()) {
            return Interval(-1, -1, false);
        } else if (a.lower() > b.upper()) {
            return Interval(1, 1, false);
        } else {
            return Interval(-1, 1, false);
        }
    }

    static Interval square(const Interval& a)
    {
        return Interval(boost::numeric::square(a.i), a.maybe_nan);
    }

    static Interval sqrt(const Interval& a)
    {
        return Interval(boost::numeric::sqrt(a.i),
                        a.maybe_nan || a.lower() < 0.0f);
    }

    Interval operator-() const
    {
        return Interval(-i, maybe_nan);
    }

    static Interval sin(const Interval& a)
    {
        return Interval(boost::numeric::sin(a.i), a.maybe_nan);
    }

    static Interval cos(const Interval& a)
    {
        return Interval(boost::numeric::cos(a.i), a.maybe_nan);
    }

    static Interval tan(const Interval& a)
    {
        return Interval(boost::numeric::tan(a.i), a.maybe_nan);
    }

    static Interval asin(const Interval& a)
    {
        return Interval(boost::numeric::asin(a.i),
                        a.maybe_nan || a.lower() < -1.0f || a.upper() > 1.0f);
    }

    static Interval acos(const Interval& a)
    {
        return Interval(boost::numeric::acos(a.i),
                        a.maybe_nan || a.lower() < -1.0f || a.upper() > 1.0f);
    }

    static Interval atan(const Interval& a)
    {
        // If the interval has an infinite bound, then return the largest
        // possible output interval (of +/- pi/2).  This rescues us from
        // situations where we do atan(y / x) and the behavior of the
        // interval changes if you're approaching x = 0 from below versus
        // from above.
        auto i = (std::isinf(a.lower()) || std::isinf(a.upper()))
            ? I(-M_PI/2, M_PI/2)
            : boost::numeric::atan(a.i);
        return Interval(i, a.maybe_nan);
    }

    static Interval exp(const Interval& a)
    {
        return Interval(boost::numeric::exp(a.i), a.maybe_nan);
    }

    static Interval log(const Interval& a)
    {
        // Exactly 0 is ok (returns -inf)
        const bool u = a.maybe_nan || a.lower() < 0.0f;
        return Interval(boost::numeric::log(a.i), u);
    }

    static Interval abs(const Interval& a)
    {
        return Interval(boost::numeric::abs(a.i), a.maybe_nan);
    }

    static Interval recip(const Interval& a)
    {
        // The numerator can't be 0, so a denominator of 0 won't produce NaN
        return Interval(1.0f / a.i, a.maybe_nan);
    }

protected:
    using I =  boost::numeric::interval<float,
        boost::numeric::interval_lib::policies<
            boost::numeric::interval_lib::save_state<
                boost::numeric::interval_lib::rounded_transc_std<float>>,
            boost::numeric::interval_lib::checking_base<float>>>;
    I i;

    Interval(const I& i, bool maybe_nan)
        : i(i), maybe_nan(maybe_nan)
    {
        // Nothing to do here
    }

    bool maybe_nan;
};

inline Interval operator+(const Interval& a, const Interval& b)
{
    const bool u = a.maybe_nan || b.maybe_nan ||
        (a.lower() == -INFINITY && b.upper() == INFINITY) ||
        (b.lower() == -INFINITY && a.upper() == INFINITY);
    return Interval(a.i + b.i, u);
}

inline Interval operator*(const Interval& a, const Interval& b)
{
    const bool u = a.maybe_nan || b.maybe_nan ||
        ((a.lower() == -INFINITY || a.upper() == INFINITY)
          && b.lower() <= 0.0f && b.upper() >= 0.0f) ||
        ((b.lower() == -INFINITY || b.upper() == INFINITY)
         && a.lower() <= 0.0f && a.upper() >= 0.0f);
    return Interval(a.i * b.i, u);
}

inline Interval operator-(const Interval& a, const Interval& b)
{
    const bool u = a.maybe_nan || b.maybe_nan ||
        (a.lower() == -INFINITY && b.lower() == -INFINITY) ||
        (a.upper() == -INFINITY && b.upper() == -INFINITY);
    return Interval(a.i - b.i, u);
}

inline Interval operator/(const Interval& a, const Interval& b)
{
    // We're conservative here: if b crosses 0, then we assume
    // it can produce both positive and negative infinity.  This
    // works around behaviors in boost where
    //      0.1 / [-0.3215, 0] = [-inf, -0.32]
    // (which obviously doesn't contain 0.1 / 0 = +inf
    auto i = (b.lower() <= 0.0f && b.upper() >= 0.0f)
        ? Interval::I(-INFINITY, INFINITY)
        : (a.i / b.i);

    const bool u = a.maybe_nan || b.maybe_nan ||
        ((a.lower() == -INFINITY || a.upper() == INFINITY) &&
         (b.lower() == -INFINITY || b.upper() == INFINITY)) ||
        (a.lower() <= 0.0f && a.upper() >= 0.0f &&
         b.lower() <= 0.0f && b.upper() >= 0.0f);
    return Interval(i, u);
}

}
