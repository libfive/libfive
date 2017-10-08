/*
Ao: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include <boost/numeric/interval.hpp>

namespace Kernel {
namespace Interval {

typedef boost::numeric::interval<float,
    boost::numeric::interval_lib::policies<
        boost::numeric::interval_lib::save_state<
            boost::numeric::interval_lib::rounded_transc_std<float>>,
        boost::numeric::interval_lib::checking_base<float>>> I;

enum State { EMPTY, FILLED, AMBIGUOUS, UNKNOWN };

inline bool isFilled(const Interval::I& i) { return i.upper() < 0; }
inline bool isEmpty(const Interval::I& i)  { return i.lower() > 0; }
inline State state(const Interval::I& i)
{
    return isEmpty(i)  ? EMPTY :
           isFilled(i) ? FILLED : AMBIGUOUS;
}

}   // namespace Interval
}   // namespace Kernel

inline Kernel::Interval::I atan2(const Kernel::Interval::I& y,
                                 const Kernel::Interval::I& x)
{
    // There are 9 possible cases for interval atan2:
    // - Completely within a quadrant (4 cases)
    // - Completely within two quadrants (4 cases)
    // - Containing the origin (1 case)

    if (x.lower() > 0)
    {   // Right half of the plane
        if (y.lower() > 0)
        {   // 1st quadrant
            return Kernel::Interval::I(atan2(y.lower(), x.upper()),
                                       atan2(y.upper(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 4th quadrant
            return Kernel::Interval::I(atan2(y.lower(), x.lower()),
                                       atan2(y.upper(), x.upper()));
        }
        else
        {   // Crossing the X axis
            return Kernel::Interval::I(atan2(y.lower(), x.lower()),
                                       atan2(y.upper(), x.lower()));
        }
    }
    else if (x.upper() < 0)
    {   // Left half of the plane
        if (y.lower() > 0)
        {   // 2nd quadrant
            return Kernel::Interval::I(atan2(y.upper(), x.upper()),
                                       atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 3rd quadrant
            return Kernel::Interval::I(atan2(y.upper(), x.lower()),
                                       atan2(y.lower(), x.upper()));
        }
        else
        {   // Branch cut
            return Kernel::Interval::I(-M_PI, M_PI);
        }
    }
    else
    {  // Both sides of the plane
        if (y.lower() > 0)
        {   // Top half of the plane
            return Kernel::Interval::I(atan2(y.lower(), x.upper()),
                                       atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {
            // Bottom half of the plane
            return Kernel::Interval::I(atan2(y.upper(), x.lower()),
                                       atan2(y.upper(), x.upper()));
        }
        else
        {
            // Contains the origin
            return Kernel::Interval::I(-M_PI, M_PI);
        }
    }
}
