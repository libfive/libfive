/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <boost/numeric/interval.hpp>

#include <glm/vec3.hpp>

typedef boost::numeric::interval<float,
    boost::numeric::interval_lib::policies<
        boost::numeric::interval_lib::save_state<
            boost::numeric::interval_lib::rounded_transc_std<float>>,
        boost::numeric::interval_lib::checking_base<float>>> Interval;

inline Interval atan2(const Interval& y, const Interval& x)
{
    // There are 9 possible cases for interval atan2:
    // - Completely within a quadrant (4 cases)
    // - Completely within two quadrants (4 cases)
    // - Containing the origin (1 case)

    if (x.lower() > 0)
    {   // Right half of the plane
        if (y.lower() > 0)
        {   // 1st quadrant
            return Interval(atan2(y.lower(), x.upper()),
                            atan2(y.upper(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 4th quadrant
            return Interval(atan2(y.lower(), x.lower()),
                            atan2(y.upper(), x.upper()));
        }
        else
        {   // Crossing the X axis
            return Interval(atan2(y.lower(), x.lower()),
                            atan2(y.upper(), x.lower()));
        }
    }
    else if (x.upper() < 0)
    {   // Left half of the plane
        if (y.lower() > 0)
        {   // 2nd quadrant
            return Interval(atan2(y.upper(), x.upper()),
                            atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 3rd quadrant
            return Interval(atan2(y.upper(), x.lower()),
                            atan2(y.lower(), x.upper()));
        }
        else
        {   // Branch cut
            return Interval(-M_PI, M_PI);
        }
    }
    else
    {  // Both sides of the plane
        if (y.lower() > 0)
        {   // Top half of the plane
            return Interval(atan2(y.lower(), x.upper()),
                            atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {
            // Bottom half of the plane
            return Interval(atan2(y.upper(), x.lower()),
                            atan2(y.upper(), x.upper()));
        }
        else
        {
            // Contains the origin
            return Interval(-M_PI, M_PI);
        }
    }
}
