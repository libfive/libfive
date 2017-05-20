#pragma once

#include <boost/numeric/interval.hpp>

namespace Kernel {

typedef boost::numeric::interval<float,
    boost::numeric::interval_lib::policies<
        boost::numeric::interval_lib::save_state<
            boost::numeric::interval_lib::rounded_transc_std<float>>,
        boost::numeric::interval_lib::checking_base<float>>> Interval;

}

inline Kernel::Interval atan2(const Kernel::Interval& y, const Kernel::Interval& x)
{
    // There are 9 possible cases for interval atan2:
    // - Completely within a quadrant (4 cases)
    // - Completely within two quadrants (4 cases)
    // - Containing the origin (1 case)

    if (x.lower() > 0)
    {   // Right half of the plane
        if (y.lower() > 0)
        {   // 1st quadrant
            return Kernel::Interval(atan2(y.lower(), x.upper()),
                                    atan2(y.upper(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 4th quadrant
            return Kernel::Interval(atan2(y.lower(), x.lower()),
                                    atan2(y.upper(), x.upper()));
        }
        else
        {   // Crossing the X axis
            return Kernel::Interval(atan2(y.lower(), x.lower()),
                                    atan2(y.upper(), x.lower()));
        }
    }
    else if (x.upper() < 0)
    {   // Left half of the plane
        if (y.lower() > 0)
        {   // 2nd quadrant
            return Kernel::Interval(atan2(y.upper(), x.upper()),
                                    atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {   // 3rd quadrant
            return Kernel::Interval(atan2(y.upper(), x.lower()),
                                    atan2(y.lower(), x.upper()));
        }
        else
        {   // Branch cut
            return Kernel::Interval(-M_PI, M_PI);
        }
    }
    else
    {  // Both sides of the plane
        if (y.lower() > 0)
        {   // Top half of the plane
            return Kernel::Interval(atan2(y.lower(), x.upper()),
                                    atan2(y.lower(), x.lower()));
        }
        else if (y.upper() < 0)
        {
            // Bottom half of the plane
            return Kernel::Interval(atan2(y.upper(), x.lower()),
                                    atan2(y.upper(), x.upper()));
        }
        else
        {
            // Contains the origin
            return Kernel::Interval(-M_PI, M_PI);
        }
    }
}
