#pragma once

#include <boost/numeric/interval.hpp>

typedef boost::numeric::interval<float,
        boost::numeric::interval_lib::policies<
            boost::numeric::interval_lib::rounded_transc_opp<float>,
            boost::numeric::interval_lib::checking_catch_nan<float>>>
        Interval;
