#pragma once

#include <boost/numeric/interval.hpp>

typedef boost::numeric::interval<float,
    boost::numeric::interval_lib::policies<
        boost::numeric::interval_lib::save_state_nothing<
            boost::numeric::interval_lib::rounded_transc_exact<float>>,
        boost::numeric::interval_lib::checking_base<float>>> Interval;
