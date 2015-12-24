/**
 *  @file oglplus/math/constants.hpp
 *  @brief Math constants
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_CONSTANTS_1107121519_HPP
#define OGLPLUS_MATH_CONSTANTS_1107121519_HPP

#include <cmath>

namespace oglplus {
namespace math {

#ifdef M_PI
inline decltype(M_PI) Pi(void)
{
	return M_PI;
}

inline decltype(2*M_PI) TwoPi(void)
{
	return 2*M_PI;
}

inline decltype(0.5*M_PI) HalfPi(void)
{
	return 0.5*M_PI;
}
#else
inline decltype(std::atan(1.0) * 4.0) Pi(void)
{
	static auto _pi = std::atan(1.0) * 4.0;
	return _pi;
}

inline decltype(std::atan(1.0) * 8.0) TwoPi(void)
{
	static auto _pi = std::atan(1.0) * 8.0;
	return _pi;
}

inline decltype(std::atan(1.0) * 2.0) HalfPi(void)
{
	static auto _pi = std::atan(1.0) * 2.0;
	return _pi;
}
#endif

} // namespace math
} // namespace oglplus

#endif // include guard
