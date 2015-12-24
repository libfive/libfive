/**
 *  @file oglplus/viewport_index.hpp
 *  @brief ViewportIndex object
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_VIEWPORT_INDEX_1405030825_HPP
#define OGLPLUS_VIEWPORT_INDEX_1405030825_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the viewport index (implementation-dependent limited) number
class ViewportIndex
 : public LimitedCount
{
public:
	ViewportIndex(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	ViewportIndex,
	MAX_VIEWPORTS
)
#endif

} // namespace oglplus

#endif // include guard
