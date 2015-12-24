/**
 *  @file oglplus/draw_buffer_index.hpp
 *  @brief DrawBufferIndex object
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DRAW_BUFFER_INDEX_1405030825_HPP
#define OGLPLUS_DRAW_BUFFER_INDEX_1405030825_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the draw buffer index (implementation-dependent limited) number
class DrawBufferIndex
 : public LimitedCount
{
public:
	DrawBufferIndex(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	DrawBufferIndex,
	MAX_DRAW_BUFFERS
)
#endif

} // namespace oglplus

#endif // include guard
