/**
 *  @file oglplus/vertex_attrib_slot.hpp
 *  @brief VertexAttribSlot object
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_VERTEX_ATTRIB_SLOT_1405030825_HPP
#define OGLPLUS_VERTEX_ATTRIB_SLOT_1405030825_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the vertex attribute slot (implementation-dependent limited) number
/**
 *  @see VertexAttrib
 *
 *  @ingroup shader_variables
 */
class VertexAttribSlot
 : public LimitedCount
{
public:
	VertexAttribSlot(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	VertexAttribSlot,
	MAX_VERTEX_ATTRIBS
)
#endif

} // namespace oglplus

#endif // include guard
