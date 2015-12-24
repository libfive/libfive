/**
 *  @file oglplus/dsa/renderbuffer.ipp
 *  @brief Implementation of DSA Renderbuffer functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/images/image_spec.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

#if GL_VERSION_4_5 || GL_ARB_direct_state_access
OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::Renderbuffer>::
GetIntParam(GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetNamedRenderbufferParameteriv)(
		_obj_name(),
		query,
		&result
	);
	OGLPLUS_VERIFY(
		GetNamedRenderbufferParameteriv,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Renderbuffer>::
Storage(const images::ImageSpec& image_spec)
{
	Storage(
		image_spec.internal_format,
		image_spec.width,
		image_spec.height
	);
}

#endif // GL_ARB_direct_state_access

} // namespace oglplus

