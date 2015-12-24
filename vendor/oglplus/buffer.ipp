/**
 *  @file oglplus/buffer.ipp
 *  @brief Implementation of Buffer functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
GLenum ObjBindingOps<tag::Buffer>::
_binding_query(BufferTarget target)
{
	switch(GLenum(target))
	{
#include <oglplus/enums/buffer_target_bq.ipp>
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLenum ObjBindingOps<tag::Buffer>::
_binding_query(BufferIndexedTarget target)
{
	switch(GLenum(target))
	{
#include <oglplus/enums/buffer_indexed_target_bq.ipp>
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::Buffer>::
_binding(BufferTarget target)
{
	GLint name = 0;
	OGLPLUS_GLFUNC(GetIntegerv)(_binding_query(target), &name);
	OGLPLUS_VERIFY(
		GetIntegerv,
		Error,
		EnumParam(_binding_query(target))
	);

	assert(!(name < 0));
	return GLuint(name);
}

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::Buffer>::
_binding(BufferIndexedTarget target, GLuint index)
{
	GLint name = 0;
	OGLPLUS_GLFUNC(GetIntegeri_v)(_binding_query(target), index, &name);
	OGLPLUS_VERIFY(
		GetIntegeri_v,
		Error,
		EnumParam(_binding_query(target))
	);

	assert(!(name < 0));
	return GLuint(name);
}

OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::ExplicitSel, tag::Buffer>::
GetIntParam(Target target, GLenum query)
{
	GLint value = 0;
	OGLPLUS_GLFUNC(GetBufferParameteriv)(
		GLenum(target),
		query,
		&value
	);
	OGLPLUS_VERIFY(
		GetBufferParameteriv,
		ObjectError,
		ObjectBinding(target).
		EnumParam(query)
	);
	return value;
}

} // namespace oglplus

