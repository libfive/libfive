/**
 *  @file oglplus/shapes/draw.ipp
 *  @brief Implementation of shape draw instructions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/glfunc.hpp>
#include <oglplus/error/basic.hpp>
#include <oglplus/lib/incl_end.ipp>
#include <oglplus/assert.hpp>

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
void DrawOperation::SetupPrimitiveRestart_(void) const
{
#if GL_VERSION_3_1
	if(restart_index == NoRestartIndex())
	{
		OGLPLUS_GLFUNC(Disable)(GL_PRIMITIVE_RESTART);
		OGLPLUS_VERIFY_SIMPLE(Disable);
	}
	else
	{
		OGLPLUS_GLFUNC(Enable)(GL_PRIMITIVE_RESTART);
		OGLPLUS_VERIFY_SIMPLE(Enable);
		OGLPLUS_GLFUNC(PrimitiveRestartIndex)(restart_index);
		OGLPLUS_VERIFY_SIMPLE(PrimitiveRestartIndex);
	}
#else
	if(restart_index != NoRestartIndex())
	{
		OGLPLUS_ABORT(
			"Primitive restarting required, "
			"but not supported by the used version of OpenGL!"
		);
	}
#endif
}

OGLPLUS_LIB_FUNC
void DrawOperation::CleanupPrimitiveRestart_(void) const
{
	if(restart_index != NoRestartIndex())
	{
#if GL_VERSION_3_1
		OGLPLUS_GLFUNC(Disable)(GL_PRIMITIVE_RESTART);
		OGLPLUS_VERIFY_SIMPLE(Disable);
#endif
	}
}

OGLPLUS_LIB_FUNC
void DrawOperation::Draw_(
	const void* indices,
	DataType index_data_type,
	GLuint inst_count,
	GLuint base_inst
) const
{
	switch(method)
	{
		case Method::DrawArrays:
		{
			return DrawArrays_(
				inst_count,
				base_inst
			);
		}

		case Method::DrawElements:
		{
			return DrawElements_(
				indices,
				index_data_type,
				inst_count,
				base_inst
			);
		}
	}
}


OGLPLUS_LIB_FUNC
void DrawOperation::DrawArrays_(GLuint inst_count, GLuint base_inst) const
{
	if(inst_count == 1)
	{
		OGLPLUS_GLFUNC(DrawArrays)(
			GLenum(mode),
			GLint(first),
			GLsizei(count)
		);
		OGLPLUS_CHECK_SIMPLE(DrawArrays);
	}
	else if(base_inst == 0)
	{
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
		OGLPLUS_GLFUNC(DrawArraysInstanced)(
			GLenum(mode),
			GLint(first),
			GLsizei(count),
			GLsizei(inst_count)
		);
		OGLPLUS_CHECK_SIMPLE(DrawArraysInstanced);
#else
		OGLPLUS_ABORT(
			"DrawArraysInstanced required, "
			"but not supported by the used version of OpenGL!"
		);
#endif
	}
	else
	{
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2
		OGLPLUS_GLFUNC(DrawArraysInstancedBaseInstance)(
			GLenum(mode),
			GLint(first),
			GLsizei(count),
			GLsizei(inst_count),
			base_inst
		);
		OGLPLUS_CHECK_SIMPLE(DrawArraysInstancedBaseInstance);
#else
		OGLPLUS_ABORT(
			"DrawArraysInstancedBaseInstance required, "
			"but not supported by the used version of OpenGL!"
		);
#endif
	}
}

OGLPLUS_LIB_FUNC
void DrawOperation::DrawElements_(
	const void* indices,
	DataType index_data_type,
	GLuint inst_count,
	GLuint base_inst
) const
{
	SetupPrimitiveRestart_();
	if(inst_count == 1)
	{
		OGLPLUS_GLFUNC(DrawElements)(
			GLenum(mode),
			GLsizei(count),
			GLenum(index_data_type),
			indices
		);
		OGLPLUS_CHECK_SIMPLE(DrawElements);
	}
	else if(base_inst == 0)
	{
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
		OGLPLUS_GLFUNC(DrawElementsInstanced)(
			GLenum(mode),
			GLsizei(count),
			GLenum(index_data_type),
			indices,
			GLsizei(inst_count)
		);
		OGLPLUS_CHECK_SIMPLE(DrawElementsInstanced);
#else
		OGLPLUS_ABORT(
			"DrawElementsInstanced required, "
			"but not supported by the used version of OpenGL!"
		);
#endif
	}
	else
	{
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseInstance)(
			GLenum(mode),
			GLsizei(count),
			GLenum(index_data_type),
			indices,
			GLsizei(inst_count),
			base_inst
		);
		OGLPLUS_CHECK_SIMPLE(DrawElementsInstancedBaseInstance);
#else
		OGLPLUS_ABORT(
			"DrawElementsInstancedBaseInstance required, "
			"but not supported by the used version of OpenGL!"
		);
#endif
	}
	CleanupPrimitiveRestart_();
}

} // shapes
} // oglplus

