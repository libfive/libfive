/**
 *  @file oglplus/prog_var/typecheck.ipp
 *  @brief Implementation of program variable typechecking utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/object/tags.hpp>
#include <oglplus/object/desc.hpp>
#include <oglplus/lib/incl_end.ipp>
#include <cstring>

namespace oglplus {

#if !OGLPLUS_NO_UNIFORM_TYPECHECK

OGLPLUS_LIB_FUNC
bool GLSLtoCppTypeMatcher<GLint>::_matches(GLenum sl_type)
{
	const GLenum allowed[] = {
		GL_INT,

#ifdef GL_IMAGE_1D
		GL_IMAGE_1D,
#endif
#ifdef GL_IMAGE_1D_ARRAY
		GL_IMAGE_1D_ARRAY,
#endif
#ifdef GL_IMAGE_2D
		GL_IMAGE_2D,
#endif
#ifdef GL_IMAGE_2D_ARRAY
		GL_IMAGE_2D_ARRAY,
#endif
#ifdef GL_IMAGE_2D_MULTISAMPLE
		GL_IMAGE_2D_MULTISAMPLE,
#endif
#ifdef GL_IMAGE_2D_MULTISAMPLE_ARRAY
		GL_IMAGE_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_IMAGE_2D_RECT
		GL_IMAGE_2D_RECT,
#endif
#ifdef GL_IMAGE_3D
		GL_IMAGE_3D,
#endif
#ifdef GL_IMAGE_BUFFER
		GL_IMAGE_BUFFER,
#endif
#ifdef GL_IMAGE_CUBE
		GL_IMAGE_CUBE,
#endif

#ifdef GL_INT_IMAGE_1D
		GL_INT_IMAGE_1D,
#endif
#ifdef GL_INT_IMAGE_1D_ARRAY
		GL_INT_IMAGE_1D_ARRAY,
#endif
#ifdef GL_INT_IMAGE_2D
		GL_INT_IMAGE_2D,
#endif
#ifdef GL_INT_IMAGE_2D_ARRAY
		GL_INT_IMAGE_2D_ARRAY,
#endif
#ifdef GL_INT_IMAGE_2D_MULTISAMPLE
		GL_INT_IMAGE_2D_MULTISAMPLE,
#endif
#ifdef GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
		GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_INT_IMAGE_2D_RECT
		GL_INT_IMAGE_2D_RECT,
#endif
#ifdef GL_INT_IMAGE_3D
		GL_INT_IMAGE_3D,
#endif
#ifdef GL_INT_IMAGE_BUFFER
		GL_INT_IMAGE_BUFFER,
#endif
#ifdef GL_INT_IMAGE_CUBE
		GL_INT_IMAGE_CUBE,
#endif

#ifdef GL_UNSIGNED_INT_IMAGE_1D
		GL_UNSIGNED_INT_IMAGE_1D,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_1D_ARRAY
		GL_UNSIGNED_INT_IMAGE_1D_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_2D
		GL_UNSIGNED_INT_IMAGE_2D,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_2D_ARRAY
		GL_UNSIGNED_INT_IMAGE_2D_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
		GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
		GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_2D_RECT
		GL_UNSIGNED_INT_IMAGE_2D_RECT,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_3D
		GL_UNSIGNED_INT_IMAGE_3D,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_BUFFER
		GL_UNSIGNED_INT_IMAGE_BUFFER,
#endif
#ifdef GL_UNSIGNED_INT_IMAGE_CUBE
		GL_UNSIGNED_INT_IMAGE_CUBE,
#endif

#ifdef GL_INT_SAMPLER_1D
		GL_INT_SAMPLER_1D,
#endif
#ifdef GL_INT_SAMPLER_1D_ARRAY
		GL_INT_SAMPLER_1D_ARRAY,
#endif
#ifdef GL_INT_SAMPLER_2D
		GL_INT_SAMPLER_2D,
#endif
#ifdef GL_INT_SAMPLER_2D_ARRAY
		GL_INT_SAMPLER_2D_ARRAY,
#endif
#ifdef GL_INT_SAMPLER_2D_MULTISAMPLE
		GL_INT_SAMPLER_2D_MULTISAMPLE,
#endif
#ifdef GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
		GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_INT_SAMPLER_2D_RECT
		GL_INT_SAMPLER_2D_RECT,
#endif
#ifdef GL_INT_SAMPLER_3D
		GL_INT_SAMPLER_3D,
#endif
#ifdef GL_INT_SAMPLER_BUFFER
		GL_INT_SAMPLER_BUFFER,
#endif
#ifdef GL_INT_SAMPLER_CUBE
		GL_INT_SAMPLER_CUBE,
#endif

#ifdef GL_INT_SAMPLER_CUBE_MAP_ARRAY
		GL_INT_SAMPLER_CUBE_MAP_ARRAY,
#endif

#ifdef GL_UNSIGNED_INT_SAMPLER_1D
		GL_UNSIGNED_INT_SAMPLER_1D,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
		GL_UNSIGNED_INT_SAMPLER_1D_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_2D
		GL_UNSIGNED_INT_SAMPLER_2D,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
		GL_UNSIGNED_INT_SAMPLER_2D_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
		GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
		GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_2D_RECT
		GL_UNSIGNED_INT_SAMPLER_2D_RECT,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_3D
		GL_UNSIGNED_INT_SAMPLER_3D,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_BUFFER
		GL_UNSIGNED_INT_SAMPLER_BUFFER,
#endif
#ifdef GL_UNSIGNED_INT_SAMPLER_CUBE
		GL_UNSIGNED_INT_SAMPLER_CUBE,
#endif

#ifdef GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
		GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY,
#endif

#ifdef GL_SAMPLER_1D
		GL_SAMPLER_1D,
#endif
#ifdef GL_SAMPLER_1D_ARRAY
		GL_SAMPLER_1D_ARRAY,
#endif
#ifdef GL_SAMPLER_1D_ARRAY_SHADOW
		GL_SAMPLER_1D_ARRAY_SHADOW,
#endif
#ifdef GL_SAMPLER_1D_SHADOW
		GL_SAMPLER_1D_SHADOW,
#endif
#ifdef GL_SAMPLER_2D
		GL_SAMPLER_2D,
#endif
#ifdef GL_SAMPLER_2D_ARRAY
		GL_SAMPLER_2D_ARRAY,
#endif
#ifdef GL_SAMPLER_2D_ARRAY_SHADOW
		GL_SAMPLER_2D_ARRAY_SHADOW,
#endif
#ifdef GL_SAMPLER_2D_MULTISAMPLE
		GL_SAMPLER_2D_MULTISAMPLE,
#endif
#ifdef GL_SAMPLER_2D_MULTISAMPLE_ARRAY
		GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
#endif
#ifdef GL_SAMPLER_2D_RECT
		GL_SAMPLER_2D_RECT,
#endif
#ifdef GL_SAMPLER_2D_RECT_SHADOW
		GL_SAMPLER_2D_RECT_SHADOW,
#endif
#ifdef GL_SAMPLER_2D_SHADOW
		GL_SAMPLER_2D_SHADOW,
#endif
#ifdef GL_SAMPLER_3D
		GL_SAMPLER_3D,
#endif
#ifdef GL_SAMPLER_BUFFER
		GL_SAMPLER_BUFFER,
#endif
#ifdef GL_SAMPLER_CUBE
		GL_SAMPLER_CUBE,
#endif

#ifdef GL_SAMPLER_CUBE_MAP_ARRAY
		GL_SAMPLER_CUBE_MAP_ARRAY,
#endif

#ifdef GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
		GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW,
#endif

#ifdef GL_SAMPLER_CUBE_SHADOW
		GL_SAMPLER_CUBE_SHADOW,
#endif
		GL_BOOL
	};
	std::size_t i=0, n=sizeof(allowed)/sizeof(allowed[0]);

	while(i != n) if(sl_type == allowed[i++]) return true;

	return false;
}

OGLPLUS_LIB_FUNC
bool GLSLtoCppTypeMatcher<GLuint>::_matches(GLenum sl_type)
{
	const GLenum allowed[] = {
		GL_UNSIGNED_INT,
#ifdef GL_UNSIGNED_INT_ATOMIC_COUNTER
		GL_UNSIGNED_INT_ATOMIC_COUNTER,
#endif
		GL_BOOL
	};
	std::size_t i=0, n=sizeof(allowed)/sizeof(allowed[0]);

	while(i != n) if(sl_type == allowed[i++]) return true;

	return false;
}

OGLPLUS_LIB_FUNC
bool GLSLtoCppTypeMatcher_Vec::_does_match(
	GLenum sl_type,
	std::size_t type_idx,
	std::size_t dim
)
{
	const GLenum allowed[][4] = {
		{
			GL_BOOL,
			GL_BOOL_VEC2,
			GL_BOOL_VEC3,
			GL_BOOL_VEC4
		}, {
			GL_INT,
			GL_INT_VEC2,
			GL_INT_VEC3,
			GL_INT_VEC4
		}, {
			GL_UNSIGNED_INT,
			GL_UNSIGNED_INT_VEC2,
			GL_UNSIGNED_INT_VEC3,
			GL_UNSIGNED_INT_VEC4
		}, {
			GL_FLOAT,
			GL_FLOAT_VEC2,
			GL_FLOAT_VEC3,
			GL_FLOAT_VEC4
		}
#if defined(GL_DOUBLE) && defined(GL_DOUBLE_VEC2)
		 , {
			GL_DOUBLE,
			GL_DOUBLE_VEC2,
			GL_DOUBLE_VEC3,
			GL_DOUBLE_VEC4
		}
#endif
	};
	if(type_idx >= sizeof(allowed)/sizeof(allowed[0]))
		return false;
	return sl_type == allowed[type_idx][dim-1];
}

OGLPLUS_LIB_FUNC
bool GLSLtoCppTypeMatcher_Mat::_does_match(
	GLenum sl_type,
	std::size_t type_idx,
	std::size_t rows,
	std::size_t cols
)
{
	const GLenum allowed[][3][3] = {
		{
			{
				GL_FLOAT_MAT2,
				GL_FLOAT_MAT3x2,
				GL_FLOAT_MAT4x2
			}, {
				GL_FLOAT_MAT2x3,
				GL_FLOAT_MAT3,
				GL_FLOAT_MAT4x3
			}, {
				GL_FLOAT_MAT2x4,
				GL_FLOAT_MAT3x4,
				GL_FLOAT_MAT4
			}
		}
#if defined(GL_DOUBLE_MAT4)
		 , {
			{
				GL_DOUBLE_MAT2,
				GL_DOUBLE_MAT3x2,
				GL_DOUBLE_MAT4x2
			}, {
				GL_DOUBLE_MAT2x3,
				GL_DOUBLE_MAT3,
				GL_DOUBLE_MAT4x3
			}, {
				GL_DOUBLE_MAT2x4,
				GL_DOUBLE_MAT3x4,
				GL_DOUBLE_MAT4
			}
		}
#endif
	};
	if(type_idx >= sizeof(allowed)/sizeof(allowed[0]))
		return false;
	return sl_type==allowed[type_idx][rows-2][cols-2];
}

OGLPLUS_LIB_FUNC
void ProgVarTypecheck<tag::Typecheck, tag::Uniform>::
_do_check(
	bool (*type_matches)(GLenum),
	GLenum var_type,
	ProgramName program,
	GLint location,
	StrCRef identifier
)
{
	OGLPLUS_HANDLE_ERROR_IF(
		!type_matches(var_type),
		GL_INVALID_OPERATION,
		"Uniform type mismatch",
		ProgVarError,
		Program(program).
		Identifier(identifier).
		Index(location)
	);
}

#endif // !OGLPLUS_NO_UNIFORM_TYPECHECK

} // namespace oglplus

