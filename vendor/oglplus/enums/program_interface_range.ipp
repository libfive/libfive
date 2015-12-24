//  File implement/oglplus/enums/program_interface_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/program_interface.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ProgramInterface
> ValueRange_(ProgramInterface*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PROGRAMINTERFACE)
#define OGLPLUS_IMPL_EVR_PROGRAMINTERFACE
{
static const GLenum _values[] = {
#if defined GL_UNIFORM
GL_UNIFORM,
#endif
#if defined GL_UNIFORM_BLOCK
GL_UNIFORM_BLOCK,
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
GL_ATOMIC_COUNTER_BUFFER,
#endif
#if defined GL_PROGRAM_INPUT
GL_PROGRAM_INPUT,
#endif
#if defined GL_PROGRAM_OUTPUT
GL_PROGRAM_OUTPUT,
#endif
#if defined GL_VERTEX_SUBROUTINE
GL_VERTEX_SUBROUTINE,
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE
GL_TESS_CONTROL_SUBROUTINE,
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE
GL_TESS_EVALUATION_SUBROUTINE,
#endif
#if defined GL_GEOMETRY_SUBROUTINE
GL_GEOMETRY_SUBROUTINE,
#endif
#if defined GL_FRAGMENT_SUBROUTINE
GL_FRAGMENT_SUBROUTINE,
#endif
#if defined GL_COMPUTE_SUBROUTINE
GL_COMPUTE_SUBROUTINE,
#endif
#if defined GL_VERTEX_SUBROUTINE_UNIFORM
GL_VERTEX_SUBROUTINE_UNIFORM,
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE_UNIFORM
GL_TESS_CONTROL_SUBROUTINE_UNIFORM,
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
GL_TESS_EVALUATION_SUBROUTINE_UNIFORM,
#endif
#if defined GL_GEOMETRY_SUBROUTINE_UNIFORM
GL_GEOMETRY_SUBROUTINE_UNIFORM,
#endif
#if defined GL_FRAGMENT_SUBROUTINE_UNIFORM
GL_FRAGMENT_SUBROUTINE_UNIFORM,
#endif
#if defined GL_COMPUTE_SUBROUTINE_UNIFORM
GL_COMPUTE_SUBROUTINE_UNIFORM,
#endif
#if defined GL_TRANSFORM_FEEDEBACK_VARYING
GL_TRANSFORM_FEEDEBACK_VARYING,
#endif
#if defined GL_BUFFER_VARIABLE
GL_BUFFER_VARIABLE,
#endif
#if defined GL_SHADER_STORAGE_BLOCK
GL_SHADER_STORAGE_BLOCK,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ProgramInterface
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

