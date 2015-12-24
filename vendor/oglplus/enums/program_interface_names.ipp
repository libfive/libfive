//  File implement/oglplus/enums/program_interface_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ProgramInterface*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PROGRAMINTERFACE)
#define OGLPLUS_IMPL_EVN_PROGRAMINTERFACE
{
switch(value)
{
#if defined GL_UNIFORM
	case GL_UNIFORM: return StrCRef("UNIFORM");
#endif
#if defined GL_UNIFORM_BLOCK
	case GL_UNIFORM_BLOCK: return StrCRef("UNIFORM_BLOCK");
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
	case GL_ATOMIC_COUNTER_BUFFER: return StrCRef("ATOMIC_COUNTER_BUFFER");
#endif
#if defined GL_PROGRAM_INPUT
	case GL_PROGRAM_INPUT: return StrCRef("PROGRAM_INPUT");
#endif
#if defined GL_PROGRAM_OUTPUT
	case GL_PROGRAM_OUTPUT: return StrCRef("PROGRAM_OUTPUT");
#endif
#if defined GL_VERTEX_SUBROUTINE
	case GL_VERTEX_SUBROUTINE: return StrCRef("VERTEX_SUBROUTINE");
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE
	case GL_TESS_CONTROL_SUBROUTINE: return StrCRef("TESS_CONTROL_SUBROUTINE");
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE
	case GL_TESS_EVALUATION_SUBROUTINE: return StrCRef("TESS_EVALUATION_SUBROUTINE");
#endif
#if defined GL_GEOMETRY_SUBROUTINE
	case GL_GEOMETRY_SUBROUTINE: return StrCRef("GEOMETRY_SUBROUTINE");
#endif
#if defined GL_FRAGMENT_SUBROUTINE
	case GL_FRAGMENT_SUBROUTINE: return StrCRef("FRAGMENT_SUBROUTINE");
#endif
#if defined GL_COMPUTE_SUBROUTINE
	case GL_COMPUTE_SUBROUTINE: return StrCRef("COMPUTE_SUBROUTINE");
#endif
#if defined GL_VERTEX_SUBROUTINE_UNIFORM
	case GL_VERTEX_SUBROUTINE_UNIFORM: return StrCRef("VERTEX_SUBROUTINE_UNIFORM");
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE_UNIFORM
	case GL_TESS_CONTROL_SUBROUTINE_UNIFORM: return StrCRef("TESS_CONTROL_SUBROUTINE_UNIFORM");
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
	case GL_TESS_EVALUATION_SUBROUTINE_UNIFORM: return StrCRef("TESS_EVALUATION_SUBROUTINE_UNIFORM");
#endif
#if defined GL_GEOMETRY_SUBROUTINE_UNIFORM
	case GL_GEOMETRY_SUBROUTINE_UNIFORM: return StrCRef("GEOMETRY_SUBROUTINE_UNIFORM");
#endif
#if defined GL_FRAGMENT_SUBROUTINE_UNIFORM
	case GL_FRAGMENT_SUBROUTINE_UNIFORM: return StrCRef("FRAGMENT_SUBROUTINE_UNIFORM");
#endif
#if defined GL_COMPUTE_SUBROUTINE_UNIFORM
	case GL_COMPUTE_SUBROUTINE_UNIFORM: return StrCRef("COMPUTE_SUBROUTINE_UNIFORM");
#endif
#if defined GL_TRANSFORM_FEEDEBACK_VARYING
	case GL_TRANSFORM_FEEDEBACK_VARYING: return StrCRef("TRANSFORM_FEEDEBACK_VARYING");
#endif
#if defined GL_BUFFER_VARIABLE
	case GL_BUFFER_VARIABLE: return StrCRef("BUFFER_VARIABLE");
#endif
#if defined GL_SHADER_STORAGE_BLOCK
	case GL_SHADER_STORAGE_BLOCK: return StrCRef("SHADER_STORAGE_BLOCK");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums

