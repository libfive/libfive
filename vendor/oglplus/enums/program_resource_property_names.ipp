//  File implement/oglplus/enums/program_resource_property_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/program_resource_property.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ProgramResourceProperty*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PROGRAMRESOURCEPROPERTY)
#define OGLPLUS_IMPL_EVN_PROGRAMRESOURCEPROPERTY
{
switch(value)
{
#if defined GL_ARRAY_SIZE
	case GL_ARRAY_SIZE: return StrCRef("ARRAY_SIZE");
#endif
#if defined GL_OFFSET
	case GL_OFFSET: return StrCRef("OFFSET");
#endif
#if defined GL_BLOCK_INDEX
	case GL_BLOCK_INDEX: return StrCRef("BLOCK_INDEX");
#endif
#if defined GL_ARRAY_STRIDE
	case GL_ARRAY_STRIDE: return StrCRef("ARRAY_STRIDE");
#endif
#if defined GL_MATRIX_STRIDE
	case GL_MATRIX_STRIDE: return StrCRef("MATRIX_STRIDE");
#endif
#if defined GL_IS_ROW_MAJOR
	case GL_IS_ROW_MAJOR: return StrCRef("IS_ROW_MAJOR");
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER_INDEX
	case GL_ATOMIC_COUNTER_BUFFER_INDEX: return StrCRef("ATOMIC_COUNTER_BUFFER_INDEX");
#endif
#if defined GL_BUFFER_BINDING
	case GL_BUFFER_BINDING: return StrCRef("BUFFER_BINDING");
#endif
#if defined GL_BUFFER_DATA_SIZE
	case GL_BUFFER_DATA_SIZE: return StrCRef("BUFFER_DATA_SIZE");
#endif
#if defined GL_NUM_ACTIVE_VARIABLES
	case GL_NUM_ACTIVE_VARIABLES: return StrCRef("NUM_ACTIVE_VARIABLES");
#endif
#if defined GL_ACTIVE_VARIABLES
	case GL_ACTIVE_VARIABLES: return StrCRef("ACTIVE_VARIABLES");
#endif
#if defined GL_REFERENCED_BY_VERTEX_SHADER
	case GL_REFERENCED_BY_VERTEX_SHADER: return StrCRef("REFERENCED_BY_VERTEX_SHADER");
#endif
#if defined GL_REFERENCED_BY_TESS_CONTROL_SHADER
	case GL_REFERENCED_BY_TESS_CONTROL_SHADER: return StrCRef("REFERENCED_BY_TESS_CONTROL_SHADER");
#endif
#if defined GL_REFERENCED_BY_TESS_EVALUATION_SHADER
	case GL_REFERENCED_BY_TESS_EVALUATION_SHADER: return StrCRef("REFERENCED_BY_TESS_EVALUATION_SHADER");
#endif
#if defined GL_REFERENCED_BY_GEOMETRY_SHADER
	case GL_REFERENCED_BY_GEOMETRY_SHADER: return StrCRef("REFERENCED_BY_GEOMETRY_SHADER");
#endif
#if defined GL_REFERENCED_BY_FRAGMENT_SHADER
	case GL_REFERENCED_BY_FRAGMENT_SHADER: return StrCRef("REFERENCED_BY_FRAGMENT_SHADER");
#endif
#if defined GL_REFERENCED_BY_COMPUTE_SHADER
	case GL_REFERENCED_BY_COMPUTE_SHADER: return StrCRef("REFERENCED_BY_COMPUTE_SHADER");
#endif
#if defined GL_NUM_COMPATIBLE_SUBROUTINES
	case GL_NUM_COMPATIBLE_SUBROUTINES: return StrCRef("NUM_COMPATIBLE_SUBROUTINES");
#endif
#if defined GL_COMPATIBLE_SUBROUTINES
	case GL_COMPATIBLE_SUBROUTINES: return StrCRef("COMPATIBLE_SUBROUTINES");
#endif
#if defined GL_TOP_LEVEL_ARRAY_SIZE
	case GL_TOP_LEVEL_ARRAY_SIZE: return StrCRef("TOP_LEVEL_ARRAY_SIZE");
#endif
#if defined GL_TOP_LEVEL_ARRAY_STRIDE
	case GL_TOP_LEVEL_ARRAY_STRIDE: return StrCRef("TOP_LEVEL_ARRAY_STRIDE");
#endif
#if defined GL_LOCATION
	case GL_LOCATION: return StrCRef("LOCATION");
#endif
#if defined GL_LOCATION_INDEX
	case GL_LOCATION_INDEX: return StrCRef("LOCATION_INDEX");
#endif
#if defined GL_LOCATION_COMPONENT
	case GL_LOCATION_COMPONENT: return StrCRef("LOCATION_COMPONENT");
#endif
#if defined GL_IS_PER_PATCH
	case GL_IS_PER_PATCH: return StrCRef("IS_PER_PATCH");
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
	case GL_TRANSFORM_FEEDBACK_BUFFER_INDEX: return StrCRef("TRANSFORM_FEEDBACK_BUFFER_INDEX");
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
	case GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE: return StrCRef("TRANSFORM_FEEDBACK_BUFFER_STRIDE");
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

