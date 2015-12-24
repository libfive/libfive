//  File implement/oglplus/enums/program_resource_property_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ProgramResourceProperty
> ValueRange_(ProgramResourceProperty*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PROGRAMRESOURCEPROPERTY)
#define OGLPLUS_IMPL_EVR_PROGRAMRESOURCEPROPERTY
{
static const GLenum _values[] = {
#if defined GL_ARRAY_SIZE
GL_ARRAY_SIZE,
#endif
#if defined GL_OFFSET
GL_OFFSET,
#endif
#if defined GL_BLOCK_INDEX
GL_BLOCK_INDEX,
#endif
#if defined GL_ARRAY_STRIDE
GL_ARRAY_STRIDE,
#endif
#if defined GL_MATRIX_STRIDE
GL_MATRIX_STRIDE,
#endif
#if defined GL_IS_ROW_MAJOR
GL_IS_ROW_MAJOR,
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER_INDEX
GL_ATOMIC_COUNTER_BUFFER_INDEX,
#endif
#if defined GL_BUFFER_BINDING
GL_BUFFER_BINDING,
#endif
#if defined GL_BUFFER_DATA_SIZE
GL_BUFFER_DATA_SIZE,
#endif
#if defined GL_NUM_ACTIVE_VARIABLES
GL_NUM_ACTIVE_VARIABLES,
#endif
#if defined GL_ACTIVE_VARIABLES
GL_ACTIVE_VARIABLES,
#endif
#if defined GL_REFERENCED_BY_VERTEX_SHADER
GL_REFERENCED_BY_VERTEX_SHADER,
#endif
#if defined GL_REFERENCED_BY_TESS_CONTROL_SHADER
GL_REFERENCED_BY_TESS_CONTROL_SHADER,
#endif
#if defined GL_REFERENCED_BY_TESS_EVALUATION_SHADER
GL_REFERENCED_BY_TESS_EVALUATION_SHADER,
#endif
#if defined GL_REFERENCED_BY_GEOMETRY_SHADER
GL_REFERENCED_BY_GEOMETRY_SHADER,
#endif
#if defined GL_REFERENCED_BY_FRAGMENT_SHADER
GL_REFERENCED_BY_FRAGMENT_SHADER,
#endif
#if defined GL_REFERENCED_BY_COMPUTE_SHADER
GL_REFERENCED_BY_COMPUTE_SHADER,
#endif
#if defined GL_NUM_COMPATIBLE_SUBROUTINES
GL_NUM_COMPATIBLE_SUBROUTINES,
#endif
#if defined GL_COMPATIBLE_SUBROUTINES
GL_COMPATIBLE_SUBROUTINES,
#endif
#if defined GL_TOP_LEVEL_ARRAY_SIZE
GL_TOP_LEVEL_ARRAY_SIZE,
#endif
#if defined GL_TOP_LEVEL_ARRAY_STRIDE
GL_TOP_LEVEL_ARRAY_STRIDE,
#endif
#if defined GL_LOCATION
GL_LOCATION,
#endif
#if defined GL_LOCATION_INDEX
GL_LOCATION_INDEX,
#endif
#if defined GL_LOCATION_COMPONENT
GL_LOCATION_COMPONENT,
#endif
#if defined GL_IS_PER_PATCH
GL_IS_PER_PATCH,
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
GL_TRANSFORM_FEEDBACK_BUFFER_INDEX,
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ProgramResourceProperty
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

