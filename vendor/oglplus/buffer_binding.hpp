/**
 *  @file oglplus/buffer_binding.hpp
 *  @brief Buffer binding point indices
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BUFFER_BINDING_1107121519_HPP
#define OGLPLUS_BUFFER_BINDING_1107121519_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the uniform buffer binding point index
class UniformBufferBindingPoint
 : public LimitedCount
{
public:
	/// Construction from a @c GLuint
	UniformBufferBindingPoint(GLuint count);
};
#elif GL_VERSION_3_1 || GL_ARB_uniform_buffer_object
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	UniformBufferBindingPoint,
	MAX_UNIFORM_BUFFER_BINDINGS
)
#else
typedef GLuint UniformBufferBindingPoint;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the transform feedback buffer binding point index
class TransformFeedbackBufferBindingPoint
 : public LimitedCount
{
public:
	/// Construction from a @c GLuint
	TransformFeedbackBufferBindingPoint(GLuint count);
};
#elif GL_VERSION_4_0 || GL_ARB_transform_feedback3
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	TransformFeedbackBufferBindingPoint,
	MAX_TRANSFORM_FEEDBACK_BUFFERS
)
#else
typedef GLuint TransformFeedbackBufferBindingPoint;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the atomic counter buffer binding point index
class AtomicCounterBufferBindingPoint
 : public LimitedCount
{
public:
	/// Construction from a @c GLuint
	AtomicCounterBufferBindingPoint(GLuint count);
};
#elif GL_VERSION_4_2 || GL_ARB_shader_atomic_counters
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	AtomicCounterBufferBindingPoint,
	MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
)
#else
typedef GLuint AtomicCounterBufferBindingPoint;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the shader storage buffer binding point index
class ShaderStorageBufferBindingPoint
 : public LimitedCount
{
public:
	/// Construction from a @c GLuint
	ShaderStorageBufferBindingPoint(GLuint count);
};
#elif GL_VERSION_4_3 || GL_ARB_shader_storage_buffer_object
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	ShaderStorageBufferBindingPoint,
	MAX_SHADER_STORAGE_BUFFER_BINDINGS
)
#else
typedef GLuint ShaderStorageBufferBindingPoint;
#endif

} // namespace oglplus

#endif // include guard
