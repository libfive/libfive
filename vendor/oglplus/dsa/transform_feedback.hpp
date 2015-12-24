/**
 *  @file oglplus/dsa/transform_feedback.hpp
 *  @brief TransformFeedback object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_TRANSFORM_FEEDBACK_1107121519_HPP
#define OGLPLUS_DSA_TRANSFORM_FEEDBACK_1107121519_HPP

#include <oglplus/transform_feedback.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/object/name.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

template <>
struct ObjGenTag<tag::DirectState, tag::TransformFeedback>
{
	typedef tag::Create Type;
};

/// Class wrapping transform feedback-related functionality with direct state access
/** @note Do not use this class directly, use DSATransformFeedback instead.
 *
 */
template <>
class ObjectOps<tag::DirectState, tag::TransformFeedback>
 : public ObjZeroOps<tag::DirectState, tag::TransformFeedback>
{
protected:
	ObjectOps(TransformFeedbackName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::TransformFeedback>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::TransformFeedback> _base;

	ObjectOps(ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	{ }

	ObjectOps(const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	{ }

	ObjectOps& operator = (ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		return *this;
	}

	ObjectOps& operator = (const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		return *this;
	}
#endif
	GLint GetIntParam(GLenum query) const;
	GLint GetIntParam(GLenum query, GLuint index) const;
	GLint64 GetInt64Param(GLenum query, GLuint index) const;

	Boolean Active(void) const
	{
		return Boolean(
			GetIntParam(GL_TRANSFORM_FEEDBACK_ACTIVE),
			std::nothrow
		);
	}

	Boolean Paused(void) const
	{
		return Boolean(
			GetIntParam(GL_TRANSFORM_FEEDBACK_PAUSED),
			std::nothrow
		);
	}

	ObjectOps& BufferBase(GLuint index, BufferName buffer)
	{
		OGLPLUS_GLFUNC(TransformFeedbackBufferBase)(
			_obj_name(),
			index,
			GetGLName(buffer)
		);
		OGLPLUS_CHECK(
			TransformFeedbackBufferBase,
			ObjectPairError,
			Subject(buffer).
			Object(*this).
			Index(index)
		);
		return *this;
	}

	ObjectOps& BufferRange(
		GLuint index,
		BufferName buffer,
		BufferSize offset,
		BufferSize size
	)
	{
		OGLPLUS_GLFUNC(TransformFeedbackBufferRange)(
			_obj_name(),
			index,
			GetGLName(buffer),
			GLintptr(offset.Get()),
			GLsizeiptr(size.Get())
		);
		OGLPLUS_CHECK(
			TransformFeedbackBufferRange,
			ObjectPairError,
			Subject(buffer).
			Object(*this).
			Index(index)
		);
		return *this;
	}

	BufferName BufferBinding(GLuint index) const
	{
		return BufferName(GetIntParam(
			GL_TRANSFORM_FEEDBACK_BUFFER_BINDING,
			index
		));
	}

	GLint64 BufferStart(GLuint index) const
	{
		return GetInt64Param(
			GL_TRANSFORM_FEEDBACK_BUFFER_START,
			index
		);
	}

	GLint64 BufferSize(GLuint index) const
	{
		return GetInt64Param(
			GL_TRANSFORM_FEEDBACK_BUFFER_SIZE,
			index
		);
	}
};

/// TransformFeedback operations with direct state access
typedef ObjectOps<tag::DirectState, tag::TransformFeedback>
	DSATransformFeedbackOps;

/// An @ref oglplus_object encapsulating the OpenGL transform feedback functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSATransformFeedbackOps> DSATransformFeedback;

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/transform_feedback.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
