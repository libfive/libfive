/**
 *  @file oglplus/dsa/vertex_array.hpp
 *  @brief VertexArray wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_VERTEX_ARRAY_1107121519_HPP
#define OGLPLUS_DSA_VERTEX_ARRAY_1107121519_HPP

#include <oglplus/vertex_array.hpp>
#include <oglplus/vertex_attrib_slot.hpp>
#include <oglplus/data_type.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

template <>
struct ObjGenTag<tag::DirectState, tag::VertexArray>
{
	typedef tag::Create Type;
};

/// Class wrapping vertex array-related functionality with direct state access
/** @note Do not use this class directly, use DSAVertexArray instead.
 *
 */
template <>
class ObjectOps<tag::DirectState, tag::VertexArray>
 : public ObjZeroOps<tag::DirectState, tag::VertexArray>
{
protected:
	ObjectOps(VertexArrayName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::VertexArray>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::VertexArray> _base;

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
	/// Bind buffer to VAO's element buffer binding point
	/**
	 *  @glsymbols
	 *  @glfunref{VertexArrayElementBuffer}
	 */
	ObjectOps& ElementBuffer(BufferName buffer)
	{
		OGLPLUS_GLFUNC(VertexArrayElementBuffer)(
			_obj_name(),
			GetGLName(buffer)
		);
		OGLPLUS_CHECK(
			VertexArrayElementBuffer,
			ObjectPairError,
			Subject(buffer).
			Object(*this)
		);
		return *this;
	}

	/// Enable the specified vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{EnableVertexArrayAttrib}
	 */
	ObjectOps& EnableVertexAttrib(VertexAttribSlot location)
	{
		OGLPLUS_GLFUNC(EnableVertexArrayAttrib)(
			_obj_name(),
			GLuint(location)
		);
		OGLPLUS_CHECK(
			EnableVertexArrayAttrib,
			ObjectError,
			Object(*this).
			Index(GLuint(location))
		);
		return *this;
	}

	/// Disable the specified vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{DisableVertexArrayAttrib}
	 */
	ObjectOps& DisableVertexAttrib(VertexAttribSlot location)
	{
		OGLPLUS_GLFUNC(DisableVertexArrayAttrib)(
			_obj_name(),
			GLuint(location)
		);
		OGLPLUS_CHECK(
			DisableVertexArrayAttrib,
			ObjectError,
			Object(*this).
			Index(GLuint(location))
		);
		return *this;
	}
};

/// VertexArray operations with direct state access
typedef ObjectOps<tag::DirectState, tag::VertexArray>
	DSAVertexArrayOps;

/// An @ref oglplus_object encapsulating the OpenGL vertex array functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSAVertexArrayOps> DSAVertexArray;

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#endif // include guard
