/**
 *  @file oglplus/context/capabilities.hpp
 *  @brief Wrappers for OpenGL capability-related functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_CAPABILITIES_1201040722_HPP
#define OGLPLUS_CONTEXT_CAPABILITIES_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/capability.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the capability-related commands
/**
 *  @ingroup ogl_context
 */
class Capabilities
{
public:
	/// Enable a @p capability
	/**
	 *  @glsymbols
	 *  @glfunref{Enable}
	 */
	static void Enable(Capability capability)
	{
		OGLPLUS_GLFUNC(Enable)(GLenum(capability));
		OGLPLUS_VERIFY(
			Enable,
			Error,
			EnumParam(capability)
		);
	}

	/// Enable a @p functionality
	/**
	 *  @glsymbols
	 *  @glfunref{Enable}
	 */
	static void Enable(Functionality functionality, GLuint number)
	{
		OGLPLUS_GLFUNC(Enable)(GLenum(functionality)+number);
		OGLPLUS_VERIFY(
			Enable,
			Error,
			EnumParam(functionality).
			Index(number)
		);
	}

	/// Disable a @p capability
	/**
	 *  @glsymbols
	 *  @glfunref{Disable}
	 */
	static void Disable(Capability capability)
	{
		OGLPLUS_GLFUNC(Disable)(GLenum(capability));
		OGLPLUS_VERIFY(
			Disable,
			Error,
			EnumParam(capability)
		);
	}

	/// Disable a @p functionality
	/**
	 *  @glsymbols
	 *  @glfunref{Disable}
	 */
	static void Disable(Functionality functionality, GLuint number)
	{
		OGLPLUS_GLFUNC(Disable)(GLenum(functionality)+number);
		OGLPLUS_VERIFY(
			Disable,
			Error,
			EnumParam(functionality).
			Index(number)
		);
	}

	/// Checks if a @p capability is enabled
	/**
	 *  @glsymbols
	 *  @glfunref{IsEnabled}
	 */
	static Boolean IsEnabled(Capability capability)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsEnabled)(GLenum(capability)),
			std::nothrow
		);
		OGLPLUS_VERIFY(
			IsEnabled,
			Error,
			EnumParam(capability)
		);
		return result;
	}

	/// Checks if a @p functionality is enabled
	/**
	 *  @glsymbols
	 *  @glfunref{IsEnabled}
	 */
	static Boolean IsEnabled(Functionality functionality, GLuint number)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsEnabled)(
				GLenum(functionality)+
				number
			), std::nothrow
		);
		OGLPLUS_VERIFY(
			IsEnabled,
			Error,
			EnumParam(functionality).
			Index(number)
		);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Enable a @p capability for an indexed target
	/**
	 *  @glsymbols
	 *  @glfunref{Enablei}
	 */
	static void Enable(Capability capability, GLuint index)
	{
		OGLPLUS_GLFUNC(Enablei)(GLenum(capability), index);
		OGLPLUS_VERIFY(
			Enablei,
			Error,
			EnumParam(capability).
			Index(index)
		);
	}

	/// Disable a @p capability for an indexed target
	/**
	 *  @glsymbols
	 *  @glfunref{Disablei}
	 */
	static void Disable(Capability capability, GLuint index)
	{
		OGLPLUS_GLFUNC(Disablei)(GLenum(capability), index);
		OGLPLUS_VERIFY(
			Disablei,
			Error,
			EnumParam(capability).
			Index(index)
		);
	}

	/// Check if a @p capability is enabled for indexed target
	/**
	 *  @glsymbols
	 *  @glfunref{IsEnabledi}
	 */
	static Boolean IsEnabled(Capability capability, GLuint index)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsEnabledi)(
				GLenum(capability),
				index
			), std::nothrow
		);
		OGLPLUS_VERIFY(
			IsEnabledi,
			Error,
			EnumParam(capability).
			Index(index)
		);
		return result;
	}
#endif // GL_VERSION_3_0
};

} // namespace context
} // namespace oglplus

#endif // include guard
