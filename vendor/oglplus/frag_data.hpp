/**
 *  @file oglplus/frag_data.hpp
 *  @brief Fragment data output wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_FRAG_DATA_1107121519_HPP
#define OGLPLUS_FRAG_DATA_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <oglplus/frag_data_slot.hpp>

#include <cassert>

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0

namespace oglplus {

template <>
class ProgVarLocOps<tag::FragData>
{
private:
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
public:
	/// Bind the fragment data location
	/**
	 *  @see GetLocation
	 *  @see QueryLocation
	 *
	 *  @glsymbols
	 *  @glfunref{BindFragDataLocation}
	 */
	static void BindLocation(
		ProgramName program,
		FragDataSlot location,
		StrCRef identifier
	)
	{
		OGLPLUS_GLFUNC(BindFragDataLocation)(
			GetGLName(program),
			GLuint(location),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			BindFragDataLocation,
			ProgVarError,
			Program(program).
			Identifier(identifier).
			Index(GLuint(location))
		);
	}

	/// Finds the fragment data location, throws on failure if active_only
	/** Finds the location / index of the fragment data specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such fragment data output exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetFragDataLocation}
	 */
	static GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	)
	{
		GLint result = OGLPLUS_GLFUNC(GetFragDataLocation)(
			GetGLName(program),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetFragDataLocation,
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);
		OGLPLUS_HANDLE_ERROR_IF(
			active_only && (result < 0),
			GL_INVALID_OPERATION,
			MsgGettingInactive(),
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);
		return result;
	}
};

template <>
class ProgVarCommonOps<tag::FragData>
 : public ProgVarLoc<tag::FragData>
{
protected:
	ProgVarCommonOps(FragDataLoc fdloc)
	 : ProgVarLoc<tag::FragData>(fdloc)
	{ }
public:
	void Bind(StrCRef identifier)
	{
		BindLocation(
			this->Program(),
			this->_location,
			identifier
		);
	}
};

/// Encapsulates frag data operations
/**
 *  @see FragData
 *
 *  @ingroup shader_variables
 */
typedef ProgVar<
	tag::ImplicitSel,
	tag::FragData,
	tag::NoTypecheck,
	void
> FragData;

typedef FragData FragmentData;

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/frag_data.ipp>
#endif

#endif // GL_VERSION_3_0

#endif // include guard
