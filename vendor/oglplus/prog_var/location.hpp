/**
 *  @file oglplus/prog_var/location.hpp
 *  @brief Program variable location wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_LOCATION_1405052234_HPP
#define OGLPLUS_PROG_VAR_LOCATION_1405052234_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/object/tags.hpp>
#include <oglplus/object/name.hpp>
#include <oglplus/error/prog_var.hpp>

namespace oglplus {

template <typename VarTag>
GLint GetGLLocation(ProgVarLoc<VarTag>)
OGLPLUS_NOEXCEPT(true);

/// Wrapper encapsulating program variable location/index
template <typename VarTag>
class ProgVarLoc
 : public ProgVarLocOps<VarTag>
{
protected:
	friend GLint GetGLLocation<VarTag>(ProgVarLoc)
	OGLPLUS_NOEXCEPT(true);

	GLuint _program;
	GLint _location;

	void RequireActive(StrCRef identifier) const
	{
		OGLPLUS_HANDLE_ERROR_IF(
			!IsActive(),
			GL_INVALID_OPERATION,
			ProgVarLocOps<VarTag>::MsgUsingInactive(),
			ProgVarError,
			Program(ProgramName(_program)).
			Identifier(identifier)
		);
	}

	void RequireActive(void) const
	{
		RequireActive(StrCRef());
	}
public:
	/// Default construction
	ProgVarLoc(void)
	OGLPLUS_NOEXCEPT(true)
	 : _program(0u)
	 , _location(-1)
	{ }

	/// Creates variable without specific @p location in specified @p program
	ProgVarLoc(ProgramName program)
	OGLPLUS_NOEXCEPT(true)
	 : _program(GetGLName(program))
	 , _location(-1)
	{ }

	/// Creates variable with specified @p location without specific program
	OGLPLUS_EXPLICIT
	ProgVarLoc(GLint location)
	OGLPLUS_NOEXCEPT(true)
	 : _program(0)
	 , _location(location)
	{ }

	/// Creates variable with specified @p location in specified @p program
	ProgVarLoc(ProgramName program, GLint location)
	OGLPLUS_NOEXCEPT(true)
	 : _program(GetGLName(program))
	 , _location(location)
	{ }

	/// Creates variable with specified @p identifier in specified @p program
	ProgVarLoc(ProgramName program, StrCRef identifier)
	 : _program(GetGLName(program))
	 , _location(ProgVarLocOps<VarTag>::GetLocation(
		program,
		identifier,
		true
	)){ }

	template <typename LocOpsParam>
	ProgVarLoc(LocOpsParam param, ProgramName program, StrCRef identifier)
	 : ProgVarLocOps<VarTag>(param)
	 , _program(GetGLName(program))
	 , _location(ProgVarLocOps<VarTag>::GetLocation(
		program,
		identifier,
		true
	)){ }

	/// Creates variable with specified @p identifier in specified @p program
	ProgVarLoc(ProgramName program, StrCRef identifier, bool active_only)
	 : _program(GetGLName(program))
	 , _location(ProgVarLocOps<VarTag>::GetLocation(
		program,
		identifier,
		active_only
	)){ }

	template <typename LocOpsParam>
	ProgVarLoc(
		LocOpsParam param,
		ProgramName program,
		StrCRef identifier,
		bool active_only
	): ProgVarLocOps<VarTag>(param)
	 , _program(GetGLName(program))
	 , _location(ProgVarLocOps<VarTag>::GetLocation(
		program,
		identifier,
		active_only
	)){ }

	/// Copy assignment
	ProgVarLoc& Assign(ProgVarLoc that)
	{
		this->_program = that._program;
		this->_location= that._location;
		return *this;
	}

	/// Late initialization of the variable location from its identifier
	ProgVarLoc& BindTo(StrCRef identifier, bool is_active = true)
	{
		_location = ProgVarLocOps<VarTag>::GetLocation(
			ProgramName(_program),
			identifier,
			is_active
		);
		return *this;
	}

	/// The program the variable belongs to
	ProgramName Program(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return ProgramName(_program);
	}

	/// Returns the location of the variable
	GLint Location(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _location;
	}

	/// Returns true if the variable is active
	bool IsActive(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _location >= 0;
	}

	/// Returns true if the variable is active
	OGLPLUS_EXPLICIT operator bool (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return IsActive();
	}

	/// Equality comparison
	friend bool operator == (ProgVarLoc a, ProgVarLoc b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._program == b._program)&&(a._location == b._location);
	}

	/// Inequality comparison
	friend bool operator != (ProgVarLoc a, ProgVarLoc b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._program != b._program)||(a._location != b._location);
	}

	/// Ordering
	friend bool operator < (ProgVarLoc a, ProgVarLoc b)
	OGLPLUS_NOEXCEPT(true)
	{
		if(a._program <  b._program) return true;
		if(a._program == b._program) return (a._location < b._location);
		return false;
	}
};

/// Returns the GL location/index of the specified @p variable
template <typename VarTag>
inline
GLint GetGLLocation(ProgVarLoc<VarTag> variable)
OGLPLUS_NOEXCEPT(true)
{
	return variable._location;
}

} // namespace oglplus

#endif // include guard
