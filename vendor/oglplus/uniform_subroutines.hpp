/**
 *  @file oglplus/uniform_subroutines.hpp
 *  @brief Wrapper for uniform subroutine operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_UNIFORM_SUBROUTINE_1107121519_HPP
#define OGLPLUS_UNIFORM_SUBROUTINE_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/set_ops.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <oglplus/shader_type.hpp>

#include <cassert>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_shader_subroutine

template <>
class ProgVarLocOps<tag::Subroutine>
{
private:
	ShaderType _stage;
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
	ProgVarLocOps(ShaderType stage)
	 : _stage(stage)
	{ }
public:
	/// Finds the subroutine location, throws on failure if active_only
	/** Finds the location / index of the subroutine specified
	 *  by @p identifier in the @p stage of a @p program. If active_only
	 *  is true then throws if no such subroutine exists or if it is
	 *  not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetSubroutineIndex}
	 */
	static GLint GetLocation(
		ProgramName program,
		ShaderType stage,
		StrCRef identifier,
		bool active_only
	)
	{
		GLuint result = OGLPLUS_GLFUNC(GetSubroutineIndex)(
			GetGLName(program),
			GLenum(stage),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetSubroutineIndex,
			ProgVarError,
			Program(program).
			Identifier(identifier).
			EnumParam(stage)
		);
		OGLPLUS_HANDLE_ERROR_IF(
			active_only && (result == GL_INVALID_INDEX),
			GL_INVALID_OPERATION,
			MsgGettingInactive(),
			ProgVarError,
			Program(program).
			Identifier(identifier).
			EnumParam(stage)
		);

		if(result == GL_INVALID_INDEX)
		{
			return -1;
		}
		return GLint(result);
	}

	/// Finds the subroutine location, throws on failure if active_only
	/** Finds the location / index of the subroutine specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such subroutine exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetSubroutineIndex}
	 */
	GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	) const
	{
		return GetLocation(
			program,
			this->_stage,
			identifier,
			active_only
		);
	}

	/// Returns this subroutine's program stage
	ShaderType Stage(void) const
	{
		return _stage;
	}
};

/// Specialization of ProgVar for subroutines
template <>
class ProgVar<tag::ImplicitSel, tag::Subroutine, tag::NoTypecheck, void>
 : public ProgVarLoc<tag::Subroutine>
{
public:
	/// Subroutine with the specified location
	ProgVar(SubroutineLoc pvloc)
	 : ProgVarLoc<tag::Subroutine>(pvloc)
	{ }

	/// Subroutine with the specified @p identifier in @p stage of @p program
	ProgVar(ProgramName program, ShaderType stage, StrCRef identifier)
	 : ProgVarLoc<tag::Subroutine>(stage, program, identifier)
	{ }

	/// Subroutine with the specified @p identifier in @p stage of @p program
	ProgVar(
		ProgramName program,
		ShaderType stage,
		StrCRef identifier,
		bool active_only
	): ProgVarLoc<tag::Subroutine>(stage, program, identifier, active_only)
	{ }
};

/// Subroutine
typedef ProgVar<tag::ImplicitSel, tag::Subroutine, tag::NoTypecheck, void>
	Subroutine;

template <>
class ProgVarLocOps<tag::SubroutineUniform>
{
private:
	ShaderType _stage;
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
	ProgVarLocOps(ShaderType stage)
	 : _stage(stage)
	{ }
public:
	/// Finds the subroutine uniform location, throws on failure if active_only
	/** Finds the location of the subroutine uniform specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such subroutine exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetSubroutineUniformLocation}
	 */
	static GLint GetLocation(
		ProgramName program,
		ShaderType stage,
		StrCRef identifier,
		bool active_only
	)
	{
		GLint result = OGLPLUS_GLFUNC(GetSubroutineUniformLocation)(
			GetGLName(program),
			GLenum(stage),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetSubroutineUniformLocation,
			ProgVarError,
			Program(program).
			Identifier(identifier).
			EnumParam(stage)
		);
		OGLPLUS_HANDLE_ERROR_IF(
			active_only && (result < 0),
			GL_INVALID_OPERATION,
			MsgGettingInactive(),
			ProgVarError,
			Program(program).
			Identifier(identifier).
			EnumParam(stage)
		);
		return result;
	}

	/// Finds the subroutine uniform location, throws on failure if active_only
	/** Finds the location of the subroutine uniform specified
	 *  by @p identifier in a @p program. If active_only is true then throws
	 *  if no such uniform exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetSubroutineUniformLocation}
	 */
	GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	) const
	{
		return GetLocation(
			program,
			this->_stage,
			identifier,
			active_only
		);
	}

	/// Returns this subroutine uniform's program stage
	ShaderType Stage(void) const
	{
		return _stage;
	}
};

/// Specialization of ProgVar for subroutine uniformss
template <>
class ProgVar<tag::ImplicitSel, tag::SubroutineUniform, tag::NoTypecheck, void>
 : public ProgVarLoc<tag::SubroutineUniform>
{
public:
	/// Subroutine uniform with the specified location
	ProgVar(SubroutineUniformLoc pvloc)
	 : ProgVarLoc<tag::SubroutineUniform>(pvloc)
	{ }

	/// Sub.uniform with the specified @p identifier in @p stage of @p program
	ProgVar(ProgramName program, ShaderType stage, StrCRef identifier)
	 : ProgVarLoc<tag::SubroutineUniform>(stage, program, identifier)
	{ }

	/// Sub.uniform with the specified @p identifier in @p stage of @p program
	ProgVar(
		ProgramName program,
		ShaderType stage,
		StrCRef identifier,
		bool active_only
	): ProgVarLoc<tag::SubroutineUniform>(
		stage,
		program,
		identifier,
		active_only
	){ }
};

/// SubroutineUniform
typedef ProgVar<tag::ImplicitSel, tag::SubroutineUniform, tag::NoTypecheck, void>
	SubroutineUniform;

/// Encapsulates the uniform subroutine setting operations
/**
 *  @ingroup shader_variables
 *
 *  @glvoereq{4,0,ARB,shader_subroutine}
 */
class UniformSubroutines
{
private:
	ProgramName _program;
	ShaderType _stage;
	std::vector<GLuint> _indices;

	static std::size_t _get_location_count(
		ProgramName program,
		ShaderType stage
	)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetProgramStageiv)(
			GetGLName(program),
			GLenum(stage),
			GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS,
			&result
		);
		OGLPLUS_VERIFY(
			GetProgramStageiv,
			ObjectError,
			Object(program).
			EnumParam(stage)
		);

		assert(!(result < 0));
		return std::size_t(result);
	}

	std::vector<GLuint>& _get_indices(void)
	{
		if(_indices.empty())
		{
			_indices.resize(
				_get_location_count(
					_program,
					_stage
				), 0
			);
		}
		return _indices;
	}
public:
	/// Constructs a uniform subroutine setter for a @p stage of a @p program
	UniformSubroutines(
		ProgramName program,
		ShaderType stage
	): _program(program)
	 , _stage(stage)
	{ }

	/// Assigns the @p subroutine to the subroutine @p uniform
	/**
	 *  @note This function does not apply the changes to the actual
	 *  uniform variables in the stage of the program. Use Apply
	 *  function to do this after the subroutines are assigned.
	 *
	 *  @see Apply
	 */
	UniformSubroutines& Assign(
		const SubroutineUniform& uniform,
		const Subroutine& subroutine
	)
	{
		assert(uniform.Program() == _program);
		assert(subroutine.Program() == _program);
		assert(uniform.Stage() == ShaderType(_stage));
		assert(subroutine.Stage() == ShaderType(_stage));
		assert(subroutine.Location() >= 0);
		assert(uniform.IsActive());
		assert(uniform.Location() <= GLint(_get_indices().size()));
		assert(uniform.Location() >= 0);

		_get_indices()[std::size_t(uniform.Location())] =
			GLuint(subroutine.Location());

		return *this;
	}

	/// This type stores a setting of the whole set of subroutine uniforms
	/** Preset stores a whole setting of a set of subroutine uniforms
	 *  which can be later applied or loaded.
	 *
	 *  Applications should treat this type as opaque and use it
	 *  only with the Save, Load and Apply functions.
	 *
	 *  @see Save
	 *  @see Load
	 *  @see Apply
	 */
	class Preset
	{
	private:
		friend class UniformSubroutines;

		const std::vector<GLuint> _indices;
#ifndef NDEBUG
		ProgramName _program;
		ShaderType _stage;
#endif

		Preset(
#ifndef NDEBUG
			ProgramName program,
			ShaderType stage,
#endif
			const std::vector<GLuint>& indices
		): _indices(indices)
#ifndef NDEBUG
		 , _program(program)
		 , _stage(stage)
#endif
		{ }
	public:
		Preset(Preset&& tmp)
		 : _indices(std::move(tmp._indices))
#ifndef NDEBUG
		 , _program(tmp._program)
		 , _stage(tmp._stage)
#endif
		{ }
	};

	/// Saves the current setting of subroutine uniforms into a preset
	/**
	 *  @see Preset
	 *  @see Apply
	 *  @see Load
	 */
	Preset Save(void)
	{
		return Preset(
#ifndef NDEBUG
			_program,
			_stage,
#endif
			_get_indices()
		);
	}

	/// Loads the setting of subroutine uniforms from a @p preset
	/** @note Only presets from the same instance of UniformSubroutines
	 *  that saved them can be loaded or applied.
	 *
	 *  @see Preset
	 *  @see Apply
	 *  @see Save
	 */
	void Load(const Preset& preset)
	{
		assert(_program == preset._program);
		assert(_stage == preset._stage);
		assert(_get_indices().size() == preset._indices.size());
		_get_indices() = preset._indices;
	}

	/// Applies the setting from a preset without changing the current setting
	/** @note Only presets from the same instance of UniformSubroutines
	 *  that saved them can be loaded or applied.
	 *
	 *  @see Preset
	 *  @see Save
	 *  @see Load
	 */
	void Apply(const Preset& preset)
	{
		assert(_program == preset._program);
		assert(_stage == preset._stage);
		assert(_get_indices().size() == preset._indices.size());

		OGLPLUS_GLFUNC(UniformSubroutinesuiv)(
			GLenum(_stage),
			GLsizei(preset._indices.size()),
			preset._indices.data()
		);
		OGLPLUS_CHECK(
			UniformSubroutinesuiv,
			Error,
			EnumParam(_stage)
		);
	}

	/// Applies all changes made by Assign
	/**
	 *  @see Assign
	 */
	void Apply(void)
	{
		OGLPLUS_GLFUNC(UniformSubroutinesuiv)(
			GLenum(_stage),
			GLsizei(_get_indices().size()),
			_get_indices().data()
		);
		OGLPLUS_CHECK(
			UniformSubroutinesuiv,
			Error,
			EnumParam(_stage)
		);
	}
};

#endif // GL_ARB_shader_subroutine

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/uniform_subroutines.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
