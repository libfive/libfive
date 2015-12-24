/**
 *  @file oglplus/program_pipeline.hpp
 *  @brief ProgramPipeline object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROGRAM_PIPELINE_1107121519_HPP
#define OGLPLUS_PROGRAM_PIPELINE_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/shader_type.hpp>
#include <oglplus/program_pipeline_stage.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/error/program.hpp>
#include <oglplus/error/outcome.hpp>
#include <oglplus/detail/prog_pl_stages.hpp>

#include <cassert>

namespace oglplus {

// if program-pipeline is available
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_separate_shader_objects

/// Class wrapping program pipeline construction/destruction functions
/** @note Do not use this class directly, use ProgramPipeline instead.
 *
 *  @glvoereq{4,1,ARB,separate_shader_objects}
 *  @glsymbols
 *  @glfunref{GenProgramPipelines}
 *  @glfunref{DeleteProgramPipelines}
 *  @glfunref{IsProgramPipeline}
 */
template <>
class ObjGenDelOps<tag::ProgramPipeline>
{
protected:
	static void Gen(tag::Generate, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(GenProgramPipelines)(count, names);
		OGLPLUS_CHECK_SIMPLE(GenProgramPipelines);
	}

#if GL_VERSION_4_5 || GL_ARB_direct_state_access
	static void Gen(tag::Create, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(CreateProgramPipelines)(count, names);
		OGLPLUS_CHECK_SIMPLE(CreateProgramPipelines);
	}
#endif

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(DeleteProgramPipelines)(count, names);
		OGLPLUS_VERIFY_SIMPLE(DeleteProgramPipelines);
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsProgramPipeline)(name),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsProgramPipeline);
		return result;
	}
};

/// Program pipeline binding operations
template <>
class ObjBindingOps<tag::ProgramPipeline>
{
protected:
	static GLuint _binding(void)
	{
		GLint name = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_PROGRAM_PIPELINE_BINDING, &name);
		OGLPLUS_VERIFY(
			GetIntegerv,
			Error,
			EnumParam(GLenum(GL_PROGRAM_PIPELINE_BINDING))
		);

		assert(!(name < 0));

		return GLuint(name);
	}
public:
	/// Returns the currently bound ProgramPipeline
	/**
	 *  @glsymbols
	 *  @glfunref{GetIntegerv}
	 */
	static ProgramPipelineName Binding(void)
	{
		return ProgramPipelineName(_binding());
	}

	/// Binds the specified @p vertex_array object
	/**
	 *  @glsymbols
	 *  @glfunref{BindProgramPipeline}
	 */
	static void Bind(ProgramPipelineName pipeline)
	{
		OGLPLUS_GLFUNC(BindProgramPipeline)(GetGLName(pipeline));
		OGLPLUS_VERIFY(
			BindProgramPipeline,
			ObjectError,
			Object(pipeline)
		);
	}
};

/// Common program pipeline operations
/** @note Do not use this class directly, use ProgramPipeline
 *  or NoProgramPipeline instead.
 */
template <>
class ObjCommonOps<tag::ProgramPipeline>
 : public ProgramPipelineName
 , public ObjBindingOps<tag::ProgramPipeline>
{
protected:
	ObjCommonOps(ProgramPipelineName name)
	OGLPLUS_NOEXCEPT(true)
	 : ProgramPipelineName(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjCommonOps(ObjCommonOps&&) = default;
	ObjCommonOps(const ObjCommonOps&) = default;
	ObjCommonOps& operator = (ObjCommonOps&&) = default;
	ObjCommonOps& operator = (const ObjCommonOps&) = default;
#else
	typedef ProgramPipelineName _base1;
	typedef ObjBindingOps<tag::ProgramPipeline> _base2;

	ObjCommonOps(ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<_base1&&>(temp))
	 , _base2(static_cast<_base2&&>(temp))
	{ }

	ObjCommonOps(const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<const _base1&>(that))
	 , _base2(static_cast<const _base2&>(that))
	{ }

	ObjCommonOps& operator = (ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<_base1&&>(temp));
		_base2::operator = (static_cast<_base2&&>(temp));
		return *this;
	}

	ObjCommonOps& operator = (const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<const _base1&>(that));
		_base2::operator = (static_cast<const _base2&>(that));
		return *this;
	}
#endif
	using ObjBindingOps<tag::ProgramPipeline>::Bind;

	/// Binds this program pipeline object
	/**
	 *  @glsymbols
	 *  @glfunref{BindProgramPipeline}
	 */
	void Bind(void) const
	{
		Bind(*this);
	}
};

/// Class wrapping program pipeline functions (with direct state access)
/** @note Do not use this class directly, use ProgramPipeline instead.
 */
template <>
class ObjectOps<tag::DirectState, tag::ProgramPipeline>
 : public ObjZeroOps<tag::DirectState, tag::ProgramPipeline>
{
protected:
	ObjectOps(ProgramPipelineName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::ProgramPipeline>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::ProgramPipeline> _base;

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
	/// Types related to ProgramPipeline
	struct Properties
	{
		/// The stage of a ProgramPipeline
		typedef ProgramPipelineStage Stage;
	};

	GLint GetIntParam(GLenum query) const
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetProgramPipelineiv)(
			_obj_name(),
			query,
			&result
		);
		OGLPLUS_VERIFY(
			GetProgramPipelineiv,
			ObjectError,
			Object(*this).
			EnumParam(query)
		);
		return result;
	}

	GLuint GetUIntParam(GLenum query) const
	{
		GLint res = GetIntParam(query);
		assert(!(res < 0));
		return GLuint(res);
	}

	/// Specifies program stages by calling functions of the returned object
	/** This function returns an object that allows to specify which stages
	 *  of @p program should by used when this pipeline is active by calling
	 *  the Vertex(), TessControl(), TessEvaluation(), Geometry(), Fragment()
	 *  and All() member functions of the object returned by UseStages.
	 *
	 *  example:
	 *  @code
	 *  Program prog;
	 *  ProgramPipeline pp;
	 *  ...
	 *  pp.UseStages(prog).Vertex();
	 *  pp.UseStages(prog).Vertex().Geometry();
	 *  pp.UseStages(prog).Vertex().TessControl().TessEvaluation().Geometry();
	 *  pp.UseStages(prog).Vertex().Geometry().Fragment();
	 *  pp.UseStages(prog).Geometry();
	 *  pp.UseStages(prog).All();
	 *  @endcode
	 *
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{UseProgramStages}
	 */
	ProgPLUseStages UseStages(ProgramName program) const
	{
		return ProgPLUseStages(
			_obj_name(),
			GetGLName(program),
			0
		);
	}

	/// Use the specified @p stages of the @p program
	/**
	 *  @glsymbols
	 *  @glfunref{UseProgramStages}
	 */
	void UseStages(
		Bitfield<ProgramPipelineStage> stages,
		ProgramName program
	) const
	{
		OGLPLUS_GLFUNC(UseProgramStages)(
			_obj_name(),
			GLbitfield(stages),
			GetGLName(program)
		);
		OGLPLUS_CHECK(
			UseProgramStages,
			ObjectError,
			Object(*this)
		);
	}

#if defined GL_ALL_SHADER_BITS
	/// Use all stages of the @p program
	/**
	 *  @glsymbols
	 *  @glfunref{UseProgramStages}
	 */
	void UseAllStages(ProgramName program) const
	{
		OGLPLUS_GLFUNC(UseProgramStages)(
			_obj_name(),
			GL_ALL_SHADER_BITS,
			GetGLName(program)
		);
		OGLPLUS_VERIFY(
			UseProgramStages,
			ObjectError,
			Object(*this)
		);
	}
#endif

	/// Returns the validation process output
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramPipeline}
	 *  @glfunref{GetProgramPipelineInfoLog}
	 */
	String GetInfoLog(void) const;

	/// Returns true if the pipeline is validated, false otherwise
	/**
	 *  @see Validate
	 *
	 *  @glsymbols
	 *  @glfunref{GetProgramPipeline}
	 */
	Boolean IsValid(void) const
	{
		return Boolean(
			GetIntParam(GL_VALIDATE_STATUS),
			std::nothrow
		);
	}

	/// Validates this program pipeline
	/**
	 *  @throws Error ValidationError
	 *  @see Link
	 *
	 *  @glsymbols
	 *  @glfunref{ValidateProgramPipeline}
	 */
	ObjectOps& Validate(void);

	Outcome<ObjectOps&> Validate(std::nothrow_t);

	/// Make the @p program active for this program pipeline
	/**
	 *  @glsymbols
	 *  @glfunref{ActiveShaderProgram}
	 */
	void ActiveShaderProgram(ProgramName program) const
	{
		OGLPLUS_GLFUNC(ActiveShaderProgram)(
			_obj_name(),
			GetGLName(program)
		);
		OGLPLUS_CHECK(
			ActiveShaderProgram,
			ObjectError,
			Object(*this)
		);
	}

	/// Returns the current active shader program
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramPipeline}
	 *  @gldefref{ACTIVE_PROGRAM}
	 */
	ProgramName ActiveShaderProgram(void) const
	{
		return ProgramName(GetUIntParam(GL_ACTIVE_PROGRAM));
	}

	/// Returns true if this pipeline contains a shader of a particular type
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramPipeline}
	 */
	bool HasShader(ShaderType shader_type) const
	{
		return GetIntParam(GLenum(shader_type)) == GL_TRUE;
	}

	/// Returns the program from which the @p shader_type is used
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramPipeline}
	 */
	ProgramName ShaderProgram(ShaderType shader_type) const
	{
		return ProgramName(GetUIntParam(GLenum(shader_type)));
	}
};

/// Program pipeline operations with direct state access
typedef ObjectOps<tag::DirectState, tag::ProgramPipeline>
	ProgramPipelineOps;

/// Class that can be used to unbind the currently bound program pipeline
/**
 *  @ingroup oglplus_objects
 */
typedef ObjectZero<ObjZeroOps<tag::DirectState, tag::ProgramPipeline>>
	NoProgramPipeline;

/// An @ref oglplus_object encapsulating the OpenGL program pipeline functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<ProgramPipelineOps> ProgramPipeline;

#endif // program pipeline

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/program_pipeline.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
