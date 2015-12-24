/**
 *  @file oglplus/shader.hpp
 *  @brief Shader wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHADER_1107121519_HPP
#define OGLPLUS_SHADER_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/object/array.hpp>
#include <oglplus/error/program.hpp>
#include <oglplus/error/outcome.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/precision_type.hpp>
#include <oglplus/shader_type.hpp>
#include <oglplus/glsl_source.hpp>
#include <oglplus/size_type.hpp>

#include <array>
#include <vector>
#include <cassert>

namespace oglplus {

/// Class wrapping shader construction/destruction functions
/** @note Do not use this class directly, use Shader instead.
 *
 *  @glsymbols
 *  @glfunref{CreateShader}
 *  @glfunref{DeleteShader}
 *  @glfunref{IsShader}
 */
template <>
class ObjGenDelOps<tag::Shader>
{
protected:
	static void Gen(tag::Create, GLsizei count, GLuint* names, GLenum type)
	{
		assert(names != nullptr);
		for(GLsizei i=0; i<count; ++i)
		{
			names[i] = OGLPLUS_GLFUNC(CreateShader)(type);
			OGLPLUS_CHECK_SIMPLE(CreateShader);
		}
	}

	GLenum _type;

	void Gen(tag::Create create, GLsizei count, GLuint* names) const
	{
		Gen(create, count, names, _type);
	}

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		for(GLsizei i=0; i<count; ++i)
		{
			OGLPLUS_GLFUNC(DeleteShader)(names[i]);
			OGLPLUS_VERIFY_SIMPLE(DeleteShader);
		}
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsShader)(name),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsShader);
		return result;
	}
};

template <>
struct ObjGenTag<tag::DirectState, tag::Shader>
{
	typedef tag::Create Type;
};

/// Common shader operations
/** @note Do not use this class directly, use Shader instead.
 */
template <>
class ObjCommonOps<tag::Shader>
 : public ShaderName
{
protected:
	ObjCommonOps(ShaderName name)
	OGLPLUS_NOEXCEPT(true)
	 : ShaderName(name)
	{ }

public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjCommonOps(ObjCommonOps&&) = default;
	ObjCommonOps(const ObjCommonOps&) = default;
	ObjCommonOps& operator = (ObjCommonOps&&) = default;
	ObjCommonOps& operator = (const ObjCommonOps&) = default;
#else
	typedef ShaderName _base;

	ObjCommonOps(ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	{ }

	ObjCommonOps(const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	{ }

	ObjCommonOps& operator = (ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		return *this;
	}

	ObjCommonOps& operator = (const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		return *this;
	}
#endif
#if OGLPLUS_DOCUMENTATION_ONLY || \
	GL_ES_VERSION_3_0 || \
	GL_VERSION_4_1 || \
	GL_ARB_ES2_compatibility
	/// Get the shader precision format
	/**
	 *  @glvoereq{4,1,ARB,ES2_compatibility}
	 *  @glsymbols
	 *  @glfunref{GetShaderPrecisionFormat}
	 */
	static void PrecisionFormat(
		ShaderType shader_type,
		PrecisionType precision_type,
		GLint* range_log_2,
		GLint* precision_log_2
	)
	{
		OGLPLUS_GLFUNC(GetShaderPrecisionFormat)(
			GLenum(shader_type),
			GLenum(precision_type),
			range_log_2,
			precision_log_2
		);
		OGLPLUS_VERIFY_SIMPLE(GetShaderPrecisionFormat);
	}
#endif
};

/// Class wrapping shader functions (with direct state access)
/** @note Do not use this class directly, use Shader instead.
 */
template <>
class ObjectOps<tag::DirectState, tag::Shader>
 : public ObjZeroOps<tag::DirectState, tag::Shader>
{
protected:
	ObjectOps(ShaderName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::Shader>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::Shader> _base;

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

	/// Types related to Shader
	struct Property
	{
		/// The type of a Shader
		typedef ShaderType Type;
	};

	/// Get the type of the shader
	/**
	 *  @glsymbols
	 *  @glfunref{GetShader}
	 *  @gldefref{SHADER_TYPE}
	 */
	ShaderType Type(void) const
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetShaderiv)(
			_obj_name(),
			GL_SHADER_TYPE,
			&result
		);
		OGLPLUS_VERIFY(
			GetShaderiv,
			ObjectError,
			Object(*this)
		);
		return ShaderType(result);
	}

	/// Set the source code of the shader
	/**
	 *  @glsymbols
	 *  @glfunref{ShaderSource}
	 */
	ObjectOps& Source(
		const SizeType count,
		const GLchar* const * srcs,
		const GLint* lens
	)
	{
		OGLPLUS_GLFUNC(ShaderSource)(
			_obj_name(),
			count,
			const_cast<const GLchar**>(srcs),
			lens
		);
		OGLPLUS_VERIFY(
			ShaderSource,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	template <typename Src>
	ObjectOps& SourceTpl(const Src& src)
	{
		return Source(src.Count(), src.Parts(), src.Lengths());
	}

	/// Set the source code of the shader
	/**
	 *  @glsymbols
	 *  @glfunref{ShaderSource}
	 */
	ObjectOps& Source(GLSLString&& source)
	{
		return SourceTpl(source);
	}

	/// Set the source code of the shader
	/**
	 *  @glsymbols
	 *  @glfunref{ShaderSource}
	 */
	ObjectOps& Source(GLSLStrings&& source)
	{
		return SourceTpl(source);
	}

	/// Set the source code of the shader
	/**
	 *  @glsymbols
	 *  @glfunref{ShaderSource}
	 */
	ObjectOps& Source(const GLSLSource& glsl_source)
	{
		return SourceTpl(glsl_source);
	}

	/// Returns true if the shader is already compiled, returns false otherwise
	/**
	 *  @see Compile
	 *
	 *  @glsymbols
	 *  @glfunref{GetShader}
	 *  @gldefref{COMPILE_STATUS}
	 */
	Boolean IsCompiled(void) const
	{
		Boolean status;
		OGLPLUS_GLFUNC(GetShaderiv)(
			_obj_name(),
			GL_COMPILE_STATUS,
			status._ptr()
		);
		OGLPLUS_VERIFY(
			GetShaderiv,
			ObjectError,
			Object(*this).
			EnumParam(Type())
		);
		return status;
	}

	/// Returns the compiler output if the program is compiled
	/**
	 *  @see IsCompiled
	 *  @see Compile
	 *
	 *  @glsymbols
	 *  @glfunref{GetShader}
	 *  @glfunref{GetShaderInfoLog}
	 */
	String GetInfoLog(void) const;

	/// Compiles the shader
	/**
	 *  @post IsCompiled()
	 *  @throws Error CompileError
	 *  @see IsCompiled
	 *
	 *  @glsymbols
	 *  @glfunref{CompileShader}
	 */
	ObjectOps& Compile(void);

	Outcome<ObjectOps&> Compile(std::nothrow_t);

#if OGLPLUS_DOCUMENTATION_ONLY || \
	GL_ARB_shading_language_include

	/// Compiles the shader using the specified include paths
	/**
	 *  @post IsCompiled()
	 *  @throws Error CompileError
	 *  @see IsCompiled
	 *
	 *  @glverreq{ARB,shading_language_include}
	 *  @glsymbols
	 *  @glfunref{CompileShaderIncludeARB}
	 */
	ObjectOps& CompileInclude(
		const SizeType count,
		const GLchar* const* paths,
		const GLint* lengths
	);

	Outcome<ObjectOps&> CompileInclude(
		const SizeType count,
		const GLchar* const* paths,
		const GLint* lengths,
		std::nothrow_t
	);

	/// Compiles the shader using the specified include paths
	/**
	 *  @post IsCompiled()
	 *  @throws Error CompileError
	 *  @see IsCompiled
	 *
	 *  @glverreq{ARB,shading_language_include}
	 *  @glsymbols
	 *  @glfunref{CompileShaderIncludeARB}
	 */
	ObjectOps& CompileInclude(GLSLString&& incl)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths()
		);
	}

	Outcome<ObjectOps&> CompileInclude(GLSLString&& incl, std::nothrow_t)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths(),
			std::nothrow
		);
	}

	ObjectOps& CompileInclude(GLSLStrings&& incl)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths()
		);
	}

	Outcome<ObjectOps&> CompileInclude(GLSLStrings&& incl, std::nothrow_t)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths(),
			std::nothrow
		);
	}

	ObjectOps& CompileInclude(const GLSLSource& incl)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths()
		);
	}

	Outcome<ObjectOps&> CompileInclude(const GLSLSource& incl, std::nothrow_t)
	{
		return CompileInclude(
			incl.Count(),
			incl.Parts(),
			incl.Lengths(),
			std::nothrow
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || \
	GL_ES_VERSION_3_0 || \
	GL_VERSION_4_1 || \
	GL_ARB_ES2_compatibility
	/// Indicate that the resources associated with the compiler can be freed
	/**
	 *  @glvoereq{4,1,ARB,ES2_compatibility}
	 *  @glsymbols
	 *  @glfunref{ReleaseShaderCompiler}
	 */
	static void ReleaseCompiler(void)
	{
		OGLPLUS_GLFUNC(ReleaseShaderCompiler)();
		OGLPLUS_VERIFY_SIMPLE(ReleaseShaderCompiler);
	}
#endif
};

/// Shader operations (with direct state access)
typedef ObjectOps<tag::DirectState, tag::Shader>
	ShaderOps;

template <>
struct ObjectSubtype<tag::Shader>
{
	typedef ShaderType Type;
};

/// An object encasulating the shader object functionality
/**
 *  @see Program
 *  @see VertexShader
 *  @see GeometryShader
 *  @see FragmentShader
 *  @see TessControlShader
 *  @see TessEvaluationShader
 *  @see ComputeShader
 */
class Shader
 : public Object<ShaderOps>
{
private:
	Shader(const Shader&); // = delete;
protected:
	using Object<ShaderOps>::Uninitialized_;

	/// Uninitialized construction
	Shader(Uninitialized_ u)
	 : Object<ShaderOps>(u)
	{ }
public:
	/// Construction with shader @p type specifier
	Shader(ShaderType type)
	 : Object<ShaderOps>(type)
	{ }

	/// Construction with type specifier and textual descriptor
	Shader(ShaderType type, ObjectDesc&& description)
	 : Object<ShaderOps>(type, std::move(description))
	{ }

	/// Construction with type and source code wrapper
	Shader(
		ShaderType type,
		GLSLString&& glsl_source
	): Object<ShaderOps>(type)
	{
		this->Source(std::move(glsl_source));
		this->Compile();
	}

	/// Construction with type, description and source code wrapper
	Shader(
		ShaderType type,
		ObjectDesc&& description,
		GLSLString&& glsl_source
	): Object<ShaderOps>(type, std::move(description))
	{
		this->Source(std::move(glsl_source));
		this->Compile();
	}

	/// Construction with type and source code wrapper
	Shader(
		ShaderType type,
		GLSLStrings&& glsl_source
	): Object<ShaderOps>(type)
	{
		this->Source(std::move(glsl_source));
		this->Compile();
	}

	/// Construction with type, description and source code wrapper
	Shader(
		ShaderType type,
		ObjectDesc&& description,
		GLSLStrings&& glsl_source
	): Object<ShaderOps>(type, std::move(description))
	{
		this->Source(std::move(glsl_source));
		this->Compile();
	}

	/// Construction with type and source code wrapper
	Shader(
		ShaderType type,
		const GLSLSource& glsl_source
	): Object<ShaderOps>(type)
	{
		this->Source(glsl_source);
		this->Compile();
	}

	/// Construction with type, description and source code wrapper
	Shader(
		ShaderType type,
		ObjectDesc&& description,
		const GLSLSource& glsl_source
	): Object<ShaderOps>(type, std::move(description))
	{
		this->Source(glsl_source);
		this->Compile();
	}

	/// Shaders are movable
	Shader(Shader&& temp)
	 : Object<ShaderOps>(static_cast<Object<ShaderOps>&&>(temp))
	{ }

	Shader& operator = (Shader&& temp)
	{
		Object<ShaderOps>::operator = (
			static_cast<Object<ShaderOps>&&>(temp)
		);
		return *this;
	}
};

template <>
struct Classify<Shader>
 : Classify<Object<ShaderOps>>
{ };

template <>
class Array<Shader>
 : public Array<ObjectOps<tag::DirectState, tag::Shader>>
{
public:
	Array(BigSizeType n, ShaderType type)
	 : Array<ObjectOps<tag::DirectState, tag::Shader>>(n, type)
	{ }
};

/// Base template for specialized shader types
template <ShaderType ShType>
class SpecShader
 : public Shader
{
private:
	SpecShader(const SpecShader&); // = delete;
public:
	/// Default construction
	SpecShader(void)
	 : Shader(ShType)
	{ }

	/// Construction with a textual descriptor
	SpecShader(ObjectDesc&& description)
	 : Shader(ShType, std::move(description))
	{ }

	/// Construction with a source code wrapper
	SpecShader(GLSLString&& glsl_source)
	 : Shader(ShType, std::move(glsl_source))
	{ }

	/// Construction with description and source code wrapper
	SpecShader(
		ObjectDesc&& description,
		GLSLString&& glsl_source
	): Shader(ShType, std::move(description), std::move(glsl_source))
	{ }

	/// Construction with a source code wrapper
	SpecShader(GLSLStrings&& glsl_source)
	 : Shader(ShType, std::move(glsl_source))
	{ }

	/// Construction with description and source code wrapper
	SpecShader(
		ObjectDesc&& description,
		GLSLStrings&& glsl_source
	): Shader(ShType, std::move(description), std::move(glsl_source))
	{ }

	/// Construction with a source code wrapper
	SpecShader(const GLSLSource& glsl_source)
	 : Shader(ShType, glsl_source)
	{ }

	/// Construction with description and source code wrapper
	SpecShader(
		ObjectDesc&& description,
		const GLSLSource& glsl_source
	): Shader(ShType, std::move(description), glsl_source)
	{ }

	SpecShader(SpecShader&& temp)
	 : Shader(static_cast<Shader&&>(temp))
	{ }

	SpecShader& operator = (SpecShader&& temp)
	{
		Shader::operator = (static_cast<Shader&&>(temp));
		return *this;
	}
};

/// Vertex shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::Vertex> VertexShader;

#if OGLPLUS_DOCUMENTATION_ONLY || GL_GEOMETRY_SHADER
/// Geometry shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::Geometry> GeometryShader;
#endif

/// Fragment shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::Fragment> FragmentShader;

#if OGLPLUS_DOCUMENTATION_ONLY || GL_TESS_CONTROL_SHADER
/// Tesselation control shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::TessControl> TessControlShader;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_TESS_EVALUATION_SHADER
/// Tesselation evaluation shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::TessEvaluation> TessEvaluationShader;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_COMPUTE_SHADER
/// Compute shader wrapper
/**
 *  @see Shader
 *  @see Program
 *  @ingroup oglplus_objects
 */
typedef SpecShader<ShaderType::Compute> ComputeShader;
#endif

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shader.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
