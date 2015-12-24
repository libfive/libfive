/**
 *  @file oglplus/uniform.hpp
 *  @brief Uniform wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_UNIFORM_1107121519_HPP
#define OGLPLUS_UNIFORM_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/varpara_fns.hpp>
#include <oglplus/prog_var/set_ops.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <type_traits>

namespace oglplus {

template <>
class ProgVarLocOps<tag::Uniform>
{
private:
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
public:
	/// Finds the uniform location, throws on failure if active_only
	/** Finds the location of the uniform variable specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such uniform exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetUniformLocation}
	 */
	static GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	)
	{
		GLint result = OGLPLUS_GLFUNC(GetUniformLocation)(
			GetGLName(program),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetUniformLocation,
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

// collection of Uniform setter functions for basic types
template <>
class ProgVarSetters<tag::ImplicitSel, tag::Uniform, tag::NativeTypes>
{
protected:
	OGLPLUS_ERROR_CONTEXT(Uniform, Uniform)

	OGLPLUS_AUX_VARPARA_FNS(Uniform, ui, t, GLuint)
	OGLPLUS_AUX_VARPARA_FNS(Uniform, i, t, GLint)
#if GL_ARB_bindless_texture
	OGLPLUS_AUX_VARPARA_FNC(UniformHandle, ui64ARB, t, GLuint64, 1)
#elif GL_NV_shader_buffer_load
	OGLPLUS_AUX_VARPARA_FNC(Uniform, ui64NV, t, GLuint64EXT, 1)
#endif
	OGLPLUS_AUX_VARPARA_FNS(Uniform, f, t, GLfloat)
#if GL_VERSION_3_3 || GL_ARB_gpu_shader_fp64
	OGLPLUS_AUX_VARPARA_FNS(Uniform, d, t, GLdouble)
#endif

	OGLPLUS_AUX_VARPARA_FNS(Uniform, iv, v, GLint)
#if GL_ARB_bindless_texture
	OGLPLUS_AUX_VARPARA_FNC(UniformHandle, ui64vARB, v, GLuint64, 1)
#endif
	OGLPLUS_AUX_VARPARA_FNS(Uniform, fv, v, GLfloat)
#if GL_VERSION_3_3 || GL_ARB_gpu_shader_fp64
	OGLPLUS_AUX_VARPARA_FNS(Uniform, dv, v, GLdouble)
#endif
};

// collection of Uniform setter function for matrices
template <>
class ProgVarSetters<tag::ImplicitSel, tag::Uniform, tag::MatrixTypes>
{
protected:
	OGLPLUS_ERROR_CONTEXT(UniformMatrix, Uniform)

	OGLPLUS_AUX_VARPARA_MAT_FNS(UniformMatrix, fv, v, GLfloat)
#if GL_VERSION_3_3 || GL_ARB_gpu_shader_fp64
	OGLPLUS_AUX_VARPARA_MAT_FNS(UniformMatrix, dv, v, GLdouble)
#endif
};

/// Encapsulates uniform value setting functionality
/**
 *  @ingroup shader_variables
 */
template <typename OpsTag, typename T>
class ProgVarGetSetOps<OpsTag, tag::Uniform, T>
 : public ProgVarCommonOps<tag::Uniform>
 , public ProgVarBaseSetOps<OpsTag, tag::Uniform, tag::NativeTypes, T, 16>
{
protected:
	ProgVarGetSetOps(UniformLoc uloc)
	 : ProgVarCommonOps<tag::Uniform>(uloc)
	{ }
public:
	/// Set the value of the uniform
	/**
	 *  @glsymbols
	 *  @glfunref{Uniform}
	 *  @glfunref{ProgramUniform}
	 */
	void SetValue(T value)
	{
		this->_do_set(
			this->_program,
			this->_location,
			value
		);
	}

	/// Set multiple consecutive values
	void SetValues(std::size_t n, const T* values)
	{
		this->template _do_set_many<1>(
			this->_program,
			this->_location,
			GLsizei(n),
			values
		);
	}
};

template <typename OpsTag, typename T, std::size_t N>
class ProgVarGetSetOps<OpsTag, tag::Uniform, Vector<T, N>>
 : public ProgVarCommonOps<tag::Uniform>
 , public ProgVarBaseSetOps<OpsTag, tag::Uniform, tag::NativeTypes, T, 4>
{
protected:
	ProgVarGetSetOps(UniformLoc uloc)
	 : ProgVarCommonOps<tag::Uniform>(uloc)
	{ }
public:
	void SetValue(const Vector<T, N>& value)
	{
		this->template _do_set<N>(_program, _location, Data(value));
	}

	void SetValues(std::size_t n, const T* values)
	{
		assert(n % N == 0);
		this->template _do_set_many<N>(
			this->_program,
			this->_location,
			GLsizei(n / N),
			values
		);
	}

	void SetValues(std::size_t n, const Vector<T, N>* values, std::true_type)
	{
		const T* temp = reinterpret_cast<const T*>(values);
		SetValues(n*N, temp);
	}

	void SetValues(std::size_t n, const Vector<T, N>* values,std::false_type)
	{
		std::vector<T> temp;
		temp.reserve(n*N);
		for(std::size_t i=0; i!=n; ++i)
		{
			temp.insert(temp.end(), Data(values), Data(values)+N);
		}
		SetValues(temp.size(), temp.data());
	}

	void SetValues(std::size_t n, const Vector<T, N>* values)
	{
		SetValues(
			n, values,
			std::integral_constant<
				bool,
				sizeof(Vector<T, N>[4]) == sizeof(T[N*4])
			>()
		);
	}
};

template <typename OpsTag, typename T, std::size_t R, std::size_t C>
class ProgVarGetSetOps<OpsTag, tag::Uniform, Matrix<T, R, C>>
 : public ProgVarCommonOps<tag::Uniform>
 , public ProgVarBaseSetOps<OpsTag, tag::Uniform, tag::MatrixTypes, T, 16>
{
protected:
	ProgVarGetSetOps(UniformLoc uloc)
	 : ProgVarCommonOps<tag::Uniform>(uloc)
	{ }
public:
	void SetValue(const Matrix<T, R, C>& value)
	{
		this->template _do_set_mat<C, R>(
			this->_program,
			this->_location,
			1,
			true,
			Data(value)
		);
	}

	void SetValues(std::size_t n, bool row_major, const T* values)
	{
		assert(n % R*C == 0);
		this->template _do_set_mat<C, R>(
			this->_program,
			this->_location,
			GLsizei(n/(R*C)),
			row_major,
			values
		);
	}

	void SetValues(
		std::size_t n,
		const Matrix<T, R, C>* values,
		std::true_type
	)
	{
		const T* temp = reinterpret_cast<const T*>(values);
		SetValues(n*R*C, true, temp);
	}

	void SetValues(
		std::size_t n,
		const Matrix<T, R, C>* values,
		std::false_type
	)
	{
		std::vector<T> temp;
		temp.reserve(n*R*C);
		for(std::size_t i=0; i!=n; ++i)
		{
			temp.insert(temp.end(), Data(values), Data(values)+R*C);
		}
		SetValues(temp.size(), temp.data());
	}

	void SetValues(std::size_t n, const Matrix<T, R, C>* values)
	{
		SetValues(
			n, values,
			std::integral_constant<
				bool,
				sizeof(Matrix<T, R, C>[4]) == sizeof(T[R*C*4])
			>()
		);
	}
};

OGLPLUS_DECLARE_PROG_VAR(
	Uniform,
	tag::ImplicitSel,
	tag::Uniform,
	tag::NoTypecheck
)

/// Uniform sampler
typedef Uniform<GLint> UniformSampler;

// typeless uniform
template <typename OpsTag>
class ProgVar<OpsTag, tag::Uniform, tag::NoTypecheck, void>
 : public ProgVarCommonOps<tag::Uniform>
{
private:
	typedef ProgVarCommonOps<tag::Uniform> Base;
public:
	ProgVar(ProgramName program, GLuint location)
	 : Base(UniformLoc(program, GLint(location)))
	{ }

	ProgVar(ProgramName program, StrCRef identifier)
	 : Base(UniformLoc(program, identifier))
	{ }

	template <typename T>
	void Set(T value)
	{
		ProgVar<OpsTag, tag::Uniform, tag::NoTypecheck, T>(*this)
			.Set(value);
	}

	template <typename T>
	void SetValue(T value)
	{
		ProgVar<OpsTag, tag::Uniform, tag::NoTypecheck, T>(*this)
			.SetValue(value);
	}

	template <typename T>
	void SetValues(std::size_t n, T values)
	{
		ProgVar<OpsTag, tag::Uniform, tag::NoTypecheck, T>(*this)
			.SetValues(n, values);
	}

	template <typename T>
	ProgVar& operator = (T value)
	{
		Set(value);
		return *this;
	}
};

typedef ProgVar<tag::ImplicitSel, tag::Uniform, tag::NoTypecheck, void> UntypedUniform;

inline UntypedUniform
operator / (ProgramName program, StrCRef identifier)
{
	return UntypedUniform(program, identifier);
}

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/uniform.ipp>
#endif

#endif // include guard
