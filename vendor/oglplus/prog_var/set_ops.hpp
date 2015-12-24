/**
 *  .file oglplus/prog_var/set_ops.hpp
 *  .brief Program variable (uniform / vertex-attribute) helpers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_SET_OPS_1107121519_HPP
#define OGLPLUS_PROG_VAR_SET_OPS_1107121519_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/error/basic.hpp>
#include <oglplus/prog_var/callers.hpp>

#include <type_traits>
#include <cstddef>

namespace oglplus {

template <class OpsTag, class VarTag, class TypTag, class T, std::size_t M>
class ProgVarBaseSetOps;

template <typename OpsTag, typename VarTag, typename T, std::size_t M>
class ProgVarBaseSetOps<OpsTag, VarTag, tag::NativeTypes, T, M>
 : public ProgVarSetters<OpsTag, VarTag, tag::NativeTypes>
 , public ProgVarCallers<OpsTag, T>
{
private:
	typedef ProgVarSetters<OpsTag, VarTag, tag::NativeTypes> Setters;
	typedef ProgVarCallers<OpsTag, T> Callers;

	OGLPLUS_ERROR_REUSE_CONTEXT(Setters)

	typedef std::false_type _set_done;
	typedef std::true_type  _set_cont;

	template <std::size_t N>
	static std::integral_constant<bool,  (N > 4)> _set_mode(void)
	{
		return std::integral_constant<bool, (N > 4)>();
	}

	template <std::size_t N, typename LI, typename V>
	static void _do_set_v(
		_set_cont,
		GLuint program,
		LI base_location,
		LI location,
		const V* v
	)
	{
		std::integral_constant<std::size_t, 4> nparam;
		Callers::_call_set_v(
			program,
			location,
			Setters::_fns_v(nparam, v),
			v
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(base_location)
		);
		_do_set_v<N - 4, LI, V>(
			_set_mode<N - 4>(),
			program,
			base_location,
			location+1,
			v+4
		);
	}

	template <std::size_t N, typename LI, typename V>
	static void _do_set_v(
		_set_done,
		GLuint program,
		LI base_location,
		LI location,
		const V* v
	)
	{
		std::integral_constant<std::size_t, N> nparam;
		Callers::_call_set_v(
			program,
			location,
			Setters::_fns_v(nparam, v),
			v
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(base_location)
		);
	}

	template <std::size_t N, typename LI, typename V>
	static void _do_set_n(
		_set_done,
		GLuint program,
		LI base_location,
		LI location,
		GLsizei n,
		const V* v
	)
	{
		std::integral_constant<std::size_t, N> nparam;
		Callers::_call_set_vn(
			program,
			location,
			n,
			Setters::_fns_v(nparam, v),
			v
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(base_location)
		);
	}


#if !OGLPLUS_NO_VARIADIC_TEMPLATES
	template <typename LI, typename S, typename ... V>
	static void _do_set_t(
		_set_cont,
		GLuint program,
		LI base_location,
		LI location,
		S v0, S v1, S v2, S v3,
		V ... v
	)
	{
		std::integral_constant<std::size_t, 4> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v0),
			v0, v1, v2, v3
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(base_location)
		);
		_do_set_t(
			_set_mode<sizeof...(V)>(),
			program,
			base_location,
			location+1,
			v...
		);
	}

	template <typename LI, typename ... V>
	static void _do_set_t(
		_set_done,
		GLuint program,
		LI base_location,
		LI location,
		V ... v
	)
	{
		std::integral_constant<std::size_t, sizeof...(V)> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v...),
			v...
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(base_location)
		);
	}
#endif //NO_VARIADIC_TEMPLATES

protected:

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
	template <typename LI, typename ... V>
	static void _do_set(GLuint program, LI location, V ... v)
	{
		static_assert(
			(sizeof...(V) > 0) && (sizeof...(V) <= M),
			"Set requires 1 to M arguments"
		);
		_do_set_t(
			_set_mode<sizeof...(V)>(),
			program,
			location,
			location,
			v...
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}
#else
	template <typename LI, typename V>
	static void _do_set(GLuint program, LI location, V v0)
	{
		std::integral_constant<std::size_t, 1> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v0),
			v0
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}

	template <typename LI, typename V>
	static void _do_set(GLuint program, LI location, V v0, V v1)
	{
		std::integral_constant<std::size_t, 2> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v0),
			v0, v1
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}

	template <typename LI, typename V>
	static void _do_set(GLuint program, LI location, V v0, V v1, V v2)
	{
		std::integral_constant<std::size_t, 3> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v0),
			v0, v1, v2
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}

	template <typename LI, typename V>
	static void _do_set(GLuint program, LI location, V v0, V v1, V v2, V v3)
	{
		std::integral_constant<std::size_t, 4> nparam;
		Callers::_call_set_t(
			program,
			location,
			Setters::_fns_t(nparam, &v0),
			v0, v1, v2, v3
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}
#endif //NO_VARIADIC_TEMPLATES

	template <std::size_t Cols, typename LI, typename V>
	static void _do_set(GLuint program, LI location, const V* v)
	{
		static_assert(
			(Cols > 0) && (Cols <= M),
			"The number of elements must be between 1 and M"
		);
		_do_set_v<Cols, LI, V>(
			_set_mode<Cols>(),
			program,
			location,
			location,
			v
		);
	}

	template <std::size_t Cols, typename LI, typename V>
	static void _do_set_many(GLuint prog, LI location, GLsizei n, const V*v)
	{
		static_assert(
			(Cols > 0) && (Cols <= M),
			"The number of elements must be between 1 and M"
		);
		_do_set_n<Cols, LI, V>(
			_set_mode<Cols>(),
			prog,
			location,
			location,
			n,
			v
		);
	}
};

template <typename OpsTag, typename VarTag, typename T, std::size_t M>
class ProgVarBaseSetOps<OpsTag, VarTag, tag::MatrixTypes, T, M>
 : public ProgVarSetters<OpsTag, VarTag, tag::MatrixTypes>
 , public ProgVarCallers<OpsTag, T>
{
private:
	typedef ProgVarSetters<OpsTag, VarTag, tag::MatrixTypes> Setters;
	typedef ProgVarCallers<OpsTag, T> Callers;

	OGLPLUS_ERROR_REUSE_CONTEXT(Setters)
protected:
	template <std::size_t Cols, std::size_t Rows, typename LI, typename V>
	static void _do_set_mat(
		GLuint program,
		LI location,
		GLsizei count,
		Boolean transpose,
		V* v
	)
	{
		static_assert(
			(Cols > 0) && (Cols <= 4),
			"The number of columns must be between 1 and 4"
		);
		static_assert(
			(Rows > 0) && (Rows <= 4),
			"The number of rows must be between 1 and 4"
		);
		std::integral_constant<std::size_t, Rows> rows;
		std::integral_constant<std::size_t, Cols> cols;
		Callers::_call_set_m(
			program,
			location,
			count,
			transpose._get(),
			Setters::_fns_v(cols, rows, v),
			v
		);
		OGLPLUS_CHECK_CTXT(
			ProgVarError,
			Program(ProgramName(program)).
			Index(location)
		);
	}

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
	template <std::size_t Cols, typename LI, typename V, typename ... P>
	static void _do_set_mat_p(
		GLuint program,
		LI location,
		bool transpose,
		V v,
		P ... p
	)
	{
		static_assert(
			(Cols > 0) && (Cols <= 4),
			"The number of columns must be between 1 and 4"
		);
		static_assert(
			(sizeof...(P) + 1) % Cols == 0,
			"Not enough values for the last row"
		);
		V values[] = {v, V(p)...};
		_do_set_mat<Cols, (sizeof...(P) + 1) / Cols, LI, V>(
			program,
			location,
			1,
			transpose,
			values
		);
	}
#endif //NO_VARIADIC_TEMPLATES
};

} // namespace oglplus

#endif // include guard
