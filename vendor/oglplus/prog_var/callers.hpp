/**
 *  .file oglplus/prog_var/callers.hpp
 *  .brief Program variable (uniform / vertex-attribute) helpers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_CALLERS_1107121519_HPP
#define OGLPLUS_PROG_VAR_CALLERS_1107121519_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/error/basic.hpp>
#include <oglplus/prog_var/varpara_fns.hpp>

#include <type_traits>
#include <cstddef>

namespace oglplus {

template <typename T>
class ProgVarCallers<tag::ImplicitSel, T>
{
protected:
	template <typename LI, typename UI>
	static void _call_set_v(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, const T*),
		const T* v
	)
	{
		_fn(location, v);
	}

	template <typename LI, typename UI, typename SI>
	static void _call_set_v(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, SI, const T*),
		const T* v
	)
	{
		_fn(location, 1, v);
	}

	template <typename LI, typename UI, typename SI>
	static void _call_set_vn(
		GLuint /*program*/,
		LI location,
		GLsizei n,
		void(GLAPIENTRY *_fn)(UI, SI, const T*),
		const T* v
	)
	{
		_fn(location, n, v);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, P),
		P v0
	)
	{
		_fn(location, v0);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, P, P),
		P v0, P v1
	)
	{
		_fn(location, v0, v1);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, P, P, P),
		P v0, P v1, P v2
	)
	{
		_fn(location, v0, v1, v2);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint /*program*/,
		LI location,
		void(GLAPIENTRY *_fn)(UI, P, P, P, P),
		P v0, P v1, P v2, P v3
	)
	{
		_fn(location, v0, v1, v2, v3);
	}

	template <typename LI, typename ID, typename CT, typename TP>
	static void _call_set_m(
		GLuint /*program*/,
		LI location,
		GLsizei count,
		GLboolean transpose,
		void(GLAPIENTRY *_fn)(ID, CT, TP, const T*),
		const T* v
	)
	{
		_fn(location, count, transpose, v);
	}
};

template <typename T>
class ProgVarCallers<tag::DirectState, T>
{
protected:
	template <typename LI, typename UI>
	static void _call_set_v(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, const T*),
		const T* v
	)
	{
		_fn(program, location, v);
	}

	template <typename LI, typename UI, typename SI>
	static void _call_set_v(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, SI, const T*),
		const T* v
	)
	{
		_fn(program, location, 1, v);
	}

	template <typename LI, typename UI, typename SI>
	static void _call_set_vn(
		GLuint program,
		LI location,
		GLsizei n,
		void(GLAPIENTRY *_fn)(GLuint, UI, SI, const T*),
		const T* v
	)
	{
		_fn(program, location, n, v);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, P),
		P v0
	)
	{
		_fn(program, location, v0);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, P, P),
		P v0, P v1
	)
	{
		_fn(program, location, v0, v1);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, P, P, P),
		P v0, P v1, P v2
	)
	{
		_fn(program, location, v0, v1, v2);
	}

	template <typename LI, typename UI, typename P>
	static void _call_set_t(
		GLuint program,
		LI location,
		void(GLAPIENTRY *_fn)(GLuint, UI, P, P, P, P),
		P v0, P v1, P v2, P v3
	)
	{
		_fn(program, location, v0, v1, v2, v3);
	}

	template <typename LI, typename ID, typename CT, typename TP>
	static void _call_set_m(
		GLuint program,
		LI location,
		GLsizei count,
		GLboolean transpose,
		void(GLAPIENTRY *_fn)(GLuint, ID, CT, TP, const T*),
		const T* v
	)
	{
		_fn(program, location, count, transpose, v);
	}
};

} // namespace oglplus

#endif // include guard
