/**
 *  @file oglplus/context/stencil_test.hpp
 *  @brief Wrappers for stencil tests and operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_STENCIL_TEST_1201040722_HPP
#define OGLPLUS_CONTEXT_STENCIL_TEST_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/compare_function.hpp>
#include <oglplus/stencil_operation.hpp>
#include <oglplus/face_mode.hpp>

namespace oglplus {
namespace context {

struct StencilFuncArgs
{
	GLenum _func;
	GLint  _refv;
	GLuint _mask;

	StencilFuncArgs(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	StencilFuncArgs(
		CompareFunction func,
		GLint refv = GLint(0),
		GLuint mask = ~GLuint(0)
	) OGLPLUS_NOEXCEPT(true)
	 : _func(GLenum(func))
	 , _refv(refv)
	 , _mask(mask)
	{ }

	CompareFunction Func(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return CompareFunction(_func);
	}

	GLint Ref(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _refv;
	}

	GLuint ValueMask(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _mask;
	}

	friend
	bool operator == (const StencilFuncArgs& a, const StencilFuncArgs& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return	(a._func == b._func) &&
			(a._refv == b._refv) &&
			(a._mask == b._mask);
	}

	friend
	bool operator != (const StencilFuncArgs& a, const StencilFuncArgs& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return	(a._func != b._func) ||
			(a._refv != b._refv) ||
			(a._mask != b._mask);
	}
};

struct StencilOperations
{
	StencilOperation _sfail;
	StencilOperation _dfail;
	StencilOperation _dpass;

	StencilOperations(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	StencilOperations(
		StencilOperation sfail,
		StencilOperation dfail,
		StencilOperation dpass
	) OGLPLUS_NOEXCEPT(true)
	 : _sfail(sfail)
	 , _dfail(dfail)
	 , _dpass(dpass)
	{ }

	StencilOperation StencilFail(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _sfail;
	}

	StencilOperation DepthFail(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _dfail;
	}

	StencilOperation DepthPass(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _dpass;
	}

	friend
	bool operator == (const StencilOperations& a, const StencilOperations& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return	(a._sfail == b._sfail) &&
			(a._dfail == b._dfail) &&
			(a._dpass == b._dpass);
	}

	friend
	bool operator != (const StencilOperations& a, const StencilOperations& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return	(a._sfail != b._sfail) ||
			(a._dfail != b._dfail) ||
			(a._dpass != b._dpass);
	}
};

/// Wrapper for the stencil-buffer-related operations
/**
 *  @ingroup ogl_context
 */
class StencilTest
{
public:
	/// Sets the stencil function
	/**
	 *  @glsymbols
	 *  @glfunref{StencilFunc}
	 */
	static void StencilFunc(
		CompareFunction func,
		GLint ref = GLint(0),
		GLuint mask = ~GLuint(0)
	)
	{
		OGLPLUS_GLFUNC(StencilFunc)(GLenum(func), ref, mask);
		OGLPLUS_VERIFY(
			StencilFunc,
			Error,
			EnumParam(func)
		);
	}

	static void StencilFunc(const StencilFuncArgs& fa)
	{
		return StencilFunc(fa.Func(), fa.Ref(), fa.ValueMask());
	}

	/// Sets the stencil function separately for front and back faces
	/**
	 *  @glsymbols
	 *  @glfunref{StencilFuncSeparate}
	 */
	static void StencilFuncSeparate(
		Face face,
		CompareFunction func,
		GLint ref = GLint(0),
		GLuint mask = ~GLuint(0)
	)
	{
		OGLPLUS_GLFUNC(StencilFuncSeparate)(
			GLenum(face),
			GLenum(func),
			ref,
			mask
		);
		OGLPLUS_VERIFY(
			StencilFuncSeparate,
			Error,
			EnumParam(func)
		);
	}

	static void StencilFuncSeparate(
		Face face,
		const StencilFuncArgs& fa
	)
	{
		return StencilFuncSeparate(
			face,
			fa.Func(),
			fa.Ref(),
			fa.ValueMask()
		);
	}

	static void StencilFuncSeparateSingle(
		SingleFace face,
		CompareFunction func,
		GLint ref = GLint(0),
		GLuint mask = ~GLuint(0)
	)
	{
		OGLPLUS_GLFUNC(StencilFuncSeparate)(
			GLenum(face),
			GLenum(func),
			ref,
			mask
		);
		OGLPLUS_VERIFY(
			StencilFuncSeparate,
			Error,
			EnumParam(func)
		);
	}

	static void StencilFuncSeparateSingle(
		SingleFace face,
		const StencilFuncArgs& fa
	)
	{
		return StencilFuncSeparateSingle(
			face,
			fa.Func(),
			fa.Ref(),
			fa.ValueMask()
		);
	}

	/// Returns the stencil function
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_FUNC}
	 *  @gldefref{STENCIL_BACK_FUNC}
	 */
	static CompareFunction StencilFunc(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_FUNC:
			GL_STENCIL_FUNC,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return CompareFunction(result);
	}

	static CompareFunction StencilFunc(Face face)
	{
		return StencilFunc(face == Face::Back);
	}

	/// Returns the stencil reference value
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_REF}
	 *  @gldefref{STENCIL_BACK_REF}
	 */
	static GLint StencilRef(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_REF:
			GL_STENCIL_REF,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	static GLint StencilRef(Face face)
	{
		return StencilRef(face == Face::Back);
	}

	/// Returns the value of stencil mask
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_VALUE_MASK}
	 *  @gldefref{STENCIL_BACK_VALUE_MASK}
	 */
	static GLuint StencilValueMask(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_VALUE_MASK:
			GL_STENCIL_VALUE_MASK,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return GLuint(result);
	}

	static GLuint StencilValueMask(Face face)
	{
		return StencilValueMask(face == Face::Back);
	}

	static
	oglplus::context::StencilFuncArgs StencilFuncArgs(bool backface = false)
	{
		return oglplus::context::StencilFuncArgs(
			StencilFunc(backface),
			StencilRef(backface),
			StencilValueMask(backface)
		);
	}

	static
	oglplus::context::StencilFuncArgs StencilFuncArgs(Face face)
	{
		return StencilFuncArgs(face == Face::Back);
	}

	static
	oglplus::context::StencilFuncArgs StencilFuncArgsSingle(SingleFace face)
	{
		return StencilFuncArgs(face == SingleFace::Back);
	}

	/// Sets the stencil operation
	/**
	 *  @glsymbols
	 *  @glfunref{StencilOp}
	 */
	static void StencilOp(
		StencilOperation sfail,
		StencilOperation dfail,
		StencilOperation dpass
	)
	{
		OGLPLUS_GLFUNC(StencilOp)(
			GLenum(sfail),
			GLenum(dfail),
			GLenum(dpass)
		);
		OGLPLUS_VERIFY_SIMPLE(StencilOp);
	}

	static void StencilOp(const StencilOperations& ops)
	{
		StencilOp(ops._sfail, ops._dfail, ops._dpass);
	}

	/// Sets the stencil operation separately for front and back faces
	/**
	 *  @glsymbols
	 *  @glfunref{StencilOpSeparate}
	 */
	static void StencilOpSeparate(
		Face face,
		StencilOperation sfail,
		StencilOperation dfail,
		StencilOperation dpass
	)
	{
		OGLPLUS_GLFUNC(StencilOpSeparate)(
			GLenum(face),
			GLenum(sfail),
			GLenum(dfail),
			GLenum(dpass)
		);
		OGLPLUS_VERIFY_SIMPLE(StencilOpSeparate);
	}

	static void StencilOpSeparateSingle(
		SingleFace face,
		StencilOperation sfail,
		StencilOperation dfail,
		StencilOperation dpass
	)
	{
		OGLPLUS_GLFUNC(StencilOpSeparate)(
			GLenum(face),
			GLenum(sfail),
			GLenum(dfail),
			GLenum(dpass)
		);
		OGLPLUS_VERIFY_SIMPLE(StencilOpSeparate);
	}

	static void StencilOpSeparate(
		Face face,
		const StencilOperations& ops
	)
	{
		StencilOpSeparate(face, ops._sfail, ops._dfail, ops._dpass);
	}

	static void StencilOpSeparateSingle(
		SingleFace face,
		const StencilOperations& ops
	)
	{
		StencilOpSeparateSingle(
			face,
			ops._sfail,
			ops._dfail,
			ops._dpass
		);
	}

	/// Returns the stencil-fail action
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_FAIL}
	 *  @gldefref{STENCIL_BACK_FAIL}
	 */
	static StencilOperation StencilFail(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_FAIL:
			GL_STENCIL_FAIL,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return StencilOperation(result);
	}

	static StencilOperation StencilFail(Face face)
	{
		return StencilFail(face == Face::Back);
	}

	/// Returns the stencil-pass depth-fail action
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_PASS_DEPTH_FAIL}
	 *  @gldefref{STENCIL_BACK_PASS_DEPTH_FAIL}
	 */
	static StencilOperation StencilPassDepthFail(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_PASS_DEPTH_FAIL:
			GL_STENCIL_PASS_DEPTH_FAIL,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return StencilOperation(result);
	}

	static StencilOperation StencilPassDepthFail(Face face)
	{
		return StencilPassDepthFail(face == Face::Back);
	}

	/// Returns the stencil-pass depth-pass action
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_PASS_DEPTH_PASS}
	 *  @gldefref{STENCIL_BACK_PASS_DEPTH_PASS}
	 */
	static StencilOperation StencilPassDepthPass(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_PASS_DEPTH_PASS:
			GL_STENCIL_PASS_DEPTH_PASS,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return StencilOperation(result);
	}

	static StencilOperation StencilPassDepthPass(Face face)
	{
		return StencilPassDepthPass(face == Face::Back);
	}

	static StencilOperations StencilOps(bool backface = false)
	{
		return StencilOperations(
			StencilFail(backface),
			StencilPassDepthFail(backface),
			StencilPassDepthPass(backface)
		);
	}

	static StencilOperations StencilOps(Face face)
	{
		return StencilOps(face == Face::Back);
	}

	static StencilOperations StencilOpsSingle(SingleFace face)
	{
		return StencilOps(face == SingleFace::Back);
	}

};

} // namespace context
} // namespace oglplus

#endif // include guard
