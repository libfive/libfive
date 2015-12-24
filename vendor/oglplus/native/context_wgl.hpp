/**
 *  @file oglplus/native/context_wgl.hpp
 *  @brief Wrapper for native current WGL/OpenGL context
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_NATIVE_CONTEXT_1404232057_HPP
#define OGLPLUS_NATIVE_CONTEXT_1404232057_HPP
// NOTE: the include guard is intentionally without the _WGL suffix so that
// different native context implementation cannot be included simultaneously.

#include <oglplus/native/surface_wgl.hpp>

namespace oglplus {
namespace native {

/// Wrapper for a valid WGL context handle
class ContextWGL
{
private:
	::HGLRC _context;

	friend ::HGLRC GetHGLRC(const ContextWGL&);

protected:
	struct Current_ { };

	ContextWGL(Current_)
	 : _context(::wglGetCurrentContext())
	{
		if(!_context) HandleNoWGLRC();
	}
public:
	friend
	bool operator == (const ContextWGL& a, const ContextWGL& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return a._context == b._context;
	}

	friend
	bool operator != (const ContextWGL& a, const ContextWGL& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return a._context != b._context;
	}

	friend
	bool operator <  (const ContextWGL& a, const ContextWGL& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return a._context <  b._context;
	}

	/// Returns a wrapper for the currently bound WGL context
	/** This function gets and wraps the current WGL rendering context.
	 *  If no context is current it throws a @c runtime_error.
	 *
	 *  @throws std::runtime_error
	 */
	static ContextWGL Current(void)
	{
		return ContextWGL(Current_());
	}

	/// Makes the context current on this thread
	void MakeCurrent(const SurfaceWGL& surface)
	{
		::wglMakeCurrent(surface._hdc, _context);
	}

	/// Releases the current context without binding a new one
	void Release(void)
	{
		::wglMakeCurrent(::HDC(0), ::HGLRC(0));
	}
};

inline
::HGLRC GetHGLRC(const ContextWGL& cwgl)
OGLPLUS_NOEXCEPT(true)
{
	return cwgl._context;
}

typedef ContextWGL Context;

class CurrentContextWGL
 : public ContextWGL
{
public:
	CurrentContextWGL(void)
	 : ContextWGL(ContextWGL::Current_())
	{ }
};

typedef CurrentContextWGL CurrentContext;

} // namespace native
} // namespace oglplus

#endif // include guard
