/**
 *  @file oglplus/native/surface_glx.hpp
 *  @brief Wrapper for native current GLX/OpenGL surface
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_NATIVE_SURFACE_1404232057_HPP
#define OGLPLUS_NATIVE_SURFACE_1404232057_HPP
// NOTE: the include guard is intentionally without the _GLX suffix so that
// different native surface implementation cannot be included simultaneously.

#define OGLPLUS_NATIVE_GLX 1

#include <oglplus/native/common_glx.hpp>
#include <GL/glx.h>

namespace oglplus {
namespace native {

/// Wrapper for GLX (drawable) surface handle
class SurfaceGLX
{
private:
	::Display* _display;
	::GLXDrawable  _drawable;

	friend class ContextGLX;

	struct Current_ { };

	SurfaceGLX(Current_)
	 : _display(::glXGetCurrentDisplay())
	 , _drawable(::glXGetCurrentDrawable())
	{
		if(!_display) HandleNoGLXDisplay();
		if(!_drawable) HandleNoGLXDrawable();
	}

	unsigned _query(int param) const
	{
		unsigned result = 0;
		::glXQueryDrawable(
			_display,
			_drawable,
			param,
			&result
		);
		return result;
	}
public:
	/// Returns a wrapper for the currently bound GLX surface
	/** This function gets and wraps the current GLX drawable (+display).
	 *  If no drawable is current it throws a @c runtime_error.
	 *
	 *  @throws std::runtime_error
	 */
	static SurfaceGLX Current(void)
	{
		return SurfaceGLX(Current_());
	}

	/// Returns the width of the surface
	int Width(void) const
	{
		return int(_query(GLX_WIDTH));
	}

	/// Returns the height of the surface
	int Height(void) const
	{
		return int(_query(GLX_HEIGHT));
	}

	/// Swaps the front and back buffers
	void SwapBuffers(void)
	{
		::glXSwapBuffers(_display, _drawable);
	}
};

typedef SurfaceGLX Surface;

} // namespace native
} // namespace oglplus

#endif // include guard
