/**
 *  @file oglplus/native/surface_wgl.hpp
 *  @brief Wrapper for native current WGL/OpenGL surface
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
// NOTE: the include guard is intentionally without the _WGL suffix so that
// different native surface implementation cannot be included simultaneously.

#define OGLPLUS_NATIVE_WGL 1

#include <oglplus/native/common_wgl.hpp>
#include <Windows.h>

namespace oglplus {
namespace native {

/// Wrapper for WGL (DC) surface handle
class SurfaceWGL
{
private:
	::HDC _hdc;

	friend class ContextWGL;

	struct Current_ { };

	SurfaceWGL(Current_)
	 : _hdc(::wglGetCurrentDC())
	{
		if(!_hdc) HandleNoWGLDC();
	}
public:
	/// Returns a wrapper for the currently bound WGL surface
	/** This function gets and wraps the current WGL Device Context.
	 *  If no DC is current it throws a @c runtime_error.
	 *
	 *  @throws std::runtime_error
	 */
	static SurfaceWGL Current(void)
	{
		return SurfaceWGL(Current_());
	}

	/// Returns the width of the surface
	int Width(void) const
	{
		::RECT rect;
		if(::GetClientRect(::WindowFromDC(_hdc), &rect) == TRUE)
		{
			return rect.right - rect.left;
		}
		return 0;
	}

	/// Returns the height of the surface
	int Height(void) const
	{
		::RECT rect;
		if(::GetClientRect(::WindowFromDC(_hdc), &rect) == TRUE)
		{
			return rect.bottom - rect.top;
		}
		return 0;
	}

	/// Swaps the front and back buffers
	void SwapBuffers(void)
	{
		::SwapBuffers(_hdc);
	}
};

typedef SurfaceWGL Surface;

} // namespace native
} // namespace oglplus

#endif // include guard
