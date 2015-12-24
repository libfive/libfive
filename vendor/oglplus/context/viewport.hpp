/**
 *  @file oglplus/context/viewport.hpp
 *  @brief Wrappers for viewport-related operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_VIEWPORT_1201040722_HPP
#define OGLPLUS_CONTEXT_VIEWPORT_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/viewport_index.hpp>

namespace oglplus {
namespace context {

/// Helper structure storing position in a 2D viewport
struct ViewportPosition
{
	// private implementation detail, do not use
	GLint _v[2];

	/// The x-coordinate
	GLint X(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The y-coordinate
	GLint Y(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}
};

/// Helper structure storing the dimensions of a 2D viewport
struct ViewportSize
{
	// private implementation detail, do not use
	GLint _v[2];

	/// The width of the viewport
	GLint Width(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The height of the viewport
	GLint Height(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}
};

/// Helper structure storing the extents of a 2D viewport
struct ViewportExtents
{
	// private implementation detail, do not use
	GLint _v[4];

	ViewportExtents(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	ViewportExtents(GLint x, GLint y, GLint w, GLint h)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = x;
		_v[1] = y;
		_v[2] = w;
		_v[3] = h;
	}

	/// The x-coordinate
	GLint X(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The y-coordinate
	GLint Y(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}

	/// The x-coordinate
	GLint Left(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The y-coordinate
	GLint Bottom(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}

	/// The width of the viewport
	GLint Width(void) const
	{
		return _v[2];
	}

	/// The height of the viewport
	GLint Height(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[3];
	}

	friend
	bool operator == (const ViewportExtents& a, const ViewportExtents& b)
	OGLPLUS_NOEXCEPT(true)
	{
		for(unsigned i=0; i<4; ++i)
		{
			if(a._v[i] != b._v[i]) return false;
		}
		return true;
	}

	friend
	bool operator != (const ViewportExtents& a, const ViewportExtents& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return !(a == b);
	}
};

/// Helper structure storing the min/max bounds range
struct BoundsRange
{
	// private implementation detail, do not use
	GLint _v[2];

	/// The min limit
	GLint Min(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The max limit
	GLint Max(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}
};

/// Helper structure storing the near/far depth range
struct ViewportDepthRange
{
	// private implementation detail, do not use
	GLfloat _v[2];

	ViewportDepthRange(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	ViewportDepthRange(GLfloat near_val, GLfloat far_val)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = near_val;
		_v[1] = far_val;
	}

	/// The near limit
	GLfloat Near(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The far limit
	GLfloat Far(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}
};

/// Wrapper for the viewport-related operations
/**
 *  @ingroup ogl_context
 */
class ViewportState
{
public:
	/// Sets the extents of the current viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Viewport}
	 */
	static void Viewport(GLint x, GLint y, SizeType w, SizeType h)
	{
		OGLPLUS_GLFUNC(Viewport)(x, y, w, h);
		OGLPLUS_CHECK_SIMPLE(Viewport);
	}

	/// Sets the size of the current viewport starting at (0,0)
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Viewport}
	 */
	static void Viewport(SizeType w, SizeType h)
	{
		OGLPLUS_GLFUNC(Viewport)(0, 0, w, h);
		OGLPLUS_CHECK_SIMPLE(Viewport);
	}

	/// Sets the extents of the current viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Viewport}
	 */
	static void Viewport(const ViewportExtents& vp)
	{
		OGLPLUS_GLFUNC(Viewport)(
			vp.X(),
			vp.Y(),
			vp.Width(),
			vp.Height()
		);
		OGLPLUS_CHECK_SIMPLE(Viewport);
	}

	/// Returns the extents of the current viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{VIEWPORT}
	 */
	static ViewportExtents Viewport(void)
	{
		ViewportExtents result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_VIEWPORT, result._v);
		OGLPLUS_CHECK_SIMPLE(GetIntegerv);
		return result;
	}


#if OGLPLUS_DOCUMENTATION_ONLY || \
	GL_ES_VERSION_3_0 || \
	GL_VERSION_4_1 || \
	GL_ARB_ES2_compatibility
	/// Sets the @p near_val / @p far_val depth range of the default viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DepthRangef}
	 */
	static void DepthRange(GLclampf near_val, GLclampf far_val)
	{
		OGLPLUS_GLFUNC(DepthRangef)(near_val, far_val);
		OGLPLUS_CHECK_SIMPLE(DepthRangef);
	}
#elif OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the @p near_val / @p far_val depth range of the default viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DepthRangef}
	 */
	static void DepthRange(GLclampd near_val, GLclampd far_val)
	{
		OGLPLUS_GLFUNC(DepthRange)(near_val, far_val);
		OGLPLUS_CHECK_SIMPLE(DepthRange);
	}
#endif

	static void DepthRange(const ViewportDepthRange& vdr)
	{
		DepthRange(vdr._v[0], vdr._v[1]);
	}

	/// Returns the depth range of the default viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{DEPTH_RANGE}
	 */
	static ViewportDepthRange DepthRange(void)
	{
		ViewportDepthRange result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_DEPTH_RANGE, result._v);
		OGLPLUS_CHECK_SIMPLE(GetFloatv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_viewport_array
	/// Sets the extents of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{ViewportIndexedf}
	 */
	static void Viewport(
		ViewportIndex viewport,
		GLfloat x,
		GLfloat y,
		GLfloat width,
		GLfloat height
	)
	{
		OGLPLUS_GLFUNC(ViewportIndexedf)(
			GLuint(viewport),
			x, y,
			width,
			height
		);
		OGLPLUS_CHECK(
			ViewportIndexedf,
			Error,
			Index(GLuint(viewport))
		);
	}

	/// Sets the @p extents of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{ViewportIndexedfv}
	 */
	static void Viewport(ViewportIndex viewport, const GLfloat* extents)
	{
		OGLPLUS_GLFUNC(ViewportIndexedfv)(GLuint(viewport), extents);
		OGLPLUS_CHECK(
			ViewportIndexedfv,
			Error,
			Index(GLuint(viewport))
		);
	}

	/// Sets @p extents of the viewports specified by @p first and @p count
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{ViewportIndexedfv}
	 */
	static void ViewportArray(
		GLuint first,
		SizeType count,
		const GLfloat* extents
	)
	{
		OGLPLUS_GLFUNC(ViewportArrayv)(first, count, extents);
		OGLPLUS_CHECK_SIMPLE(ViewportArrayv);
	}

	/// Sets the extents of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{ViewportIndexedf}
	 */
	static void Viewport(
		ViewportIndex viewport,
		const ViewportExtents& vp
	)
	{
		OGLPLUS_GLFUNC(ViewportIndexedf)(
			GLuint(viewport),
			GLfloat(vp.X()),
			GLfloat(vp.Y()),
			GLfloat(vp.Width()),
			GLfloat(vp.Height())
		);
		OGLPLUS_CHECK(
			ViewportIndexedf,
			Error,
			Index(GLuint(viewport))
		);
	}

	/// Returns the extents of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{VIEWPORT}
	 */
	static ViewportExtents Viewport(ViewportIndex viewport)
	{
		ViewportExtents result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_VIEWPORT,
			GLuint(viewport),
			result._v
		);
		OGLPLUS_CHECK_SIMPLE(GetIntegeri_v);
		return result;
	}


	/// Sets the @p near_val / @p far_val depth range of a @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DepthRangeIndexed}
	 */
	static void DepthRange(
		ViewportIndex viewport,
		GLclampd near_val,
		GLclampd far_val
	)
	{
		OGLPLUS_GLFUNC(DepthRangeIndexed)(
			GLuint(viewport),
			near_val,
			far_val
		);
		OGLPLUS_CHECK(
			DepthRangeIndexed,
			Error,
			Index(GLuint(viewport))
		);
	}

	static void DepthRange(
		ViewportIndex viewport,
		const ViewportDepthRange& vdr
	)
	{
		DepthRange(viewport, vdr._v[0], vdr._v[1]);
	}

	/// Sets depth ranges of viewports specified by @p first and @p count
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DepthRangeArrayv}
	 */
	static void DepthRangeArray(
		GLuint first,
		SizeType count,
		const GLclampd *v
	)
	{
		OGLPLUS_GLFUNC(DepthRangeArrayv)(first, count, v);
		OGLPLUS_CHECK_SIMPLE(DepthRangeArrayv);
	}

	/// Returns the depth range of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{DEPTH_RANGE}
	 */
	static ViewportDepthRange DepthRange(ViewportIndex viewport)
	{
		ViewportDepthRange result;
		OGLPLUS_GLFUNC(GetFloati_v)(
			GL_DEPTH_RANGE,
			GLuint(viewport),
			result._v
		);
		OGLPLUS_CHECK(
			GetFloati_v,
			Error,
			Index(GLuint(viewport))
		);
		return result;
	}
#endif
};

class ViewportOps
{
public:
	/// Returns the implementation-dependent maximum viewport dimensions
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{MAX_VIEWPORT_DIMS}
	 */
	static ViewportSize MaxViewportDims(void)
	{
		ViewportSize result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_MAX_VIEWPORT_DIMS,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_viewport_array
	/// Returns the implementation-dependent viewport bounds range
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{VIEWPORT_BOUNDS_RANGE}
	 */
	static BoundsRange ViewportBoundsRange(void)
	{
		BoundsRange result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_VIEWPORT_BOUNDS_RANGE,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	/// Returns the number of available viewports
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{MAX_VIEWPORTS}
	 */
	static GLuint MaxViewports(void)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_MAX_VIEWPORTS, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return GLuint(result);
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard
