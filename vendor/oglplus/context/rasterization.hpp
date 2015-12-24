/**
 *  @file oglplus/context/rasterization.hpp
 *  @brief Wrappers for basic point, line and polygon resterization operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_RASTERIZATION_1201040722_HPP
#define OGLPLUS_CONTEXT_RASTERIZATION_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/face_mode.hpp>
#include <oglplus/polygon_mode.hpp>
#include <oglplus/provoke_mode.hpp>
#include <oglplus/frag_data_slot.hpp>
#include <oglplus/math/vector.hpp>

namespace oglplus {
namespace context {

/// Helper structure storing front and back polygon modes
struct PolygonModes
{
	// private implementation detail, do not use
	GLint _v[2];

	PolygonModes(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	PolygonModes(PolygonMode mode)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = GLint(mode);
		_v[1] = GLint(mode);
	}

	PolygonModes(PolygonMode front, PolygonMode back)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = GLint(front);
		_v[1] = GLint(back);
	}

	/// The front polygon mode
	PolygonMode Front(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return PolygonMode(GLenum(_v[0]));
	}

	/// The back polygon mode
	PolygonMode Back(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return PolygonMode(GLenum(_v[1]));
	}

	friend
	bool operator == (const PolygonModes& a, const PolygonModes& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._v[0] == b._v[0]) && (a._v[1] == b._v[1]);
	}

	friend
	bool operator != (const PolygonModes& a, const PolygonModes& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._v[0] != b._v[0]) || (a._v[1] != b._v[1]);
	}
};

struct PolygonOffsPara
{
	GLfloat _factor;
	GLfloat _units;

	PolygonOffsPara(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	PolygonOffsPara(GLfloat factor, GLfloat units)
	OGLPLUS_NOEXCEPT(true)
	 : _factor(factor)
	 , _units(units)
	{ }

	GLfloat Factor(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _factor;
	}

	GLfloat Units(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _units;
	}

	friend
	bool operator == (const PolygonOffsPara& a, const PolygonOffsPara& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._factor == b._factor) && (a._units == b._units);
	}

	friend
	bool operator != (const PolygonOffsPara& a, const PolygonOffsPara& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return (a._factor != b._factor) || (a._units != b._units);
	}
};

/// Wrapper for the basic point, line and polygon rasterization operations
/**
 *  @ingroup ogl_context
 */
class RasterizationState
{
public:
	/// Sets the polygon facing mode
	/**
	 *  @glsymbols
	 *  @glfunref{FrontFace}
	 */
	static void FrontFace(FaceOrientation orientation)
	{
		OGLPLUS_GLFUNC(FrontFace)(GLenum(orientation));
		OGLPLUS_VERIFY(
			FrontFace,
			Error,
			EnumParam(orientation)
		);
	}

	static FaceOrientation FrontFace(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_FRONT_FACE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return FaceOrientation(GLenum(result));
	}

	/// Sets the face culling mode
	/**
	 *  @glsymbols
	 *  @glfunref{CullFace}
	 */
	static void CullFace(Face mode)
	{
		OGLPLUS_GLFUNC(CullFace)(GLenum(mode));
		OGLPLUS_VERIFY(
			CullFace,
			Error,
			EnumParam(mode)
		);
	}

	/// Returns the face culling mode
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{CULL_FACE_MODE}
	 */
	static Face CullFaceMode(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_CULL_FACE_MODE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return Face(result);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the polygon rasterization mode
	/**
	 *  @glsymbols
	 *  @glfunref{PolygonMode}
	 */
	static void PolygonMode(Face face, oglplus::PolygonMode mode)
	{
		OGLPLUS_GLFUNC(PolygonMode)(GLenum(face), GLenum(mode));
		OGLPLUS_VERIFY(
			PolygonMode,
			Error,
			EnumParam(mode)
		);
	}

	/// Sets the polygon rasterization mode
	/**
	 *  @glsymbols
	 *  @glfunref{PolygonMode}
	 */
	static void PolygonMode(oglplus::PolygonMode mode)
	{
		OGLPLUS_GLFUNC(PolygonMode)(GL_FRONT_AND_BACK, GLenum(mode));
		OGLPLUS_VERIFY(
			PolygonMode,
			Error,
			EnumParam(mode)
		);
	}

	/// Returns the front face rasterization mode
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POLYGON_MODE}
	 */
	static oglplus::PolygonMode PolygonModeFront(void)
	{
		GLint result[2];
		OGLPLUS_GLFUNC(GetIntegerv)(GL_POLYGON_MODE, result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::PolygonMode(result[0]);
	}

	/// Returns the back face rasterization mode
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POLYGON_MODE}
	 */
	static oglplus::PolygonMode PolygonModeBack(void)
	{
		GLint result[2];
		OGLPLUS_GLFUNC(GetIntegerv)(GL_POLYGON_MODE, result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::PolygonMode(result[1]);
	}

	static void PolygonMode(const PolygonModes& modes)
	{
		if(modes._v[0] == modes._v[1])
		{
			OGLPLUS_GLFUNC(PolygonMode)(
				GL_FRONT_AND_BACK,
				GLenum(modes._v[0])
			);
			OGLPLUS_VERIFY_SIMPLE(PolygonMode);
		}
		else
		{
			OGLPLUS_GLFUNC(PolygonMode)(
				GL_FRONT,
				GLenum(modes._v[0])
			);
			OGLPLUS_VERIFY_SIMPLE(PolygonMode);
			OGLPLUS_GLFUNC(PolygonMode)(
				GL_BACK,
				GLenum(modes._v[1])
			);
			OGLPLUS_VERIFY_SIMPLE(PolygonMode);
		}
	}

	static PolygonModes PolygonMode(void)
	{
		PolygonModes result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_POLYGON_MODE, result._v);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}
#endif // GL_VERSION_3_0

	/// Sets the polygon depth offset
	/**
	 *  @glsymbols
	 *  @glfunref{PolygonOffset}
	 */
	static void PolygonOffset(GLfloat factor, GLfloat units)
	{
		OGLPLUS_GLFUNC(PolygonOffset)(factor, units);
		OGLPLUS_VERIFY_SIMPLE(PolygonOffset);
	}

	/// Returns the polygon offset factor
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POLYGON_OFFSET_FACTOR}
	 */
	static GLfloat PolygonOffsetFactor(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_POLYGON_OFFSET_FACTOR, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

	/// Returns the polygon offset units
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POLYGON_OFFSET_UNITS}
	 */
	static GLfloat PolygonOffsetUnits(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_POLYGON_OFFSET_UNITS, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

	static void PolygonOffset(const PolygonOffsPara& para)
	{
		OGLPLUS_GLFUNC(PolygonOffset)(para.Factor(), para.Units());
		OGLPLUS_VERIFY_SIMPLE(PolygonOffset);
	}

	static PolygonOffsPara PolygonOffset(void)
	{
		return PolygonOffsPara(
			PolygonOffsetFactor(),
			PolygonOffsetUnits()
		);
	}

	/// Sets the line width
	/**
	 *  @glsymbols
	 *  @glfunref{LineWidth}
	 */
	static void LineWidth(GLfloat width)
	{
		OGLPLUS_GLFUNC(LineWidth)(width);
		OGLPLUS_VERIFY_SIMPLE(LineWidth);
	}

	/// Returns the line width
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{LINE_WIDTH}
	 */
	static GLfloat LineWidth(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_LINE_WIDTH, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the point size
	/**
	 *  @glsymbols
	 *  @glfunref{PointSize}
	 */
	static void PointSize(GLfloat size)
	{
		OGLPLUS_GLFUNC(PointSize)(size);
		OGLPLUS_VERIFY_SIMPLE(PointSize);
	}

	/// Returns the point size
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POINT_SIZE}
	 */
	static GLfloat PointSize(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_POINT_SIZE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

	/// Sets the point fade threshold size
	/**
	 *  @glsymbols
	 *  @glfunref{PointParameter}
	 *  @gldefref{POINT_FADE_THRESHOLD_SIZE}
	 */
	static void PointFadeThresholdSize(GLfloat size)
	{
		OGLPLUS_GLFUNC(PointParameterf)(
			GL_POINT_FADE_THRESHOLD_SIZE,
			size
		);
		OGLPLUS_VERIFY_SIMPLE(PointParameterf);
	}

	/// Returns the point fade threshold size
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{POINT_FADE_THRESHOLD_SIZE}
	 */
	static GLfloat PointFadeThresholdSize(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_POINT_FADE_THRESHOLD_SIZE,&result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}
#endif // GL_VERSION_3_0

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2 || GL_ARB_provoking_vertex
	/// Sets the provoking vertex selection mode for flatshading
	/**
	 *  @glvoereq{3,2,ARB,provoking_vertex}
	 *  @glsymbols
	 *  @glfunref{ProvokingVertex}
	 */
	static void ProvokingVertex(ProvokeMode mode)
	{
		OGLPLUS_GLFUNC(ProvokingVertex)(GLenum(mode));
		OGLPLUS_VERIFY_SIMPLE(ProvokingVertex);
	}

	/// Returns the provoking vertex selection mode for flatshading
	/**
	 *  @glvoereq{3,2,ARB,provoking_vertex}
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{PROVOKING_VERTEX}
	 */
	static ProvokeMode ProvokingVertex(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_PROVOKING_VERTEX, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return ProvokeMode(result);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0
	/// Returns the minimal sample shading value
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{MIN_SAMPLE_SHADING_VALUE}
	 */
	static GLfloat MinSampleShading(void)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GL_MIN_SAMPLE_SHADING_VALUE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

	/// Sets the multisampling minimal sample shading value
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{MinSampleShading}
	 */
	static void MinSampleShading(GLfloat value)
	{
		OGLPLUS_GLFUNC(MinSampleShading)(value);
		OGLPLUS_VERIFY_SIMPLE(MinSampleShading);
	}
#endif

#if GL_NV_fragment_coverage_to_color
	/// Specifies the fragment output to be updated with the coverage value
	/**
	 *  @glextreq{NV,fragment_coverage_to_color}
	 *  @glsymbols
	 *  @glfunref{FragmentCoverageColorNV}
	 */
	static void FragmentCoverageColor(FragDataSlot buffer)
	{
		OGLPLUS_GLFUNC(FragmentCoverageColorNV)(GLuint(buffer));
		OGLPLUS_CHECK_SIMPLE(FragmentCoverageColorNV);
	}

	static FragDataSlot FragmentCoverageColor(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_FRAGMENT_COVERAGE_COLOR_NV,
			&result
		);
		OGLPLUS_CHECK_SIMPLE(GetIntegerv);
		return FragDataSlot(GLuint(result));
	}
#endif
};

class RasterizationOps
{
public:
	/// Returns the value of sample buffers
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{SAMPLE_BUFFERS}
	 */
	static GLint SampleBuffers(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_SAMPLE_BUFFERS, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	/// Returns the number of multisampling samples
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{SAMPLES}
	 */
	static GLint Samples(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_SAMPLES, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
	/// Returns the position of the specified multisampling sample
	/**
	 *  @glverreq{3,2}
	 *  @glsymbols
	 *  @glfunref{GetMultisample}
	 *  @gldefref{SAMPLE_POSITION}
	 */
	static Vec2f SamplePosition(GLuint index)
	{
		Vec2f result;
		OGLPLUS_GLFUNC(GetMultisamplefv)(
			GL_SAMPLE_POSITION,
			index,
			result.Data()
		);
		OGLPLUS_VERIFY_SIMPLE(GetMultisamplefv);
		return result;
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard
