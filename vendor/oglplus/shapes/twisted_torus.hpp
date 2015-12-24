/**
 *  @file oglplus/shapes/twisted_torus.hpp
 *  @brief TwistedTorus builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_TWISTED_TORUS_1201021336_HPP
#define OGLPLUS_SHAPES_TWISTED_TORUS_1201021336_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/constants.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a Torus
class TwistedTorus
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	const GLdouble _radius_out, _radius_in, _thickness;
	const GLdouble _s_slip_coef;
	const unsigned _sections, _rings, _twist;
public:
	/// Creates a torus with unit radius centered at the origin
	TwistedTorus(void)
	 : _radius_out(1.0)
	 , _radius_in(0.5)
	 , _thickness(0.02)
	 , _s_slip_coef(0.20)
	 , _sections(12)
	 , _rings(48)
	 , _twist(12)
	{ }

	/// Creates a torus with unit radius centered at the origin
	TwistedTorus(
		GLdouble rad_out,
		GLdouble rad_in,
		GLdouble thickness,
		unsigned sects,
		unsigned rings,
		unsigned twist
	): _radius_out(rad_out)
	 , _radius_in(rad_in)
	 , _thickness(thickness)
	 , _s_slip_coef(0.20)
	 , _sections(sects)
	 , _rings(rings)
	 , _twist(twist)
	{
		assert(_sections % 2 == 0);
		assert(_rings % 2 == 0);
		assert(_thickness > 0.0);
		assert(_thickness < _radius_in);
		assert(_radius_in < _radius_out);
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	std::vector<GLfloat> _positions(void) const;

	GLuint Positions(std::vector<GLfloat>& dest) const
	{
		dest = _positions();
		return 3;
	}

	/// Makes vertex coordinates and returns number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		auto v = _positions();
		dest.assign(v.begin(), v.end());
		return 3;
	}

	std::vector<GLfloat> _normals(void) const;

	GLuint Normals(std::vector<GLfloat>& dest) const
	{
		dest = _normals();
		return 3;
	}

	/// Makes vertex normals and returns number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		auto v = _normals();
		dest.assign(v.begin(), v.end());
		return 3;
	}

	std::vector<GLfloat> _tangents(void) const;

	GLuint Tangents(std::vector<GLfloat>& dest) const
	{
		dest = _tangents();
		return 3;
	}

	/// Makes vertex tangents and returns number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		auto v = _tangents();
		dest.assign(v.begin(), v.end());
		return 3;
	}

	std::vector<GLfloat> _bitangents(void) const;

	GLuint Bitangents(std::vector<GLfloat>& dest) const
	{
		dest = _bitangents();
		return 3;
	}

	/// Makes vertex bi-tangents and returns number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		auto v = _bitangents();
		dest.assign(v.begin(), v.end());
		return 3;
	}

	std::vector<GLfloat> _tex_coords(void) const;

	GLuint TexCoordinates(std::vector<GLfloat>& dest) const
	{
		dest = _tex_coords();
		return 2;
	}

	/// Makes texture coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		auto v = _tex_coords();
		dest.assign(v.begin(), v.end());
		return 2;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** TwistedTorus provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangent" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<TwistedTorus> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		TwistedTorus,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag,
			VertexTangentsTag,
			VertexBitangentsTag,
			VertexTexCoordinatesTag
		>
	> VertexAttribs;
#endif

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		bounding_sphere = oglplus::Sphere<T>(
			T(0),
			T(0),
			T(0),
			T(_radius_out + _thickness)
		);
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return IndexArray();
	}

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Default = Default()) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/twisted_torus.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
