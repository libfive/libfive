/**
 *  @file oglplus/shapes/cube.hpp
 *  @brief Cube builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_CUBE_1107121519_HPP
#define OGLPLUS_SHAPES_CUBE_1107121519_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/sphere.hpp>

#include <cmath>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a cube
class Cube
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	GLdouble _sx, _sy, _sz;
	GLdouble _ox, _oy, _oz;
public:
	/// Constructs a unit cube centered at the origin
	Cube(void)
	 : _sx(1.0)
	 , _sy(1.0)
	 , _sz(1.0)
	 , _ox(0.0)
	 , _oy(0.0)
	 , _oz(0.0)
	{ }

	/// Constructs a cube with width, height, depth
	Cube(GLdouble w, GLdouble h, GLdouble d)
	 : _sx(w)
	 , _sy(h)
	 , _sz(d)
	 , _ox(0.0)
	 , _oy(0.0)
	 , _oz(0.0)
	{ }

	/// Constructs a cube with width, height, depth and x,y and z offsets
	Cube(
		GLdouble w,
		GLdouble h,
		GLdouble d,
		GLdouble x,
		GLdouble y,
		GLdouble z
	): _sx(w)
	 , _sy(h)
	 , _sz(d)
	 , _ox(x)
	 , _oy(y)
	 , _oz(z)
	{ }

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	typedef GLuint (Cube::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	std::vector<GLfloat> _positions(void) const;

	GLuint Positions(std::vector<GLfloat>& dest) const
	{
		dest = _positions();
		return 3;
	}

	/// Makes the vertices and returns the number of values per vertex
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

	/// Makes the normals and returns the number of values per vertex
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

	/// Makes the tangents and returns the number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		auto v = _tangents();
		dest.assign(v.begin(), v.end());
		return 3;
	}

	std::vector<GLfloat> _tex_coords(void) const;

	GLuint TexCoordinates(std::vector<GLfloat>& dest) const
	{
		dest = _tex_coords();
		return 3;
	}

	/// Makes the texture coordinates and returns the number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		auto v = _tex_coords();
		dest.assign(v.begin(), v.end());
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Cube provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Cube> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Cube,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag,
			VertexTangentsTag,
			VertexTexCoordinatesTag
		>
	> VertexAttribs;
#endif

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		bounding_sphere = oglplus::Sphere<T>(
			T(_ox),
			T(_oy),
			T(_oz),
			T(std::sqrt(_sx*_sx + _sy*_sy + _sz*_sz))
		);
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLushort> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return IndexArray();
	}

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const;

	/// Returns element indices for the Cube's edges
	IndexArray Indices(Edges) const;

	/// Returns the instructions for rendering of edges
	DrawingInstructions Instructions(Edges) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/cube.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
