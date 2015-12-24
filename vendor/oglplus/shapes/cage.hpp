/**
 *  @file oglplus/shapes/cage.hpp
 *  @brief Cage builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_CAGE_1107121519_HPP
#define OGLPLUS_SHAPES_CAGE_1107121519_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>
#include <oglplus/math/matrix.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a cage
class Cage
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	Vector<GLdouble, 3> _size;
	Vector<GLdouble, 3> _barw;
	Vector<GLdouble, 3> _divs;

	static const Matrix<GLdouble, 3, 3>& _face_mat(GLuint face);

	GLdouble _face_size(GLuint face, GLuint axis) const
	{
		return std::fabs(Dot(_face_mat(face).Row(axis), _size));
	}

	GLdouble _face_barw(GLuint face, GLuint axis) const
	{
		return std::fabs(Dot(_face_mat(face).Row(axis), _barw));
	}

	GLuint _face_divs(GLuint face, GLuint axis) const
	{
		return GLuint(std::fabs(Dot(_face_mat(face).Row(axis), _divs)));
	}

	static const Vector<GLdouble, 3> _face_vec(
		GLuint face,
		const Vector<GLdouble, 3> vec
	)
	{
		return _face_mat(face)*vec;
	}

	template <typename Iter>
	static Iter _write(Iter iter, const Vector<GLdouble, 3>& vec)
	{
		*iter++ = GLfloat(vec.x());
		*iter++ = GLfloat(vec.y());
		*iter++ = GLfloat(vec.z());
		return iter;
	}

	GLuint _vert_count(void) const;

	// primitive-restart-index
	GLuint _pri(void) const { return _vert_count(); }

	GLuint _index_count(void) const;
public:
	/// Constructs a unit cage centered at the origin
	Cage(void)
	 : _size(1, 1, 1)
	 , _barw(0.15, 0.15, 0.15)
	 , _divs(4, 4, 4)
	{ }

	/// Constructs a cage with width, height, depth
	Cage(
		GLdouble xs, GLdouble ys, GLdouble zs,
		GLdouble xb, GLdouble yb, GLdouble zb,
		GLuint xd, GLuint yd, GLuint zd
	): _size(xs, ys, zs)
	 , _barw(xb, yb, zb)
	 , _divs(xd, yd, zd)
	{
		assert(xs > 0.0);
		assert(ys > 0.0);
		assert(zs > 0.0);

		assert(xd > 0);
		assert(yd > 0);
		assert(zd > 0);

		assert(xs > xb*(xd-1));
		assert(ys > yb*(yd-1));
		assert(zs > zb*(zd-1));
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	typedef GLuint (Cage::*VertexAttribFunc)(std::vector<GLfloat>&) const;

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
		auto p = _positions();
		dest.assign(p.begin(), p.end());
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
		auto n = _normals();
		dest.assign(n.begin(), n.end());
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
		auto t = _tangents();
		dest.assign(t.begin(), t.end());
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
		auto t = _tex_coords();
		dest.assign(t.begin(), t.end());
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Cage provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Cage> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Cage,
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
			T(0),
			T(0),
			T(0),
			T(Length(_size))
		);
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const;

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/cage.ipp>
#endif

#endif // include guard
