/**
 *  @file oglplus/shapes/icosahedron.hpp
 *  @brief Icosahedron builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_ICOSAHEDRON_1206011111_HPP
#define OGLPLUS_SHAPES_ICOSAHEDRON_1206011111_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

class IcosahedronBase
{
protected:
	static const GLushort* _indices(void);
	static const GLdouble* _positions(void);
};

/// Class providing vertex attributes and instructions for drawing of a icosahedron
class SimpleIcosahedron
 : public DrawingInstructionWriter
 , public DrawMode
 , public IcosahedronBase
{
public:
	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CCW;
	}

	typedef GLuint (SimpleIcosahedron::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	/// Makes the positions and returns the number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		const GLdouble* p = _positions();
		dest.assign(p, p+12*3);
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Icosahedron provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 */
	typedef VertexAttribsInfo<Icosahedron> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		SimpleIcosahedron,
		std::tuple<VertexPositionsTag>
	> VertexAttribs;
#endif

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		bounding_sphere = oglplus::Sphere<T>(T(0), T(0), T(0), T(1));
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLushort> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		const GLushort* i = _indices();
		return IndexArray(i, i+20*3);
	}

	DrawingInstructions Instructions(PrimitiveType mode) const;

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const
	{
		return Instructions(PrimitiveType::Triangles);
	}
};

/// Class providing vertex attributes and instructions for drawing of a icosahedron
class Icosahedron
 : public DrawingInstructionWriter
 , public DrawMode
 , public IcosahedronBase
{
public:
	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	typedef GLuint (Icosahedron::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	/// Makes the positions and returns the number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		dest.resize(20*3*3);

		const GLdouble* p = _positions();
		const GLushort* i = _indices();

		for(GLuint f=0; f!=20; ++f)
		{
			for(GLuint v=0; v!=3; ++v)
			{
				for(GLuint c=0; c!=3; ++c)
				{
					dest[f*9+v*3+c] = T(p[i[f*3+v]*3+c]);
				}
			}
		}
		return 3;
	}

	/// Makes the normals and returns the number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		dest.resize(20*3*3);

		const GLdouble* p = _positions();
		const GLushort* i = _indices();

		for(GLuint f=0; f!=20; ++f)
		{
			Vector<T, 3> v0(
				static_cast<T>(p[i[f*3+0]*3+0]),
				static_cast<T>(p[i[f*3+0]*3+1]),
				static_cast<T>(p[i[f*3+0]*3+2])
			);
			Vector<T, 3> v1(
				static_cast<T>(p[i[f*3+1]*3+0]),
				static_cast<T>(p[i[f*3+1]*3+1]),
				static_cast<T>(p[i[f*3+1]*3+2])
			);
			Vector<T, 3> v2(
				static_cast<T>(p[i[f*3+2]*3+0]),
				static_cast<T>(p[i[f*3+2]*3+1]),
				static_cast<T>(p[i[f*3+2]*3+2])
			);
			Vector<T, 3> fn(Normalized(Cross(v1-v0, v2-v0)));

			for(GLuint v=0; v!=3; ++v)
			{
				for(GLuint c=0; c!=3; ++c)
				{
					dest[f*9+v*3+c] = fn[c];
				}
			}
		}
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Icosahedron provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normals" the vertex normals (Normals)
	 */
	typedef VertexAttribsInfo<Icosahedron> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Icosahedron,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag
		>
	> VertexAttribs;
#endif

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(Sphere<T>& center_and_radius) const
	{
		center_and_radius = Sphere<T>(T(0), T(0), T(0), T(1));
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLushort> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return IndexArray();
	}

	DrawingInstructions Instructions(PrimitiveType mode) const;

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const
	{
		return Instructions(PrimitiveType::Triangles);
	}
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/icosahedron.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
