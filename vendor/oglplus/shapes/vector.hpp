/**
 *  @file oglplus/shapes/vector.hpp
 *  @brief PointAndVector builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_VECTOR_1107121519_HPP
#define OGLPLUS_SHAPES_VECTOR_1107121519_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/constants.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a vector
class PointAndVector
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	GLdouble _x, _y, _z;
public:
	/// Constructs an unit vector pointing in the x-axis direction
	PointAndVector(void)
	 : _x(1)
	 , _y(0)
	 , _z(0)
	{ }

	/// Constructs a vector with x,y,z coordinates
	PointAndVector(GLdouble x, GLdouble y, GLdouble z)
	 : _x(x)
	 , _y(y)
	 , _z(z)
	{ }

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	typedef GLuint (PointAndVector::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	/// Makes the vertices and returns the number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		T _positions[3] = {T(0), T(0), T(0)};
		dest.assign(_positions, _positions+3);
		return 3;
	}

	/// Makes the normals and returns the number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		T _normals[3] = {T(_x), T(_y), T(_z)};
		dest.assign(_normals, _normals+3);
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** PointAndVector provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 */
	typedef VertexAttribsInfo<PointAndVector> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		PointAndVector,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag
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
			T(std::sqrt(_x*_x + _y*_y + _z*_z))
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
	DrawingInstructions Instructions(Default = Default()) const
	{
		DrawOperation operation;
		operation.method = DrawOperation::Method::DrawArrays;
		operation.mode = PrimitiveType::Points;
		operation.first = 0;
		operation.count = 1;
		operation.restart_index = DrawOperation::NoRestartIndex();
		operation.phase = 0;

		return this->MakeInstructions(operation);
	}
};

} // shapes
} // oglplus

#endif // include guard
