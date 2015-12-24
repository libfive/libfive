/**
 *  @file oglplus/shapes/screen.hpp
 *  @brief Builder for a rectangle covering the screen
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_SCREEN_1107121519_HPP
#define OGLPLUS_SHAPES_SCREEN_1107121519_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/sphere.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/sphere.hpp>

#include <cassert>
#include <cmath>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a Screen
class Screen
 : public DrawingInstructionWriter
 , public DrawMode
{
public:
	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	/// Makes vertex normals and returns number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		unsigned k = 0;
		dest.resize(12);

		for(unsigned i=0; i!=4; ++i)
		{
			dest[k++] = T(0);
			dest[k++] = T(0);
			dest[k++] = T(1);
		}
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex tangents and returns number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		unsigned k = 0;
		dest.resize(12);

		for(unsigned i=0; i!=4; ++i)
		{
			dest[k++] = T(1);
			dest[k++] = T(0);
			dest[k++] = T(0);
		}
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex bi-tangents and returns number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		unsigned k = 0;
		dest.resize(12);

		for(unsigned i=0; i!=4; ++i)
		{
			dest[k++] = T(0);
			dest[k++] = T(1);
			dest[k++] = T(0);
		}
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex coordinates and returns number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		unsigned k = 0;
		dest.resize(8);

		dest[k++] = T(-1);
		dest[k++] = T(-1);

		dest[k++] = T(-1);
		dest[k++] = T(+1);

		dest[k++] = T(+1);
		dest[k++] = T(-1);

		dest[k++] = T(+1);
		dest[k++] = T(+1);

		assert(k == dest.size());
		// 2 values per vertex
		return 2;
	}

	/// Makes texture-coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		unsigned k = 0;
		dest.resize(8);

		dest[k++] = T(0);
		dest[k++] = T(0);

		dest[k++] = T(0);
		dest[k++] = T(1);

		dest[k++] = T(1);
		dest[k++] = T(0);

		dest[k++] = T(1);
		dest[k++] = T(1);

		assert(k == dest.size());
		// 2 values per vertex
		return 2;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Screen provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangent" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Screen> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Screen,
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
		bounding_sphere = oglplus::Sphere<T>(T(0), T(0), T(0), T(1));
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return IndexArray();
	}

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Default = Default()) const
	{
		auto instructions = this->MakeInstructions();

		DrawOperation operation;
		operation.method = DrawOperation::Method::DrawArrays;
		operation.mode = PrimitiveType::TriangleStrip;
		operation.first = GLuint(0);
		operation.count = 4;
		operation.restart_index = DrawOperation::NoRestartIndex();
		operation.phase = 0;
		this->AddInstruction(instructions, operation);

		return instructions;
	}
};

} // shapes
} // oglplus

#endif // include guard
