/**
 *  @file oglplus/shapes/spiral_sphere.hpp
 *  @brief Builder of a partial sphere shape made from spiraling bands
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_SPIRAL_SPHERE_1107121519_HPP
#define OGLPLUS_SHAPES_SPIRAL_SPHERE_1107121519_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/constants.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

/// Class providing data and instructions for rendering of a "spiral sphere"
class SpiralSphere
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	const double _radius, _thickness;
	const unsigned _bands, _divisions, _segments;

	unsigned _vertex_count(void) const;

	template <typename T>
	void _make_vectors(
		std::vector<T>& dest,
		unsigned& k,
		double sign,
		double radius
	) const;

	template <typename T>
	void _make_tangents(
		std::vector<T>& dest,
		unsigned& k,
		double sign
	) const;

	template <typename T>
	void _make_bitangents(
		std::vector<T>& dest,
		unsigned& k,
		double sign
	) const;

	template <typename T>
	void _make_uv_coords(std::vector<T>& dest, unsigned& k) const;

	template <typename T>
	void _make_side_verts(std::vector<T>& dest, unsigned& k) const;

	template <typename T>
	void _make_side_norms(std::vector<T>& dest, unsigned& k) const;

	template <typename T>
	void _make_side_tgts(std::vector<T>& dest, unsigned& k) const;

	template <typename T>
	void _make_side_btgs(std::vector<T>& dest, unsigned& k) const;

	template <typename T>
	void _make_side_uvs(std::vector<T>& dest, unsigned& k) const;
public:
	/// Creates a default spiral sphere
	SpiralSphere(void)
	 : _radius(1.0)
	 , _thickness(0.1)
	 , _bands(4)
	 , _divisions(8)
	 , _segments(48)
	{ }

	/// Creates a custom spiral sphere
	SpiralSphere(
		double radius,
		double thickness,
		unsigned bands,
		unsigned divisions,
		unsigned segments
	): _radius(radius)
	 , _thickness(thickness)
	 , _bands(bands)
	 , _divisions(divisions)
	 , _segments(segments)
	{ }

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CCW;
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

	/// Makes texture-coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		auto v = _tex_coords();
		dest.assign(v.begin(), v.end());
		return 2;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** SpiralSphere provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangent" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<SpiralSphere> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		SpiralSphere,
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
			T(_radius + _thickness)
		);
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const;

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Default = Default()) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/spiral_sphere.ipp>
#endif //OGLPLUS_LINK_LIBRARY

#endif // include guard
