/**
 *  @file oglplus/shapes/plane.hpp
 *  @brief Plane builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_PLANE_1107121519_HPP
#define OGLPLUS_SHAPES_PLANE_1107121519_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/sphere.hpp>

#include <cmath>
#include <cassert>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a Plane
class Plane
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	Vec3f _point;
	Vec3f _u, _v;
	unsigned _udiv, _vdiv;

	unsigned _vertex_count(void) const
	{
		return (_udiv+1)*(_vdiv+1);
	}
public:
	/// Creates a default plane
	Plane(void)
	 : _point(0.0f, 0.0f, 0.0f)
	 , _u(1.0f, 0.0f, 0.0f)
	 , _v(0.0f, 0.0f,-1.0f)
	 , _udiv(2)
	 , _vdiv(2)
	{ }

	/// Creates a default plane with udiv and vdiv divisions
	Plane(unsigned udiv, unsigned vdiv)
	 : _point(0.0f, 0.0f, 0.0f)
	 , _u(1.0f, 0.0f, 0.0f)
	 , _v(0.0f, 0.0f,-1.0f)
	 , _udiv(udiv)
	 , _vdiv(vdiv)
	{
		assert(udiv > 1);
		assert(vdiv > 1);
	}

	/// Creates a plane going through origin specified by @p u and @p v
	Plane(const Vec3f u, const Vec3f v)
	 : _point(0.0f, 0.0f, 0.0f)
	 , _u(u)
	 , _v(v)
	 , _udiv(2)
	 , _vdiv(2)
	{
		assert(Length(_u) > 0.0f);
		assert(Length(_v) > 0.0f);
	}

	/// Creates a plane going through @p p specified by @p u and @p v
	Plane(
		const Vec3f p,
		const Vec3f u,
		const Vec3f v,
		unsigned udiv,
		unsigned vdiv
	): _point(p)
	 , _u(u)
	 , _v(v)
	 , _udiv(udiv)
	 , _vdiv(vdiv)
	{
		assert(Length(_u) > 0.0f);
		assert(Length(_v) > 0.0f);
		assert(_udiv != 0);
		assert(_vdiv != 0);
	}

	const Vec3f& Point(void) const
	{
		return _point;
	}

	const Vec3f& U(void) const
	{
		return _u;
	}

	const Vec3f& V(void) const
	{
		return _v;
	}

	inline Vec3f Normal(void) const
	{
		return Normalized(Cross(_u, _v));
	}

	inline Vec3f Tangential(void) const
	{
		return Normalized(_u);
	}

	inline Vec3f Bitangential(void) const
	{
		return Normalized(_v);
	}

	Vec4f Equation(void) const
	{
		return Vec4f(Normal(), -Dot(Normal(), _point));
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	/// Makes vertex normals and returns number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		unsigned k = 0, n = _vertex_count();
		dest.resize(n * 3);
		Vec3f normal = Normal();

		for(unsigned i=0; i!=n; ++i)
		{
			dest[k++] = T(normal.x());
			dest[k++] = T(normal.y());
			dest[k++] = T(normal.z());
		}
		//
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex tangents and returns number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		unsigned k = 0, n = _vertex_count();
		dest.resize(n * 3);
		Vec3f tangent(Normalized(_u));

		for(unsigned i=0; i!=n; ++i)
		{
			dest[k++] = T(tangent.x());
			dest[k++] = T(tangent.y());
			dest[k++] = T(tangent.z());
		}
		//
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex bi-tangents and returns number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		unsigned k = 0, n = _vertex_count();
		dest.resize(n * 3);
		Vec3f bitangent(Normalized(_v));

		for(unsigned i=0; i!=n; ++i)
		{
			dest[k++] = T(bitangent.x());
			dest[k++] = T(bitangent.y());
			dest[k++] = T(bitangent.z());
		}
		//
		assert(k == dest.size());
		// 3 values per vertex
		return 3;
	}

	/// Makes vertex coordinates and returns number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		unsigned k = 0, n = _vertex_count();
		dest.resize(n * 3);


		Vec3f pos(_point - _u - _v);
		Vec3f ustep(_u * (2.0 / _udiv));
		Vec3f vstep(_v * (2.0 / _vdiv));

		unsigned leap = _udiv+1;

		for(unsigned j=0; j!=(_vdiv+1); ++j)
		{
			Vec3f tmp = pos;
			for(unsigned i=0; i!=leap; ++i)
			{
				dest[k++] = T(tmp.x());
				dest[k++] = T(tmp.y());
				dest[k++] = T(tmp.z());
				tmp += ustep;
			}
			pos += vstep;
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes texture-coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		unsigned k = 0, n = _vertex_count();
		dest.resize(n * 2);


		T uc, vc = T(0);
		T ustep = T(1) / _udiv;
		T vstep = T(1) / _vdiv;

		unsigned leap = _udiv+1;


		for(unsigned j=0; j!=(_vdiv+1); ++j)
		{
			uc = T(0);
			for(unsigned i=0; i!=leap; ++i)
			{
				dest[k++] = T(uc);
				dest[k++] = T(vc);
				uc += ustep;
			}
			vc += vstep;
		}
		assert(k == dest.size());
		// 2 values per vertex
		return 2;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Plane provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangent" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Plane> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Plane,
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
			T(_point.x()),
			T(_point.y()),
			T(_point.z()),
			T(Length(_u + _v))
		);
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const;

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Default = Default()) const;

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0
	/// Returns element indices that are used with the drawing instructions
	/**
	 *  glverreq{4,0}
	 */
	IndexArray Indices(Patches) const;

	/// Returns the instructions for rendering
	/**
	 *  glverreq{4,0}
	 */
	DrawingInstructions Instructions(Patches) const;
#endif

	/// Returns edge element indices that are used with the drawing instructions
	IndexArray Indices(Edges) const;

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Edges) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/plane.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
