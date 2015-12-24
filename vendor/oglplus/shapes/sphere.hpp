/**
 *  @file oglplus/shapes/sphere.hpp
 *  @brief UV-Sphere builder
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_SPHERE_1107121519_HPP
#define OGLPLUS_SHAPES_SPHERE_1107121519_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/constants.hpp>
#include <oglplus/math/sphere.hpp>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for rendering of a Sphere
class Sphere
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	double _radius;
	unsigned _sections, _rings;
public:
	/// Creates a sphere with unit radius centered at the origin
	Sphere(void)
	 : _radius(1.0)
	 , _sections(18)
	 , _rings(12)
	{ }

	/// Creates a sphere with specified radius centered at the origin
	/**
	 *  @pre (radius > 0) && (sections > 0) && (rings > 0)
	 */
	Sphere(double radius, unsigned sections, unsigned rings)
	 : _radius(radius)
	 , _sections(sections)
	 , _rings(rings)
	{
		assert(_radius > 0);
		assert(_sections > 0);
		assert(_rings > 0);
	}

	/// Returns the radius of the sphere
	double Radius(void) const
	{
		return _radius;
	}

	/// Sets the radius of the sphere
	/**
	 *  @pre radius > 0
	 */
	void Radius(double radius)
	{
		_radius = radius;
		assert(_radius > 0);
	}

	/// Returns the number of sections of the sphere
	unsigned Sections(void) const
	{
		return _sections;
	}

	/// Sets the number of sections of the sphere
	/**
	 *  @pre sections > 0
	 */
	void Sections(unsigned sections)
	{
		_sections = sections;
		assert(_sections > 0);
	}

	/// Returns the number of rings of the sphere
	unsigned Rings(void) const
	{
		return _rings;
	}

	/// Sets the number of rings of the sphere
	/**
	 *  @pre rings > 0
	 */
	void Rings(unsigned rings)
	{
		_rings = rings;
		assert(_rings > 0);
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CCW;
	}

	/// Makes vertex normals and returns number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		dest.resize(((_rings + 2) * (_sections + 1)) * 3);
		unsigned k = 0;
		//
		double r_step = (1.0 * math::Pi()) / double(_rings + 1);
		double s_step = (2.0 * math::Pi()) / double(_sections);

		for(unsigned r=0; r!=(_rings+2);++r)
		{
			double r_lat = std::cos(r*r_step);
			double r_rad = std::sin(r*r_step);
			// the sections
			for(unsigned s=0; s!=(_sections+1);++s)
			{
				dest[k++] = T(r_rad *  std::cos(s*s_step));
				dest[k++] = T(r_lat);
				dest[k++] = T(r_rad * -std::sin(s*s_step));
			}
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
		dest.resize(((_rings + 2) * (_sections + 1)) * 3);
		unsigned k = 0;
		//
		double s_step = (2.0 * math::Pi()) / double(_sections);

		for(unsigned r=0; r!=(_rings+2);++r)
		{
			for(unsigned s=0; s!=(_sections+1);++s)
			{
				dest[k++] = T(-std::sin(s*s_step));
				dest[k++] = T(0);
				dest[k++] = T(-std::cos(s*s_step));
			}
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
		dest.resize(((_rings + 2) * (_sections + 1)) * 3);
		unsigned k = 0;
		//
		double r_step = (1.0 * math::Pi()) / double(_rings + 1);
		double s_step = (2.0 * math::Pi()) / double(_sections);

		double ty = 0.0;
		for(unsigned r=0; r!=(_rings+2);++r)
		{
			double r_lat = std::cos(r*r_step);
			double r_rad = std::sin(r*r_step);
			double ny = r_lat;
			// the sections
			for(unsigned s=0; s!=(_sections+1);++s)
			{
				double tx = -std::sin(s*s_step);
				double tz = -std::cos(s*s_step);
				double nx = -r_rad * tz;
				double nz =  r_rad * tx;

				dest[k++] = T(ny*tz-nz*ty);
				dest[k++] = T(nz*tx-nx*tz);
				dest[k++] = T(nx*ty-ny*tx);
			}
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
		GLuint n = Normals(dest);
		if(_radius != 1.0)
			for(auto i=dest.begin(),e=dest.end(); i!= e; ++i)
				*i *= _radius;
		return n;
	}

	/// Makes texture-coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		dest.resize(((_rings + 2) * (_sections + 1)) * 2);
		unsigned k = 0;
		//
		double r_step = 1.0 / double(_rings + 1);
		double s_step = 1.0 / double(_sections);
		for(unsigned r=0; r!=(_rings+2);++r)
		{
			double r_lat = 1.0 - r*r_step;
			// the sections
			for(unsigned s=0; s!=(_sections+1);++s)
			{
				dest[k++] = T(s * s_step);
				dest[k++] = T(r_lat);
			}
		}
		assert(k == dest.size());
		// 2 values per vertex
		return 2;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Sphere provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangents" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Sphere> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Sphere,
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
			T(_radius)
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
#include <oglplus/shapes/sphere.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
