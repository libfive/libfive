/**
 *  @file oglplus/shapes/tetrahedrons.hpp
 *  @brief Special shape builder used in implicit surface polygonisation
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_TETRAHEDRONS_1201311347_HPP
#define OGLPLUS_SHAPES_TETRAHEDRONS_1201311347_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/sphere.hpp>
#include <cmath>
#include <cassert>

namespace oglplus {
namespace shapes {

/// Provides data and instructions for rendering of cube filling tetrahedrons
class Tetrahedrons
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	GLdouble _side;
	unsigned _divisions;
public:
	/// Makes a unit sized tetrahedra filled cube with 10 divisions
	Tetrahedrons(void)
	 : _side(1.0)
	 , _divisions(10)
	{ }

	/// Makes a cube with the specified @p side and number of @p divisions
	Tetrahedrons(GLdouble side, unsigned divisions)
	 : _side(side)
	 , _divisions(divisions)
	{
		assert(_side > 0.0);
		assert(divisions > 0);
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	/// Makes vertex coordinates and returns number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		const unsigned n = _divisions + 1;
		unsigned k = 0;
		dest.resize(n*n*n*3+3);

		dest[k++] = T(0);
		dest[k++] = T(0);
		dest[k++] = T(0);

		GLdouble step = _side / _divisions;
		//
		for(unsigned z=0; z!=n; ++z)
		for(unsigned y=0; y!=n; ++y)
		for(unsigned x=0; x!=n; ++x)
		{
			dest[k++] = T(x*step);
			dest[k++] = T(y*step);
			dest[k++] = T(z*step);
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes texture coorinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		const unsigned n = _divisions + 1;
		unsigned k = 0;
		dest.resize(n*n*n*3+3);

		dest[k++] = T(0);
		dest[k++] = T(0);
		dest[k++] = T(0);

		GLdouble step = 1.0 / _divisions;
		//
		for(unsigned z=0; z!=n; ++z)
		for(unsigned y=0; y!=n; ++y)
		for(unsigned x=0; x!=n; ++x)
		{
			dest[k++] = T(x*step);
			dest[k++] = T(y*step);
			dest[k++] = T(z*step);
		}
		assert(k == dest.size());
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Tetrahedrons provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "TexCoord" the STR texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<Tetrahedrons> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		Tetrahedrons,
		std::tuple<
			VertexPositionsTag,
			VertexTexCoordinatesTag
		>
	> VertexAttribs;
#endif

	// This is here just for consistency with Shape wrapper
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>&) const
	{
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(WithAdjacency = WithAdjacency()) const;

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(WithAdjacency = WithAdjacency()) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/tetrahedrons.ipp>
#endif //OGLPLUS_LINK_LIBRARY

#endif // include guard
