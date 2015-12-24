/**
 *  @file oglplus/shapes/analyzer.hpp
 *  @brief Class that analyzes mesh/shape properties for further processing
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_ANALYZER_1308151449_HPP
#define OGLPLUS_SHAPES_ANALYZER_1308151449_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/math/vector.hpp>

#include <oglplus/shapes/analyzer_data.hpp>

#include <cstddef>

namespace oglplus {
namespace shapes {

class ShapeAnalyzerVert;
class ShapeAnalyzerEdge;
class ShapeAnalyzerFace;

/// Class storing information about a single vertex of a generated/loaded mesh
/**
 *  @see ShapeAnalyzer
 *  @see ShapeAnalyzerEdge
 *  @see ShapeAnalyzerFace
 */
class ShapeAnalyzerVert
{
private:
	const ShapeAnalyzerGraphData& _data;
	GLuint _face_index;
	GLuint _vert_index;

	friend class ShapeAnalyzer;
	friend class ShapeAnalyzerEdge;
	friend class ShapeAnalyzerFace;

	ShapeAnalyzerVert(
		const ShapeAnalyzerGraphData& data,
		GLuint face_index,
		GLuint vert_index
	): _data(data)
	 , _face_index(face_index)
	 , _vert_index(vert_index)
	{
		assert(_face_index < data._face_index.size());
		assert(_vert_index < data._face_arity(_face_index));
	}
public:
	/// Returns the index of the vertex in its parent face
	/**
	 *  @post Index() < Face().Arity()
	 */
	GLuint Index(void) const
	{
		return _vert_index;
	}

	/// Returns the parent face of the vertex
	ShapeAnalyzerFace Face(void) const;

	/// Returns the value of the main vertex attribute at the vertex
	/** The main vertex attribute is usually the vertex position, but
	 *  for some analyses it may be something else, the UV coordinate
	 *  for example.
	 */
	Vec4d MainAttrib(void) const;

	/// Returns the value of the 'smooth' vertex attribute at the vertex
	/** This vertex attribute is used to detect if edges are 'smooth'
	 *  it is usually the vertex normal, but for some analyses it may be
	 *  something else.
	 */
	Vec4d SmoothAttrib(void) const;
};

/// Class storing information about a single edge of a generated/loaded mesh
/**
 *  @see ShapeAnalyzer
 *  @see ShapeAnalyzerVert
 *  @see ShapeAnalyzerFace
 */
class ShapeAnalyzerEdge
{
private:
	const ShapeAnalyzerGraphData& _data;
	GLuint _face_index;
	GLuint _edge_index;

	friend class ShapeAnalyzer;
	friend class ShapeAnalyzerFace;

	ShapeAnalyzerEdge(
		const ShapeAnalyzerGraphData& data,
		GLuint face_index,
		GLuint edge_index
	): _data(data)
	 , _face_index(face_index)
	 , _edge_index(edge_index)
	{
		assert(_face_index < data._face_index.size());
		assert(_edge_index < data._face_arity(_face_index));
	}
public:
	/// Returns the index of the edge in its parent face
	/**
	 *  @post Index() < Face().Arity()
	 */
	GLuint Index(void) const
	{
		return _edge_index;
	}

	/// Returns the parent face of the edge
	ShapeAnalyzerFace Face(void) const;

	/// Returns true if this edge has an adjacent edge (and adjacent face)
	bool HasAdjacentEdge(void) const;

	/// Returns the adjacent edge to this edge
	/**
	 *  @pre HasAdjacentEdge()
	 */
	ShapeAnalyzerEdge AdjacentEdge(void) const;

	/// Returns true if this edge has an opposite vertex (triangles only)
	bool HasOppositeVert(void) const;

	/// Returns the opposite vertex (if any)
	/**
	 *  @pre HasOppositeVert()
	 */
	ShapeAnalyzerVert OppositeVert(void) const;

	bool HasFlag(GLuint flag) const;

	/// Returns true if the edge is continuous
	bool IsContinuousEdge(void) const
	{
		return HasFlag(ShapeAnalyzerGraphData::_flg_contin_edge);
	}

	/// Returns true if the edge is smooth
	bool IsSmoothEdge(void) const
	{
		return HasFlag(ShapeAnalyzerGraphData::_flg_smooth_edge);
	}

	/// Returns true if the edge is a connecting edge of a triangle strip
	bool IsStripEdge(void) const
	{
		return HasFlag(ShapeAnalyzerGraphData::_flg_strip_edge);
	}

	/// Returns true if the edge is a connecting edge of a triangle fan
	bool IsFanEdge(void) const
	{
		return HasFlag(ShapeAnalyzerGraphData::_flg_fan_edge);
	}
};


/// Class storing information about a single face of a generated/loaded mesh
/**
 *  @see ShapeAnalyzer
 *  @see ShapeAnalyzerVert
 *  @see ShapeAnalyzerEdge
 */
class ShapeAnalyzerFace
{
private:
	const ShapeAnalyzerGraphData& _data;
	GLuint _index;

	friend class ShapeAnalyzer;
	friend class ShapeAnalyzerVert;
	friend class ShapeAnalyzerEdge;

	ShapeAnalyzerFace(const ShapeAnalyzerGraphData& data, GLuint index)
	 : _data(data)
	 , _index(index)
	{
		assert(_index < data._face_index.size());
	}
public:
	/// Returns the index of the face in the mesh that it belongs to
	GLuint Index(void) const
	{
		return _index;
	}

	/// The number of edges (or vertices) of this face
	/**
	 *  @see Vert()
	 *  @see Edge()
	 *  @see AdjacentFace()
	 */
	GLuint Arity(void) const
	{
		return _data._face_arity(_index);
	}

	/// Returns the vert_index-th vertex of this face
	/**
	 *  @pre vert_index < Arity()
	 *  @see Arity()
	 *  @see Edge()
	 *  @see AdjacentFace()
	 */
	ShapeAnalyzerVert Vert(GLuint vert_index) const;

	/// Returns the edge_index-th edge of this face
	/**
	 *  @pre edge_index < Arity()
	 *  @see Arity()
	 *  @see Vert()
	 *  @see AdjacentFace()
	 */
	ShapeAnalyzerEdge Edge(GLuint edge_index) const;

	/// Returns true if the edge_index-th edge has an adjacent face
	/**
	 *  @pre edge_index < Arity()
	 *  @see Arity()
	 *  @see Vert()
	 *  @see AdjacentFace()
	 */
	bool HasAdjacentFace(GLuint edge_index) const;

	/// Returns the edge_index-th adjacent face
	/**
	 *  @pre (edge_index < Arity()) && (HasAdjacentFace(edge_index))
	 *  @see Arity()
	 *  @see Vert()
	 *  @see AdjacentFace()
	 */
	ShapeAnalyzerFace AdjacentFace(GLuint edge_index) const;
};

/// Class that analyzes vertex attribs generated by shape loaders/generators
/**
 *  @see ShapeAnalyzerVert
 *  @see ShapeAnalyzerEdge
 *  @see ShapeAnalyzerFace
 */
class ShapeAnalyzer
{
private:
	ShapeAnalyzerGraphData _data;
public:
	/// Constructor takes an initialized shape builder
	template <typename ShapeBuilder>
	ShapeAnalyzer(const ShapeBuilder& builder)
	 : _data(builder)
	{ }

	/// The class storing information about a single mesh vertex
	typedef ShapeAnalyzerVert ShapeVert;

	/// The class storing information about a single mesh edge
	typedef ShapeAnalyzerEdge ShapeEdge;

	/// The class storing information about a single mesh face
	typedef ShapeAnalyzerFace ShapeFace;

	/// Return the number of faces generated by the generator
	/**
	 *  @see Face()
	 */
	GLuint FaceCount(void) const
	{
		assert(!_data._face_index.empty());
		return GLuint(_data._face_index.size());
	}

	/// Returns the face_index-th face
	/**
	 *  @pre face_index < FaceCount()
	 *  @see FaceCount()
	 */
	ShapeFace Face(GLuint face_index)
	{
		assert(face_index<_data._face_index.size());
		return ShapeFace(_data, face_index);
	}
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/analyzer.ipp>
#endif

#endif // include guard
