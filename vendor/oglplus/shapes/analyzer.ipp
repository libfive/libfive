/**
 *  @file oglplus/shapes/analyzer.ipp
 *  @brief Implementation of shapes::ShapeAnalyzer
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
ShapeAnalyzerFace ShapeAnalyzerVert::Face(void) const
{
	return ShapeAnalyzerFace(_data, _face_index);
}

OGLPLUS_LIB_FUNC
Vec4d ShapeAnalyzerVert::MainAttrib(void) const
{
	GLuint fi = _data._face_index[_face_index];
	GLuint vi = _data._face_verts[fi+_vert_index];
	return Vec4d(
		_data._main_va.data()+vi*_data._main_vpv,
		_data._main_vpv,
		0.0
	);
}

OGLPLUS_LIB_FUNC
Vec4d ShapeAnalyzerVert::SmoothAttrib(void) const
{
	GLuint fi = _data._face_index[_face_index];
	GLuint vi = _data._face_verts[fi+_vert_index];
	return Vec4d(
		_data._smooth_va.data()+vi*_data._smooth_vpv,
		_data._smooth_vpv,
		0.0
	);
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerFace ShapeAnalyzerEdge::Face(void) const
{
	return ShapeAnalyzerFace(_data, _face_index);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerEdge::HasAdjacentEdge(void) const
{
	GLuint i = _data._face_index[_face_index]+_edge_index;
	return _data._face_adj_f[i] != _data._nil_face();
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerEdge ShapeAnalyzerEdge::AdjacentEdge(void) const
{
	GLuint i = _data._face_index[_face_index]+_edge_index;
	return ShapeAnalyzerEdge(
		_data,
		_data._face_adj_f[i],
		_data._face_adj_e[i]
	);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerEdge::HasOppositeVert(void) const
{
	return _data._face_arity(_face_index) == 3;
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerVert ShapeAnalyzerEdge::OppositeVert(void) const
{
	return ShapeAnalyzerVert(
		_data,
		_face_index,
		(_edge_index+2)%3
	);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerEdge::HasFlag(GLuint flag) const
{
	GLuint i = _data._face_index[_face_index]+_edge_index;
	return (_data._face_edge_flags[i] & flag) == flag;
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerVert ShapeAnalyzerFace::Vert(GLuint vert_index) const
{
	return ShapeAnalyzerVert(
		_data,
		_index,
		vert_index
	);
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerEdge ShapeAnalyzerFace::Edge(GLuint edge_index) const
{
	return ShapeAnalyzerEdge(
		_data,
		_index,
		edge_index
	);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerFace::HasAdjacentFace(GLuint edge_index) const
{
	return _data._face_adj_f[
		_data._face_index[_index]+
		edge_index
	] != _data._nil_face();
}

OGLPLUS_LIB_FUNC
ShapeAnalyzerFace ShapeAnalyzerFace::AdjacentFace(GLuint edge_index) const
{
	return ShapeAnalyzerFace(
		_data,
		_data._face_adj_f[
			_data._face_index[_index]+
			edge_index
		]
	);
}

} // shapes
} // oglplus

