/**
 *  @file oglplus/shapes/analyzer_data.hpp
 *  @brief Storage of analyzed mesh/shape properties used for further processing
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_ANALYZER_GRAPH_1308151449_HPP
#define OGLPLUS_SHAPES_ANALYZER_GRAPH_1308151449_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/shapes/draw.hpp>

#include <vector>

namespace oglplus {
namespace shapes {

class ShapeAnalyzerGraphData
{
private:
	template <typename Index>
	static std::vector<GLuint> _adapt(const Index& index)
	{
		return std::vector<GLuint>(
			index.begin(),
			index.end()
		);
	}

	static std::vector<GLuint>&& _adapt(std::vector<GLuint>&& index)
	{
		return static_cast<std::vector<GLuint>&&>(index);
	}

	GLuint _guess_face_count(void);
	GLuint _guess_vertex_count(GLuint);

	void _initialize(void);

	void _init_draw_arrays(const DrawOperation& draw_op);

	void _init_dr_ar_triangles(const DrawOperation& draw_op);
	void _init_dr_ar_triangle_strip(const DrawOperation& draw_op);
	void _init_dr_ar_triangle_fan(const DrawOperation& draw_op);

	void _init_draw_elements(const DrawOperation& draw_op);

	void _init_dr_el_triangles(const DrawOperation& draw_op);
	void _init_dr_el_triangle_strip(const DrawOperation& draw_op);
	void _init_dr_el_triangle_fan(const DrawOperation& draw_op);

	void _detect_adjacent(void);
	bool _same_va_values(
		GLuint fa,
		GLuint ea,
		GLuint fb,
		GLuint eb,
		GLuint attr_vpv,
		const std::vector<GLdouble>& vert_attr
	);
	bool _adjacent_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb);
	bool _smooth_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb);
	bool _contin_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb);
public:
	DrawingInstructions _instr;
	std::vector<GLuint> _index;

	// main vertex attribute (usually position)
	std::vector<GLdouble> _main_va;
	// number of values per vertex for the main attribute
	GLuint _main_vpv;

	// vertex attribute used to detect smoothing (usually normal)
	std::vector<GLdouble> _smooth_va;
	// number of values per vertex for the smooting attribute
	GLuint _smooth_vpv;

	// other vertex attributes
	std::vector<std::vector<GLdouble> > _other_vas;
	// number of values per vertex for the other attributes
	std::vector<GLuint> _other_vpvs;

	// epsilon value for comparisons
	GLdouble _eps;

	// index pointing to the start of face vertices
	std::vector<GLuint> _face_index;
	std::vector<GLuint> _face_phase;

	std::vector<GLuint> _face_verts;
	std::vector<GLuint> _face_adj_f;
	std::vector<GLuint> _face_adj_e;
	std::vector<GLuint> _face_edge_flags;

	static const GLuint _flg_contin_edge = 0x0001;
	static const GLuint _flg_smooth_edge = 0x0002;
	static const GLuint _flg_strip_edge =
		_flg_smooth_edge|
		_flg_contin_edge|
		0x0004;
	static const GLuint _flg_fan_edge =
		_flg_smooth_edge|
		_flg_contin_edge|
		0x0008;

	static GLuint _face_arity(GLuint /*face*/)
	{
		// TODO Arity or triangles only?
		return 3;
	}

	typedef oglplus::ShapeDrawOperationMethod Method;
	typedef oglplus::PrimitiveType Mode;

	template <typename ShapeBuilder>
	ShapeAnalyzerGraphData(const ShapeBuilder& builder)
	 : _instr(builder.Instructions())
	 , _index(_adapt(builder.Indices()))
	 , _main_va()
	 , _main_vpv(builder.Positions(_main_va))
	 , _smooth_va()
	 , _smooth_vpv(builder.Normals(_smooth_va))
	 , _eps(1.0e-9)
	{
		_initialize();
	}

	static  GLuint _nil_face(void) { return ~GLuint(0); }
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/analyzer_data.ipp>
#endif

#endif // include guard
