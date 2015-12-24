/**
 *  @file oglplus/shapes/analyzer_graph.ipp
 *  @brief Implementation of shapes::ShapeAnalyzerGraphData
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/assert.hpp>
#include <cassert>
#include <cmath>

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_ar_triangles(const DrawOperation& draw_op)
{
	assert(draw_op.count % 3 == 0);

	GLuint i=0;
	while(i != draw_op.count)
	{
		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);

		_face_verts.push_back(draw_op.first+i++);
		_face_verts.push_back(draw_op.first+i++);
		_face_verts.push_back(draw_op.first+i++);

		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());

		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);

		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_ar_triangle_strip(const DrawOperation& draw_op)
{
	assert((draw_op.count == 0) || (draw_op.count >= 3));
	GLuint i=0;
	GLuint v=0;

	_face_index.push_back(GLuint(_face_verts.size()));
	_face_phase.push_back(draw_op.phase);

	_face_verts.push_back(draw_op.first+i++);
	_face_verts.push_back(draw_op.first+i++);
	_face_verts.push_back(draw_op.first+i++);

	_face_adj_f.push_back(_nil_face());
	_face_adj_f.push_back(_nil_face());
	_face_adj_f.push_back(_nil_face());

	_face_adj_e.push_back(0);
	_face_adj_e.push_back(0);
	_face_adj_e.push_back(0);

	_face_edge_flags.push_back(0);
	_face_edge_flags.push_back(0);
	_face_edge_flags.push_back(0);

	v+=3;

	while(i != draw_op.count)
	{
		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);
		if(i % 2 != 0)
		{
			_face_verts.push_back(draw_op.first+i-1);
			_face_verts.push_back(draw_op.first+i-2);
			_face_verts.push_back(draw_op.first+i+0);

			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());

			_face_adj_f[v-2] = GLuint(_face_index.size()-1);
			_face_adj_f[v+0] = GLuint(_face_index.size()-2);

			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);

			_face_adj_e[v-2] = 0;
			_face_adj_e[v+0] = 1;

			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);

			_face_edge_flags[v-2] |= _flg_strip_edge;
			_face_edge_flags[v+0] |= _flg_strip_edge;
		}
		else
		{
			_face_verts.push_back(draw_op.first+i-2);
			_face_verts.push_back(draw_op.first+i-1);
			_face_verts.push_back(draw_op.first+i+0);

			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());

			_face_adj_f[v-1] = GLuint(_face_index.size()-1);
			_face_adj_f[v+0] = GLuint(_face_index.size()-1);

			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);

			_face_adj_e[v-1] = 0;
			_face_adj_e[v+0] = 2;

			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);

			_face_edge_flags[v-1] |= _flg_strip_edge;
			_face_edge_flags[v+0] |= _flg_strip_edge;
		}
		++i;
		v+=3;
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_ar_triangle_fan(const DrawOperation& draw_op)
{
	assert((draw_op.count == 0) || (draw_op.count >= 3));
	GLuint i=0;
	GLuint v=0;

	_face_index.push_back(GLuint(_face_verts.size()));
	_face_phase.push_back(draw_op.phase);

	_face_verts.push_back(draw_op.first+i++);
	_face_verts.push_back(draw_op.first+i++);
	_face_verts.push_back(draw_op.first+i++);

	_face_adj_f.push_back(_nil_face());
	_face_adj_f.push_back(_nil_face());
	_face_adj_f.push_back(_nil_face());

	_face_adj_e.push_back(0);
	_face_adj_e.push_back(0);
	_face_adj_e.push_back(0);

	_face_edge_flags.push_back(0);
	_face_edge_flags.push_back(0);
	_face_edge_flags.push_back(0);

	v+=3;

	while(i != draw_op.count)
	{
		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);

		_face_verts.push_back(draw_op.first);
		_face_verts.push_back(draw_op.first+i-1);
		_face_verts.push_back(draw_op.first+i+0);

		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());

		_face_adj_f[v-1] = GLuint(_face_index.size()-1);
		_face_adj_f[v+0] = GLuint(_face_index.size()-2);

		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);

		_face_adj_e[v-1] = 0;
		_face_adj_e[v+0] = 2;

		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);

		_face_edge_flags[v-1] = _flg_fan_edge;
		_face_edge_flags[v+0] = _flg_fan_edge;

		++i;
		v+=3;
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::_init_draw_arrays(const DrawOperation& draw_op)
{
	switch(draw_op.mode)
	{
		case Mode::Triangles:
			_init_dr_ar_triangles(draw_op);
			break;
		case Mode::TriangleStrip:
			_init_dr_ar_triangle_strip(draw_op);
			break;
		case Mode::TriangleFan:
			_init_dr_ar_triangle_fan(draw_op);
			break;
		default: OGLPLUS_ABORT(
			"Only Triangles, TriangleStrip and "
			"TriangleFan are currently supported"
		);
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_el_triangles(const DrawOperation& draw_op)
{
	assert(draw_op.count % 3 == 0);

	GLuint i=0;
	while(i != draw_op.count)
	{
		if(_index[draw_op.first+i] == draw_op.restart_index)
		{
			++i;
			continue;
		}

		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);

		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);

		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());

		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);

		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_el_triangle_strip(const DrawOperation& draw_op)
{
	assert((draw_op.count == 0) || (draw_op.count >= 3));
	GLuint i=0;
	GLuint v=0;

	while(i != draw_op.count)
	{
		if(_index[draw_op.first+i] == draw_op.restart_index)
		{
			++i;
			continue;
		}

		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);

		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);

		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());

		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);

		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);

		v+=3;

		while(i != draw_op.count)
		{
			if(_index[draw_op.first+i] == draw_op.restart_index)
			{
				++i;
				break;
			}

			_face_index.push_back(GLuint(_face_verts.size()));
			_face_phase.push_back(draw_op.phase);

			if(i % 2 != 0)
			{
				_face_verts.push_back(_index[draw_op.first+i-1]);
				_face_verts.push_back(_index[draw_op.first+i-2]);
				_face_verts.push_back(_index[draw_op.first+i+0]);

				_face_adj_f.push_back(_nil_face());
				_face_adj_f.push_back(_nil_face());
				_face_adj_f.push_back(_nil_face());

				_face_adj_f[v-2] = GLuint(_face_index.size()-1);
				_face_adj_f[v+0] = GLuint(_face_index.size()-2);

				_face_adj_e.push_back(0);
				_face_adj_e.push_back(0);
				_face_adj_e.push_back(0);

				_face_adj_e[v-2] = 0;
				_face_adj_e[v+0] = 1;

				_face_edge_flags.push_back(0);
				_face_edge_flags.push_back(0);
				_face_edge_flags.push_back(0);

				_face_edge_flags[v-2] = _flg_strip_edge;
				_face_edge_flags[v+0] = _flg_strip_edge;
			}
			else
			{
				_face_verts.push_back(_index[draw_op.first+i-2]);
				_face_verts.push_back(_index[draw_op.first+i-1]);
				_face_verts.push_back(_index[draw_op.first+i+0]);

				_face_adj_f.push_back(_nil_face());
				_face_adj_f.push_back(_nil_face());
				_face_adj_f.push_back(_nil_face());

				_face_adj_f[v-1] = GLuint(_face_index.size()-1);
				_face_adj_f[v+0] = GLuint(_face_index.size()-2);

				_face_adj_e.push_back(0);
				_face_adj_e.push_back(0);
				_face_adj_e.push_back(0);

				_face_adj_e[v-1] = 0;
				_face_adj_e[v+0] = 2;

				_face_edge_flags.push_back(0);
				_face_edge_flags.push_back(0);
				_face_edge_flags.push_back(0);

				_face_edge_flags[v-1] = _flg_strip_edge;
				_face_edge_flags[v+0] = _flg_strip_edge;
			}
			++i;
			v+=3;
		}
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::
_init_dr_el_triangle_fan(const DrawOperation& draw_op)
{
	assert((draw_op.count == 0) || (draw_op.count >= 3));
	GLuint i=0;
	GLuint v=0;

	while(i != draw_op.count)
	{
		if(_index[draw_op.first+i] == draw_op.restart_index)
		{
			++i;
			continue;
		}

		_face_index.push_back(GLuint(_face_verts.size()));
		_face_phase.push_back(draw_op.phase);

		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);
		_face_verts.push_back(_index[draw_op.first+i++]);

		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());
		_face_adj_f.push_back(_nil_face());

		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);
		_face_adj_e.push_back(0);

		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);
		_face_edge_flags.push_back(0);

		v+=3;

		while(i != draw_op.count)
		{
			if(_index[draw_op.first+i] == draw_op.restart_index)
			{
				++i;
				break;
			}

			_face_index.push_back(GLuint(_face_verts.size()));
			_face_phase.push_back(draw_op.phase);

			_face_verts.push_back(_index[draw_op.first+i]);
			_face_verts.push_back(_index[draw_op.first+i-1]);
			_face_verts.push_back(_index[draw_op.first+i+0]);

			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());
			_face_adj_f.push_back(_nil_face());

			_face_adj_f[v-1] = GLuint(_face_index.size()-1);
			_face_adj_f[v+0] = GLuint(_face_index.size()-2);

			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);
			_face_adj_e.push_back(0);

			_face_adj_e[v-1] = 0;
			_face_adj_e[v+0] = 2;

			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);
			_face_edge_flags.push_back(0);

			_face_edge_flags[v-1] = _flg_fan_edge;
			_face_edge_flags[v+0] = _flg_fan_edge;

			++i;
			v+=3;
		}
	}
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::_init_draw_elements(const DrawOperation& draw_op)
{
	switch(draw_op.mode)
	{
		case Mode::Triangles:
			_init_dr_el_triangles(draw_op);
			break;
		case Mode::TriangleStrip:
			_init_dr_el_triangle_strip(draw_op);
			break;
		case Mode::TriangleFan:
			_init_dr_el_triangle_fan(draw_op);
			break;
		default: OGLPLUS_ABORT(
			"Only Triangles, TriangleStrip and "
			"TriangleFan are currently supported"
		);
	}
}

OGLPLUS_LIB_FUNC
GLuint ShapeAnalyzerGraphData::_guess_face_count(void)
{
	const std::vector<DrawOperation>& draw_ops = _instr.Operations();
	GLuint result = 0;
	for(auto i=draw_ops.begin(), e=draw_ops.end(); i!=e; ++i)
	{
		switch(i->mode)
		{
			case Mode::Triangles:
			{
				assert(i->count % 3 == 0);
				result += i->count / 3;
				break;
			}
			case Mode::TriangleStrip:
			{
				assert((i->count == 0) || (i->count >= 3));
				if(i->count) result += 1+i->count-3;
				break;
			}
			case Mode::TriangleFan:
			{
				assert((i->count == 0) || (i->count >= 3));
				if(i->count) result += 1+i->count-3;
				break;
			}
			default: OGLPLUS_ABORT(
				"Only Triangles, TriangleStrip and "
				"TriangleFan are currently supported"
			);
		}
	}
	return result;
}

OGLPLUS_LIB_FUNC
GLuint ShapeAnalyzerGraphData::_guess_vertex_count(GLuint face_count)
{
	return face_count * 3; // TODO n-gons or just triangles?
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::_initialize(void)
{
	const std::vector<DrawOperation>& draw_ops = _instr.Operations();

	GLuint fc = _guess_face_count();
	GLuint vc = _guess_vertex_count(fc);

	_face_index.reserve(fc);
	_face_phase.reserve(fc);

	_face_verts.reserve(vc);
	_face_adj_f.reserve(vc);
	_face_adj_e.reserve(vc);
	_face_edge_flags.reserve(vc);

	for(auto i=draw_ops.begin(), e=draw_ops.end(); i!=e; ++i)
	{
		switch(i->method)
		{
			case Method::DrawArrays:
				_init_draw_arrays(*i);
				break;
			case Method::DrawElements:
				_init_draw_elements(*i);
				break;
		}
	}

	_detect_adjacent();
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerGraphData::_same_va_values(
	GLuint fa,
	GLuint ea,
	GLuint fb,
	GLuint eb,
	GLuint attr_vpv,
	const std::vector<GLdouble>& vert_attr
)
{
	GLuint va0 = _face_verts[_face_index[fa]+ea];
	GLuint va1 = _face_verts[_face_index[fa]+(ea+1)%_face_arity(fa)];

	GLuint vb0 = _face_verts[_face_index[fb]+eb];
	GLuint vb1 = _face_verts[_face_index[fb]+(eb+1)%_face_arity(fb)];

	if((va0 == vb0) && (va1 == vb1)) return true;
	if((va0 == vb1) && (va1 == vb0)) return true;

	va0 *= attr_vpv;
	va1 *= attr_vpv;
	vb0 *= attr_vpv;
	vb1 *= attr_vpv;

	bool equal = true;
	for(GLuint c=0; c!=attr_vpv; ++c)
	{
		if(std::fabs(vert_attr[va0+c] - vert_attr[vb0+c]) > _eps)
		{
			equal = false;
			break;
		}
		if(std::fabs(vert_attr[va1+c] - vert_attr[vb1+c]) > _eps)
		{
			equal = false;
			break;
		}
	}
	if(equal) return true;
	else equal = true;

	for(GLuint c=0; c!=attr_vpv; ++c)
	{
		if(std::fabs(vert_attr[va0+c] - vert_attr[vb1+c]) > _eps)
		{
			equal = false;
			break;
		}
		if(std::fabs(vert_attr[va1+c] - vert_attr[vb0+c]) > _eps)
		{
			equal = false;
			break;
		}
	}
	if(equal) return true;

	return false;
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerGraphData::
_adjacent_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb)
{
	return _same_va_values(fa, ea, fb, eb, _main_vpv, _main_va);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerGraphData::
_smooth_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb)
{
	return _same_va_values(fa, ea, fb, eb, _smooth_vpv, _smooth_va);
}

OGLPLUS_LIB_FUNC
bool ShapeAnalyzerGraphData::
_contin_faces(GLuint fa, GLuint ea, GLuint fb, GLuint eb)
{
	std::size_t n = _other_vas.size();
	assert(n == _other_vpvs.size());
	bool result = true;
	for(std::size_t i=0; i!=n; ++i)
	{
		result |= _same_va_values(
			fa, ea, fb, eb,
			_other_vpvs[i],
			_other_vas[i]
		);
	}
	return result;
}

OGLPLUS_LIB_FUNC
void ShapeAnalyzerGraphData::_detect_adjacent(void)
{
	const auto fb=_face_index.begin();
	const auto fe=_face_index.end();

	assert(fb != fe);

	auto fi=fb;
	// for each face
	while(fi != fe)
	{
		GLuint fifb = GLuint(fi-fb);
		GLuint fien = _face_arity(fifb);
		for(GLuint fie=0; fie!=fien; ++fie)
		{
			GLuint i=*fi+fie;
			if(_face_adj_f[i] == _nil_face())
			{
				auto fj = fi+1;
				while(fj != fe)
				{
					GLuint fjfb = GLuint(fj-fb);
					GLuint fjen = _face_arity(fjfb);
					for(GLuint fje=0; fje!=fjen; ++fje)
					{
						GLuint j=*fj+fje;
						bool nadj = (
							_face_adj_f[j] ==
							_nil_face()
						);
						bool adjf = _adjacent_faces(
							fifb,
							fie,
							fjfb,
							fje
						);
						bool smtf = adjf&&_smooth_faces(
							fifb,
							fie,
							fjfb,
							fje
						);
						bool cntf = adjf&&_contin_faces(
							fifb,
							fie,
							fjfb,
							fje
						);
						if(nadj && adjf)
						{
							_face_adj_f[i]=fjfb;
							_face_adj_f[j]=fifb;

							_face_adj_e[i]=fje;
							_face_adj_e[j]=fie;
						}
						if(nadj && smtf)
						{
							_face_edge_flags[i] |=
								_flg_smooth_edge;
							_face_edge_flags[j] |=
								_flg_smooth_edge;
						}
						if(nadj && cntf)
						{
							_face_edge_flags[i] |=
								_flg_contin_edge;
							_face_edge_flags[j] |=
								_flg_contin_edge;
						}
					}
					++fj;
				}
			}
		}
		++fi;
	}
}

} // shapes
} // oglplus
