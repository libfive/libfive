/**
 *  @file oglplus/shapes/spiral_sphere.ipp
 *  @brief implementation of shapes::SpiralSphere
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

unsigned SpiralSphere::_vertex_count(void) const
{
	return	(_bands * 2)*
		(_divisions + 1)*
		(_segments + 1)+
		(_bands * 2)*
		(_segments + 1);
}

template <typename T>
void SpiralSphere::_make_vectors(
	std::vector<T>& dest,
	unsigned& k,
	double sign,
	double radius
) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double b_step = b_leap / double(_divisions);
	double s_step = (math::Pi()) / double(_segments);

	double m = sign * radius;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned d=0; d!=(_divisions+1); ++d)
		{
			double b_offs = 0.0;
			for(unsigned s=0; s!=(_segments+1); ++s)
			{
				double b_angle =
					2*b*b_leap + d*b_step + b_offs;
				double cb = std::cos(b_angle);
				double sb = std::sin(b_angle);

				double s_angle = s*s_step;
				double cs = std::cos(s_angle);
				double ss = std::sin(s_angle);

				dest[k++] = T(m* ss * cb);
				dest[k++] = T(m* cs);
				dest[k++] = T(m* ss *-sb);
				b_offs += ss * s_step;
			}
		}
	}
}

template <typename T>
void SpiralSphere::_make_tangents(
	std::vector<T>& dest,
	unsigned& k,
	double sign
) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double b_step = b_leap / double(_divisions);
	double s_step = (math::Pi()) / double(_segments);

	double m = sign;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned d=0; d!=(_divisions+1); ++d)
		{
			double b_offs = 0.0;
			for(unsigned s=0; s!=(_segments+1); ++s)
			{
				double b_angle =
					2*b*b_leap + d*b_step + b_offs;
				double cb = std::cos(b_angle);
				double sb = std::sin(b_angle);

				double s_angle = s*s_step;
				double ss = std::sin(s_angle);

				dest[k++] = T(m*-sb);
				dest[k++] = T(0);
				dest[k++] = T(m*-cb);
				b_offs += ss * s_step;
			}
		}
	}
}

template <typename T>
void SpiralSphere::_make_bitangents(
	std::vector<T>& dest,
	unsigned& k,
	double sign
) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double b_step = b_leap / double(_divisions);
	double s_step = (math::Pi()) / double(_segments);

	double m = sign;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned d=0; d!=(_divisions+1); ++d)
		{
			double b_offs = 0.0;
			for(unsigned s=0; s!=(_segments+1); ++s)
			{
				double b_angle =
					2*b*b_leap + d*b_step + b_offs;
				double cb = std::cos(b_angle);
				double sb = std::sin(b_angle);

				double s_angle = s*s_step;
				double cs = std::cos(s_angle);
				double ss = std::sin(s_angle);

				double tx = m*-sb;
				double ty = 0.0;
				double tz = m*-cb;

				double nx = m*ss* cb;
				double ny = m*cs;
				double nz = m*ss*-sb;

				dest[k++] = T(ny*tz-nz*ty);
				dest[k++] = T(nz*tx-nx*tz);
				dest[k++] = T(nx*ty-ny*tx);

				b_offs += ss * s_step;
			}
		}
	}
}

template <typename T>
void SpiralSphere::_make_uv_coords(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = 0.5 / double(_bands);
	double b_step = b_leap / double(_divisions);
	double s_step = 1.0 / double(_segments);

	double u = 0.0;
	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned d=0; d!=(_divisions+1); ++d)
		{
			double v = 1.0;
			for(unsigned s=0; s!=(_segments+1); ++s)
			{
				dest[k++] = T(u);
				dest[k++] = T(v);
				v -= s_step;
			}
			u += b_step;
		}
		u += b_leap;
	}
}

template <typename T>
void SpiralSphere::_make_side_verts(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double b_slip = b_leap * _thickness * 0.5;
	double s_step = (math::Pi()) / double(_segments);

	double m = _radius + _thickness * 0.5;
	double g = -1.0;

	for(unsigned b=0; b!=_bands*2; ++b)
	{
		double b_offs = 0.0;
		for(unsigned s=0; s!=(_segments+1); ++s)
		{
			double b_angle =
				b*b_leap + b_offs + g*b_slip;
			double cb = std::cos(b_angle);
			double sb = std::sin(b_angle);

			double s_angle = s*s_step;
			double cs = std::cos(s_angle);
			double ss = std::sin(s_angle);

			dest[k++] = T(m* ss * cb);
			dest[k++] = T(m* cs);
			dest[k++] = T(m* ss * -sb);
			b_offs += ss * s_step;
		}
		g *= -1.0;
	}
}

template <typename T>
void SpiralSphere::_make_side_norms(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double s_step = (math::Pi()) / double(_segments);

	double m = 1.0;
	for(unsigned b=0; b!=_bands*2; ++b)
	{
		double b_offs = 0.0;
		for(unsigned s=0; s!=(_segments+1); ++s)
		{
			double b_angle =
				b*b_leap + b_offs;
			double cb = std::cos(b_angle);
			double sb = std::sin(b_angle);

			double s_angle = s*s_step;
			double ss = std::sin(s_angle);

			dest[k++] = T(m*-sb);
			dest[k++] = T(0);
			dest[k++] = T(m* cb);
			b_offs += ss * s_step;
		}
		m *= -1.0;
	}
}

template <typename T>
void SpiralSphere::_make_side_tgts(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double s_step = (math::Pi()) / double(_segments);

	double m = -1.0;
	for(unsigned b=0; b!=_bands*2; ++b)
	{
		double b_offs = 0.0;
		for(unsigned s=0; s!=(_segments+1); ++s)
		{
			double b_angle =
				b*b_leap + b_offs;
			double cb = std::cos(b_angle);
			double sb = std::sin(b_angle);

			double s_angle = s*s_step;
			double cs = std::cos(s_angle);
			double ss = std::sin(s_angle);

			dest[k++] = T(m*ss*-cb);
			dest[k++] = T(m*cs);
			dest[k++] = T(m*ss*-sb);
			b_offs += ss * s_step;
		}
		m *= -1.0;
	}
}

template <typename T>
void SpiralSphere::_make_side_btgs(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = (math::Pi()) / double(_bands);
	double s_step = (math::Pi()) / double(_segments);

	double m = 1.0;
	for(unsigned b=0; b!=_bands*2; ++b)
	{
		double b_offs = 0.0;
		for(unsigned s=0; s!=(_segments+1); ++s)
		{
			double b_angle =
				b*b_leap + b_offs;
			double cb = std::cos(b_angle);
			double sb = std::sin(b_angle);

			double s_angle = s*s_step;
			double cs = std::cos(s_angle);
			double ss = std::sin(s_angle);

			double tx = m*ss*-cb;
			double ty = m*cs;
			double tz = m*ss*-sb;

			double nx = m* sb;
			double ny = 0.0;
			double nz = m*-cb;

			dest[k++] = T(ny*tz-nz*ty);
			dest[k++] = T(nz*tx-nx*tz);
			dest[k++] = T(nx*ty-ny*tx);

			b_offs += ss * s_step;
		}
		m *= -1.0;
	}
}

template <typename T>
void SpiralSphere::_make_side_uvs(std::vector<T>& dest, unsigned& k) const
{
	double b_leap = 0.5 / double(_bands);
	double b_slip = b_leap * _thickness * 0.5;
	double s_step = 1.0 / double(_segments);

	double g = -1.0;

	for(unsigned b=0; b!=_bands*2; ++b)
	{
		double b_offs = 0.0;
		double v = 1.0;
		for(unsigned s=0; s!=(_segments+1); ++s)
		{
			dest[k++] = T(b*b_leap + b_offs + g*b_slip);
			dest[k++] = T(v);
			v -= s_step;
		}
		g *= -1.0;
	}
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> SpiralSphere::_positions(void) const
{
	std::vector<GLfloat> dest(_vertex_count() * 3);
	unsigned k = 0;
	//
	_make_vectors(dest, k,  1.0, _radius);
	_make_vectors(dest, k,  1.0, _radius + _thickness);
	_make_side_verts(dest, k);
	//
	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> SpiralSphere::_normals(void) const
{
	std::vector<GLfloat> dest(_vertex_count() * 3);
	unsigned k = 0;
	//
	_make_vectors(dest, k, -1.0, 1.0);
	_make_vectors(dest, k,  1.0, 1.0);
	_make_side_norms(dest, k);
	//
	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> SpiralSphere::_tangents(void) const
{
	std::vector<GLfloat> dest(_vertex_count() * 3);
	unsigned k = 0;
	//
	_make_tangents(dest, k, -1.0);
	_make_tangents(dest, k,  1.0);
	_make_side_tgts(dest, k);
	//
	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> SpiralSphere::_bitangents(void) const
{
	std::vector<GLfloat> dest(_vertex_count() * 3);
	unsigned k = 0;
	//
	_make_bitangents(dest, k, -1.0);
	_make_bitangents(dest, k,  1.0);
	_make_side_btgs(dest, k);
	//
	assert(k == dest.size());
	return std::move(dest);
}

std::vector<GLfloat> SpiralSphere::_tex_coords(void) const
{
	std::vector<GLfloat> dest(_vertex_count() * 2);
	unsigned k = 0;
	//
	_make_uv_coords(dest, k);
	_make_uv_coords(dest, k);
	_make_side_uvs(dest, k);
	//
	assert(k == dest.size());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
SpiralSphere::IndexArray
SpiralSphere::Indices(SpiralSphere::Default) const
{
	const unsigned m =
		(_bands * 2)*
		(_divisions * 2)*
		(_segments + 1)+
		(_bands * 8)*
		(_segments + 1)
	;
	//
	IndexArray indices(m);
	unsigned k = 0;
	unsigned eoffs, offs = 0;
	const unsigned edge = _segments + 1;
	const unsigned band = edge * (_divisions + 1);
	const unsigned surface = _bands * band;

	for(unsigned n=0; n!=2; ++n)
	{
		unsigned edge1 = n ? edge : 0;
		unsigned edge2 = n ? 0 : edge;
		for(unsigned b=0; b!=_bands; ++b)
		{
			for(unsigned d=0; d!=_divisions; ++d)
			{
				for(unsigned s=0; s!=edge; ++s)
				{
					indices[k++] = offs + s + edge1;
					indices[k++] = offs + s + edge2;
				}
				offs += edge;
			}
			offs += edge;
		}
	}

	offs = 0;
	eoffs = 2*surface;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned s=0; s!=edge; ++s)
		{
			indices[k++] = eoffs + s;
			indices[k++] = offs + s;
		}
		offs += band;
		eoffs += edge * 2;
	}

	offs = _divisions * edge;
	eoffs = 2*surface + edge;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned s=0; s!=edge; ++s)
		{
			indices[k++] = offs + s;
			indices[k++] = eoffs + s;
		}
		offs += band;
		eoffs += edge * 2;
	}

	offs = surface;
	eoffs = 2*surface;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned s=0; s!=edge; ++s)
		{
			indices[k++] = offs + s;
			indices[k++] = eoffs + s;
		}
		offs += band;
		eoffs += edge * 2;
	}

	offs = surface + _divisions * edge;
	eoffs = 2*surface + edge;

	for(unsigned b=0; b!=_bands; ++b)
	{
		for(unsigned s=0; s!=edge; ++s)
		{
			indices[k++] = eoffs + s;
			indices[k++] = offs + s;
		}
		offs += band;
		eoffs += edge * 2;
	}

	assert(k == indices.size());
	//
	// return the indices
	return indices;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
SpiralSphere::Instructions(SpiralSphere::Default) const
{
	auto instructions = this->MakeInstructions();
	const unsigned edge = _segments + 1;

	auto method = DrawOperation::Method::DrawElements;
	auto primitive_type = PrimitiveType::TriangleStrip;

	GLuint offs = 0;
	GLuint restart_index = DrawOperation::NoRestartIndex();
	GLuint phase = 0;
	for(unsigned n=0; n!=2; ++n)
	{
		for(unsigned b=0; b!=_bands; ++b)
		{
			for(unsigned d=0; d!=_divisions; ++d)
			{
				DrawOperation operation;
				operation.method = method;
				operation.mode = primitive_type;
				operation.first = offs;
				operation.count = GLuint(edge * 2);
				operation.restart_index = restart_index;
				operation.phase = phase;
				this->AddInstruction(instructions, operation);
				offs += edge * 2;
			}
		}
		++phase;
	}
	for(unsigned n=0; n!=4; ++n)
	{
		for(unsigned b=0; b!=_bands; ++b)
		{
			DrawOperation operation;
			operation.method = method;
			operation.mode = primitive_type;
			operation.first = offs;
			operation.count = GLuint(edge * 2);
			operation.restart_index = restart_index;
			operation.phase = phase;
			this->AddInstruction(instructions, operation);
			offs += edge * 2;
		}
	}
	return instructions;
}

} // shapes
} // oglplus

