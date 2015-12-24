/**
 *  @file oglplus/shapes/subdiv_sphere.ipp
 *  @brief Implementation of shapes::SubdivSphere
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/assert.hpp>

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
GLuint SimpleSubdivSphere::_midpoint(GLuint ia, GLuint ib)
{
	GLuint ea=ia<ib?ia:ib;
	GLuint eb=ia>ib?ia:ib;

	_edge e(ea,eb);

	auto p = _midpoints.find(e);
	if(p == _midpoints.end())
	{
		Vec3d va(_positions.data()+ea*3, 3);
		Vec3d vb(_positions.data()+eb*3, 3);
		Vec3f mp = Normalized((va+vb)*0.5);
		GLuint result = GLuint(_positions.size());

		_positions.insert(_positions.end(), mp.Data(), mp.Data()+3);

		assert(result % 3 == 0);
		result /= 3;

		_midpoints[e] = result;

		return result;
	}
	else
	{
		return p->second;
	}
}

OGLPLUS_LIB_FUNC
void SimpleSubdivSphere::_subdivide(
	GLuint ia,
	GLuint ib,
	GLuint ic,
	GLuint levels
)
{
	GLuint iab = _midpoint(ia, ib);
	GLuint ibc = _midpoint(ib, ic);
	GLuint ica = _midpoint(ic, ia);

	_make_face(iab,ibc, ica, levels);
	_make_face(ica, ia, iab, levels);
	_make_face(iab, ib, ibc, levels);
	_make_face(ibc, ic, ica, levels);
}

OGLPLUS_LIB_FUNC
void SimpleSubdivSphere::_make_face(
	GLuint ia,
	GLuint ib,
	GLuint ic,
	GLuint levels
)
{
	if(levels)
	{
		_subdivide(ia, ib, ic, levels-1);
	}
	else
	{
		_indices.push_back(ia);
		_indices.push_back(ib);
		_indices.push_back(ic);
	}
}

OGLPLUS_LIB_FUNC
void SimpleSubdivSphere::_init_icosah(void)
{
	static const GLdouble init_pos[12*3] = {
		 0.000,  1.000,  0.000,
		 0.894,  0.447,  0.000,
		 0.276,  0.447,  0.851,
		-0.724,  0.447,  0.526,
		-0.724,  0.447, -0.526,
		 0.276,  0.447, -0.851,
		 0.724, -0.447,  0.526,
		-0.276, -0.447,  0.851,
		-0.894, -0.447,  0.000,
		-0.276, -0.447, -0.851,
		 0.724, -0.447, -0.526,
		 0.000, -1.000,  0.000
	};

	_positions.insert(
		_positions.end(),
		init_pos,
		init_pos+12*3
	);

	_make_face( 2,  1,  0, _subdivs);
	_make_face( 3,  2,  0, _subdivs);
	_make_face( 4,  3,  0, _subdivs);
	_make_face( 5,  4,  0, _subdivs);
	_make_face( 1,  5,  0, _subdivs);
	_make_face(11,  6,  7, _subdivs);
	_make_face(11,  7,  8, _subdivs);
	_make_face(11,  8,  9, _subdivs);
	_make_face(11,  9, 10, _subdivs);
	_make_face(11, 10,  6, _subdivs);
	_make_face( 1,  2,  6, _subdivs);
	_make_face( 2,  3,  7, _subdivs);
	_make_face( 3,  4,  8, _subdivs);
	_make_face( 4,  5,  9, _subdivs);
	_make_face( 5,  1, 10, _subdivs);
	_make_face( 2,  7,  6, _subdivs);
	_make_face( 3,  8,  7, _subdivs);
	_make_face( 4,  9,  8, _subdivs);
	_make_face( 5, 10,  9, _subdivs);
	_make_face( 1,  6, 10, _subdivs);
}

OGLPLUS_LIB_FUNC
void SimpleSubdivSphere::_init_tetrah(void)
{
	static const GLdouble init_pos[4*3] = {
		 0.0, 1.0, 0.0,
		-1.0*std::sqrt(2.0)/3.0, -1.0/3.0, -std::sqrt(2.0/3.0),
		-1.0*std::sqrt(2.0)/3.0, -1.0/3.0,  std::sqrt(2.0/3.0),
		+2.0*std::sqrt(2.0)/3.0, -1.0/3.0, 0.0
	};

	_positions.insert(
		_positions.end(),
		init_pos,
		init_pos+4*3
	);

	_make_face( 3, 2, 1, _subdivs);
	_make_face( 3, 0, 2, _subdivs);
	_make_face( 1, 0, 3, _subdivs);
	_make_face( 2, 0, 1, _subdivs);
}

OGLPLUS_LIB_FUNC
void SimpleSubdivSphere::_init_octoh(void)
{
	const GLuint px=0, nx=1, py=2, ny=3, pz=4, nz=5;

	_positions.resize(6*3, 0);

	//[0] +x
	_positions[px*3+0] =  1;
	//[1] -x
	_positions[nx*3+0] = -1;
	//[2] +y
	_positions[py*3+1] =  1;
	//[3] -y
	_positions[ny*3+1] = -1;
	//[4] +z
	_positions[pz*3+2] =  1;
	//[5] -z
	_positions[nz*3+2] = -1;

	// f[0]
	_make_face(px, py, pz, _subdivs);
	// f[1]
	_make_face(pz, py, nx, _subdivs);
	// f[2]
	_make_face(nx, ny, pz, _subdivs);
	// f[3]
	_make_face(pz, ny, px, _subdivs);
	// f[4]
	_make_face(nz, py, px, _subdivs);
	// f[5]
	_make_face(nx, py, nz, _subdivs);
	// f[6]
	_make_face(nz, ny, nx, _subdivs);
	// f[7]
	_make_face(px, ny, nz, _subdivs);
}

OGLPLUS_LIB_FUNC
SimpleSubdivSphere::SimpleSubdivSphere(GLuint subdivs, InitialShape init_shape)
 : _subdivs(subdivs)
{
	if(init_shape == InitialShape::Icosahedron)
		_init_icosah();
	else if(init_shape == InitialShape::Octohedron)
		_init_octoh();
	else if(init_shape == InitialShape::Tetrahedron)
		_init_tetrah();
	else OGLPLUS_ABORT("Invalid initial shape!");
}

OGLPLUS_LIB_FUNC
DrawingInstructions SimpleSubdivSphere::Instructions(PrimitiveType mode) const
{
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = mode;
	operation.first = 0;
	operation.count = GLuint(_indices.size());
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;

	return this->MakeInstructions(operation);
}

} // shapes
} // oglplus

