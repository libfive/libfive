/**
 *  @file oglplus/shapes/cube.ipp
 *  @brief Implementation of shapes::Cube
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
std::vector<GLfloat> Cube::_positions(void) const
{
	/*
	 *   (E)-----(A)
	 *   /|      /|
	 *  / |     / |
	 *(F)-----(B) |
	 * | (H)---|-(D)
	 * | /     | /
	 * |/      |/
	 *(G)-----(C)
	 *
	 */
	typedef GLdouble T;
	typedef GLfloat V;
	const T half_x = T(_sx)/T(2);
	const T half_y = T(_sy)/T(2);
	const T half_z = T(_sz)/T(2);
	const V c[8][3] = {
		{V(_ox+half_x), V(_oy+half_y), V(_oz-half_z)}, //(A)
		{V(_ox+half_x), V(_oy+half_y), V(_oz+half_z)}, //(B)
		{V(_ox+half_x), V(_oy-half_y), V(_oz+half_z)}, //(C)
		{V(_ox+half_x), V(_oy-half_y), V(_oz-half_z)}, //(D)
		{V(_ox-half_x), V(_oy+half_y), V(_oz-half_z)}, //(E)
		{V(_ox-half_x), V(_oy+half_y), V(_oz+half_z)}, //(F)
		{V(_ox-half_x), V(_oy-half_y), V(_oz+half_z)}, //(G)
		{V(_ox-half_x), V(_oy-half_y), V(_oz-half_z)}  //(H)
	};
	const unsigned A=0, B=1, C=2, D=3, E=4, F=5, G=6, H=7;

	std::vector<GLfloat> dest(108);
	auto p = dest.begin();

	*p++ = c[A][0]; *p++ = c[A][1]; *p++ = c[A][2];
	*p++ = c[D][0]; *p++ = c[D][1]; *p++ = c[D][2];
	*p++ = c[B][0]; *p++ = c[B][1]; *p++ = c[B][2];
	*p++ = c[C][0]; *p++ = c[C][1]; *p++ = c[C][2];
	*p++ = c[B][0]; *p++ = c[B][1]; *p++ = c[B][2];
	*p++ = c[D][0]; *p++ = c[D][1]; *p++ = c[D][2];

	*p++ = c[A][0]; *p++ = c[A][1]; *p++ = c[A][2];
	*p++ = c[B][0]; *p++ = c[B][1]; *p++ = c[B][2];
	*p++ = c[E][0]; *p++ = c[E][1]; *p++ = c[E][2];
	*p++ = c[F][0]; *p++ = c[F][1]; *p++ = c[F][2];
	*p++ = c[E][0]; *p++ = c[E][1]; *p++ = c[E][2];
	*p++ = c[B][0]; *p++ = c[B][1]; *p++ = c[B][2];

	*p++ = c[B][0]; *p++ = c[B][1]; *p++ = c[B][2];
	*p++ = c[C][0]; *p++ = c[C][1]; *p++ = c[C][2];
	*p++ = c[F][0]; *p++ = c[F][1]; *p++ = c[F][2];
	*p++ = c[G][0]; *p++ = c[G][1]; *p++ = c[G][2];
	*p++ = c[F][0]; *p++ = c[F][1]; *p++ = c[F][2];
	*p++ = c[C][0]; *p++ = c[C][1]; *p++ = c[C][2];

	*p++ = c[F][0]; *p++ = c[F][1]; *p++ = c[F][2];
	*p++ = c[G][0]; *p++ = c[G][1]; *p++ = c[G][2];
	*p++ = c[E][0]; *p++ = c[E][1]; *p++ = c[E][2];
	*p++ = c[H][0]; *p++ = c[H][1]; *p++ = c[H][2];
	*p++ = c[E][0]; *p++ = c[E][1]; *p++ = c[E][2];
	*p++ = c[G][0]; *p++ = c[G][1]; *p++ = c[G][2];

	*p++ = c[H][0]; *p++ = c[H][1]; *p++ = c[H][2];
	*p++ = c[G][0]; *p++ = c[G][1]; *p++ = c[G][2];
	*p++ = c[D][0]; *p++ = c[D][1]; *p++ = c[D][2];
	*p++ = c[C][0]; *p++ = c[C][1]; *p++ = c[C][2];
	*p++ = c[D][0]; *p++ = c[D][1]; *p++ = c[D][2];
	*p++ = c[G][0]; *p++ = c[G][1]; *p++ = c[G][2];

	*p++ = c[E][0]; *p++ = c[E][1]; *p++ = c[E][2];
	*p++ = c[H][0]; *p++ = c[H][1]; *p++ = c[H][2];
	*p++ = c[A][0]; *p++ = c[A][1]; *p++ = c[A][2];
	*p++ = c[D][0]; *p++ = c[D][1]; *p++ = c[D][2];
	*p++ = c[A][0]; *p++ = c[A][1]; *p++ = c[A][2];
	*p++ = c[H][0]; *p++ = c[H][1]; *p++ = c[H][2];

	assert(p == dest.end());

	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> Cube::_normals(void) const
{
	typedef GLfloat T;
	const T n[6][3] = {
		{+T(1),  T(0),  T(0)},
		{ T(0), +T(1),  T(0)},
		{ T(0),  T(0), +T(1)},
		{-T(1),  T(0),  T(0)},
		{ T(0), -T(1),  T(0)},
		{ T(0),  T(0), -T(1)}
	};
	std::vector<GLfloat> dest(108);

	std::vector<GLfloat>::iterator vi=dest.begin();

	for(int f=0; f!=6; ++f)
	{
		for(int v=0; v!=6; ++v)
		{
			for(int c=0; c!=3; ++c)
			{
				*vi++ = n[f][c];
			}
		}
	}
	assert(vi == dest.end());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> Cube::_tangents(void) const
{
	typedef GLfloat T;
	const T n[6][3] = {
		{ T(0),  T(0), -T(1)},
		{+T(1),  T(0),  T(0)},
		{+T(1),  T(0),  T(0)},
		{ T(0),  T(0), +T(1)},
		{-T(1),  T(0),  T(0)},
		{-T(1),  T(0),  T(0)}
	};

	std::vector<GLfloat> dest(108);
	std::vector<GLfloat>::iterator vi=dest.begin();
	for(int f=0; f!=6; ++f)
	{
		for(int v=0; v!=6; ++v)
		{
			for(int c=0; c!=3; ++c)
			{
				*vi++ = n[f][c];
			}
		}
	}
	assert(vi == dest.end());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> Cube::_tex_coords(void) const
{
	typedef GLfloat T;
	const T n[6][2] = {
		{+T(1), +T(1)},
		{+T(1),  T(0)},
		{ T(0), +T(1)},
		{ T(0),  T(0)},
		{ T(0), +T(1)},
		{+T(1),  T(0)}
	};

	std::vector<GLfloat> dest(108);
	std::vector<GLfloat>::iterator vi=dest.begin();
	for(int f=0; f!=6; ++f)
	{
		for(int v=0; v!=6; ++v)
		{
			for(int c=0; c!=2; ++c)
			{
				*vi++ = n[v][c];
			}
			*vi++ = f;
		}
	}
	assert(vi == dest.end());
	return std::move(dest);
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Cube::Instructions(Cube::Default) const
{
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawArrays;
	operation.mode = PrimitiveType::Triangles;
	operation.first = 0;
	operation.count = 36;
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;

	return this->MakeInstructions(operation);
}

OGLPLUS_LIB_FUNC
Cube::IndexArray
Cube::Indices(Cube::Edges) const
{
	/*
	 *   (E)-----(A)
	 *   /|      /|
	 *  / |     / |
	 *(F)-----(B) |
	 * | (H)---|-(D)
	 * | /     | /
	 * |/      |/
	 *(G)-----(C)
	 *
	 */
	GLushort _indices[24] = {
		 0,  1,  5,  2, //+x
		19, 22, 23, 18, //-x
		 6,  7, 10, 11, //+y
		26, 29, 24, 25, //-y
		12, 13, 16, 17, //+z
		31, 35, 32, 30  //-z
	};
	IndexArray indices(_indices, _indices+24);
	return indices;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Cube::Instructions(Cube::Edges) const
{
	auto instructions = this->MakeInstructions();
	for(unsigned r=0; r!=6; ++r)
	{
		DrawOperation operation;
		operation.method = DrawOperation::Method::DrawElements;
		operation.mode = PrimitiveType::LineLoop;
		operation.first = GLuint(r*4);
		operation.count = GLuint(4);
		operation.restart_index = DrawOperation::NoRestartIndex();
		operation.phase = 0;

		this->AddInstruction(instructions, operation);
	}
	return instructions;
}

} // shapes
} // oglplus
