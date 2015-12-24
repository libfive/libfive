/**
 *  @file oglplus/shapes/torus.ipp
 *  @brief Implementation of shapes::Torus
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
Torus::IndexArray
Torus::Indices(Torus::Default) const
{
#ifdef GL_PRIMITIVE_RESTART
	const unsigned n = _rings * (2 * (_sections + 1) + 1);
#else
	const unsigned n = _rings * (2 * (_sections + 1) + 2);
#endif
	//
	IndexArray indices(n);
	unsigned k = 0;
	unsigned offs = 0;
	// the triangle strips
	for(unsigned r=0; r!=(_rings); ++r)
	{
		for(unsigned s=0; s!=(_sections+1); ++s)
		{
			indices[k++] = offs + s;
			indices[k++] = offs + s + (_sections+1);
		}
		offs += _sections + 1;
#ifdef GL_PRIMITIVE_RESTART
		indices[k++] = n;
#else
		indices[k++] = offs + _sections;
		indices[k++] = offs;
#endif
	}
	assert(k == indices.size());
	//
	// return the indices
	return indices;
}

OGLPLUS_LIB_FUNC
Torus::IndexArray
Torus::Indices(Torus::Quads) const
{
	const unsigned n = _rings * (4 * _sections + 1);
	//
	IndexArray indices(n);
	unsigned k = 0;
	unsigned offs = 0;
	// the quads made of lines with adjacency
	for(unsigned r=0; r!=(_rings); ++r)
	{
		for(unsigned s=0; s!=(_sections); ++s)
		{
			indices[k++] = offs + s;
			indices[k++] = offs + s + (_sections+1);
			indices[k++] = offs + s + 1;
			indices[k++] = offs + s + (_sections+1) + 1;
		}
		indices[k++] = n;
		offs += _sections + 1;
	}
	assert(k == indices.size());
	//
	// return the indices
	return indices;
}

OGLPLUS_LIB_FUNC
Torus::IndexArray
Torus::Indices(WithAdjacency) const
{
	const unsigned m = _rings*(_sections + 1);
	const unsigned n = _rings*(4 * (_sections + 1) + 1);
	//
	IndexArray indices(n);
	unsigned k = 0;
	unsigned offs = 0;

	for(unsigned r=0; r!=(_rings); ++r)
	{
		indices[k++] = offs;
		indices[k++] = offs + (2*_sections);
		indices[k++] = offs + (_sections+1);
		for(unsigned s=0; s!=_sections; ++s)
		{
			indices[k++] = (offs + m-(_sections+1))%m + s+1;
			indices[k++] = offs + s + 1;
			indices[k++] = (offs + 2*(_sections+1)) % m + s;
			indices[k++] = offs + (_sections+1) + s + 1;
		}
		indices[k++] = offs + 1;
		indices[k++] = n;
		offs += _sections + 1;
	}
	assert(k == indices.size());
	//
	// return the indices
	return indices;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Torus::Instructions(Torus::Default) const
{
	auto instructions = this->MakeInstructions();

#ifdef GL_PRIMITIVE_RESTART
	const GLuint n = _rings * (2 * (_sections + 1) + 1);
#else
	const GLuint n = _rings * (2 * (_sections + 1) + 2);
#endif
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::TriangleStrip;
	operation.first = GLuint(0);
	operation.count = n;
#ifdef GL_PRIMITIVE_RESTART
	operation.restart_index = n;
#else
	operation.restart_index = DrawOperation::NoRestartIndex();
#endif
	operation.phase = 0;

	this->AddInstruction(instructions, operation);

	return instructions;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Torus::Instructions(Torus::Quads) const
{
#if GL_VERSION_3_2
	auto instructions = this->MakeInstructions();

	const GLuint n = _rings * (4 * _sections + 1);
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::LinesAdjacency;
	operation.first = GLuint(0);
	operation.count = n;
	operation.restart_index = n;
	operation.phase = 0;

	this->AddInstruction(instructions, operation);

	return instructions;
#else
	return Instructions();
#endif
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Torus::Instructions(Torus::WithAdjacency) const
{
#if GL_VERSION_3_2
	auto instructions = this->MakeInstructions();

	const unsigned n = _rings*(4 * (_sections + 1) + 1);
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::TriangleStripAdjacency;
	operation.first = GLuint(0);
	operation.count = n;
	operation.restart_index = n;
	operation.phase = 0;

	this->AddInstruction(instructions, operation);

	return instructions;
#else
	return Instructions();
#endif
}

} // shapes
} // oglplus

