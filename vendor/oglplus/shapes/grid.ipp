/**
 *  @file oglplus/shapes/grid.ipp
 *  @brief Implementation of shapes::Grid
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
Grid::IndexArray
Grid::Indices(Grid::Default) const
{
	unsigned k = 0;

	IndexArray indices((_udiv+1)*2 + (_vdiv+1)*2);

	for(unsigned i=0; i!=(_udiv+1); ++i)
	{
		indices[k++] = i*2;
		indices[k++] = i*2+1;
	}

	unsigned leap = (_udiv+1)*2;

	indices[k++] = 0;
	indices[k++] = leap-2;

	for(unsigned j=0; j!=(_vdiv-1); ++j)
	{
		indices[k++] = leap+j*2;
		indices[k++] = leap+j*2+1;
	}

	indices[k++] = 1;
	indices[k++] = leap-1;

	assert(k == indices.size());

	return indices;
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Grid::Instructions(Grid::Default) const
{
	auto instructions = this->MakeInstructions();

	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::Lines;
	operation.first = GLuint(0);
	operation.count = GLuint((_udiv+1)*2+(_vdiv+1)*2);
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;
	this->AddInstruction(instructions, operation);

	return instructions;
}

} // shapes
} // oglplus

