/**
 *  @file oglplus/shapes/icosahedron.ipp
 *  @brief Implementation of shapes::Icosahedron
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
DrawingInstructions SimpleIcosahedron::Instructions(PrimitiveType mode) const
{
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = mode;
	operation.first = 0;
	operation.count = 20*3;
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;

	return this->MakeInstructions(operation);
}

OGLPLUS_LIB_FUNC
DrawingInstructions Icosahedron::Instructions(PrimitiveType mode) const
{
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawArrays;
	operation.mode = mode;
	operation.first = 0;
	operation.count = 20*3;
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;

	return this->MakeInstructions(operation);
}

OGLPLUS_LIB_FUNC
const GLushort* IcosahedronBase::_indices(void)
{
	static const GLushort indices[20*3] = {
		 2,  1,  0,
		 3,  2,  0,
		 4,  3,  0,
		 5,  4,  0,
		 1,  5,  0,
		11,  6,  7,
		11,  7,  8,
		11,  8,  9,
		11,  9, 10,
		11, 10,  6,
		 1,  2,  6,
		 2,  3,  7,
		 3,  4,  8,
		 4,  5,  9,
		 5,  1, 10,
		 2,  7,  6,
		 3,  8,  7,
		 4,  9,  8,
		 5, 10,  9,
		 1,  6, 10
	};
	return indices;
}

OGLPLUS_LIB_FUNC
const GLdouble* IcosahedronBase::_positions(void)
{
	static const GLdouble positions[12*3] = {
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
	return positions;
}

} // shapes
} // oglplus

