/**
 *  @file oglplus/shapes/tetrahedrons.ipp
 *  @brief Implementation of shapes::Tetrahedrons
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace shapes {

Tetrahedrons::IndexArray
Tetrahedrons::Indices(WithAdjacency) const
{
	const unsigned n = _divisions;
	const unsigned a = n+1;
	const unsigned b = a*a;
	//
	IndexArray indices(n*n*n*6*6);
	unsigned k = 0;

	/*
	 *   (E)-----(F)
	 *   /|      /|
	 *  / |     / |
	 *(H)-----(G) |
	 * | (A)---|-(B)
	 * | /     | /
	 * |/      |/
	 *(D)-----(C)
	 *
	 */

	for(unsigned z=0; z!=n; ++z)
	for(unsigned y=0; y!=n; ++y)
	for(unsigned x=0; x!=n; ++x)
	{
		unsigned A = (z+0)*b + (y+0)*a + (x+0) + 1;
		unsigned B = (z+0)*b + (y+0)*a + (x+1) + 1;
		unsigned C = (z+1)*b + (y+0)*a + (x+1) + 1;
		unsigned D = (z+1)*b + (y+0)*a + (x+0) + 1;
		unsigned E = (z+0)*b + (y+1)*a + (x+0) + 1;
		unsigned F = (z+0)*b + (y+1)*a + (x+1) + 1;
		unsigned G = (z+1)*b + (y+1)*a + (x+1) + 1;
		unsigned H = (z+1)*b + (y+1)*a + (x+0) + 1;
		unsigned O = 0;

		assert(A < a*a*a+1);
		assert(B < a*a*a+1);
		assert(C < a*a*a+1);
		assert(D < a*a*a+1);
		assert(E < a*a*a+1);
		assert(F < a*a*a+1);
		assert(G < a*a*a+1);
		assert(H < a*a*a+1);

		indices[k++] = C;
		indices[k++] = A;
		indices[k++] = B;
		indices[k++] = O;
		indices[k++] = G;
		indices[k++] = O;

		indices[k++] = B;
		indices[k++] = G;
		indices[k++] = A;
		indices[k++] = O;
		indices[k++] = F;
		indices[k++] = O;

		indices[k++] = E;
		indices[k++] = G;
		indices[k++] = F;
		indices[k++] = O;
		indices[k++] = A;
		indices[k++] = O;

		indices[k++] = E;
		indices[k++] = G;
		indices[k++] = A;
		indices[k++] = O;
		indices[k++] = H;
		indices[k++] = O;

		indices[k++] = A;
		indices[k++] = G;
		indices[k++] = D;
		indices[k++] = O;
		indices[k++] = H;
		indices[k++] = O;

		indices[k++] = D;
		indices[k++] = G;
		indices[k++] = A;
		indices[k++] = O;
		indices[k++] = C;
		indices[k++] = O;
	}

	assert(k == indices.size());
	//
	// return the indices
	return indices;
}

/// Returns the instructions for rendering
DrawingInstructions
Tetrahedrons::Instructions(WithAdjacency) const
{
	const unsigned n = _divisions;
	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::TrianglesAdjacency;
	operation.first = GLuint(0);
	operation.count = GLuint(n*n*n*6*6);
	operation.restart_index = DrawOperation::NoRestartIndex();
	operation.phase = 0;
	return this->MakeInstructions(operation);
}

} // shapes
} // oglplus

