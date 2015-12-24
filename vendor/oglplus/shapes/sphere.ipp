/**
 *  @file oglplus/shapes/sphere.ipp
 *  @brief Implementation of shapes::Sphere
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
Sphere::IndexArray
Sphere::Indices(Sphere::Default) const
{
#ifdef GL_PRIMITIVE_RESTART
	const unsigned n = (_rings + 1)*(2 * (_sections + 1) + 1);
#else
	const unsigned n = (_rings + 1)*(2 * (_sections + 1) + 2);
#endif
	//
	IndexArray indices(n);
	unsigned k = 0;
	unsigned offs = 0;
	// the triangle strips
	for(unsigned r=0; r!=(_rings+1); ++r)
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
	return std::move(indices);
}

OGLPLUS_LIB_FUNC
DrawingInstructions
Sphere::Instructions(Sphere::Default) const
{
	auto instructions = this->MakeInstructions();

#ifdef GL_PRIMITIVE_RESTART
	const GLuint n = (_rings + 1)*(2 * (_sections + 1) + 1);
#else
	const GLuint n = (_rings + 1)*(2 * (_sections + 1) + 2);
#endif

	DrawOperation operation;
	operation.method = DrawOperation::Method::DrawElements;
	operation.mode = PrimitiveType::TriangleStrip;
	operation.first =  GLuint(0);
	operation.count = n;
#ifdef GL_PRIMITIVE_RESTART
	operation.restart_index = n;
#else
	operation.restart_index = DrawOperation::NoRestartIndex();
#endif
	operation.phase = 0;
	this->AddInstruction(instructions, operation);

	return std::move(instructions);
}

} // shapes
} // oglplus

