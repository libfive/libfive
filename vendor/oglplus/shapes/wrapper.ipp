/**
 *  @file oglplus/shapes/wrapper.ipp
 *  @brief Implementation of shapes::ShapeWrapper
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
VertexArray ShapeWrapperBase::VAOForProgram(const ProgramOps& prog) const
{
	VertexArray vao;
	vao.Bind();
	SetupForProgram(prog);
	return std::move(vao);
}

OGLPLUS_LIB_FUNC
void ShapeWrapperBase::SetupForProgram(ProgramName progName) const
{
	Program::Bind(progName);
	size_t i=0, n = _names.size();
	while(i != n)
	{
		if(_npvs[i] != 0)
		{
			try
			{
				_vbos[i].Bind(Buffer::Target::Array);
				VertexArrayAttrib attr(progName, _names[i]);
				attr.Setup<GLfloat>(_npvs[i]);
				attr.Enable();
			}
			catch(Error&){ }
		}
		++i;
	}
	assert((i+1) == _npvs.size());
	if(_npvs[i] != 0)
	{
		assert((i+1) == _vbos.size());
		_vbos[i].Bind(Buffer::Target::ElementArray);
	}
}

} // shapes
} // oglplus

