/**
 *  @file oglplus/sampler.ipp
 *  @brief Implementation of Sampler functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::Sampler>::
_binding(TextureUnitSelector tex_unit)
{
	OGLPLUS_GLFUNC(ActiveTexture)(GLuint(tex_unit));
	OGLPLUS_VERIFY(
		ActiveTexture,
		Error,
		Index(GLuint(tex_unit))
	);

	GLint name = 0;
	OGLPLUS_GLFUNC(GetIntegerv)(GL_SAMPLER_BINDING, &name);
	OGLPLUS_VERIFY(
		GetIntegerv,
		Error,
		EnumParam(GLenum(GL_SAMPLER_BINDING))
	);

	assert(!(name < 0));
	return GLuint(name);
}

} // namespace oglplus

