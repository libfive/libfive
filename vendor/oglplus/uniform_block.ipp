/**
 *  @file oglplus/uniform_block.ipp
 *  @brief Implementation of UniformBlock
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1 || GL_ARB_uniform_buffer_object

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::UniformBlock>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program uniform block";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::UniformBlock>::
MsgUsingInactive(void)
{
	return "Using inactive program uniform block";
}

OGLPLUS_LIB_FUNC
GLenum ProgVarCommonOps<tag::UniformBlock>::
_translate_ref(ShaderType shader_type)
{
	switch(shader_type)
	{
#ifdef GL_VERTEX_SHADER
		case ShaderType::Vertex:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER;
#endif
#ifdef GL_TESS_CONTROL_SHADER
		case ShaderType::TessControl:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER;
#endif
#ifdef GL_TESS_EVALUATION_SHADER
		case ShaderType::TessEvaluation:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER;
#endif
#ifdef GL_GEOMETRY_SHADER
		case ShaderType::Geometry:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER;
#endif
#ifdef GL_FRAGMENT_SHADER
		case ShaderType::Fragment:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER;
#endif
#ifdef GL_COMPUTE_SHADER
		case ShaderType::Compute:
		return GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER;
#endif
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLenum ProgVarCommonOps<tag::UniformBlock>::
_translate_max(ShaderType shader_type)
{
	switch(shader_type)
	{
#ifdef GL_VERTEX_SHADER
		case ShaderType::Vertex:
		return GL_MAX_VERTEX_UNIFORM_BLOCKS;
#endif
#ifdef GL_TESS_CONTROL_SHADER
		case ShaderType::TessControl:
		return GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS;
#endif
#ifdef GL_TESS_EVALUATION_SHADER
		case ShaderType::TessEvaluation:
		return GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS;
#endif
#ifdef GL_GEOMETRY_SHADER
		case ShaderType::Geometry:
		return GL_MAX_GEOMETRY_UNIFORM_BLOCKS;
#endif
#ifdef GL_FRAGMENT_SHADER
		case ShaderType::Fragment:
		return GL_MAX_FRAGMENT_UNIFORM_BLOCKS;
#endif
#ifdef GL_COMPUTE_SHADER
		case ShaderType::Compute:
		return GL_MAX_COMPUTE_UNIFORM_BLOCKS;
#endif
	}
	return 0;
}

#endif // uniform buffer object

} // namespace oglplus

