/**
 *  @file oglplus/shader_storage_block.ipp
 *  @brief Implementation of ShaderStorageBlock
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::ShaderStorageBlock>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program shader-storage block";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::ShaderStorageBlock>::
MsgUsingInactive(void)
{
	return "Using inactive program shader-storage block";
}

OGLPLUS_LIB_FUNC
GLenum ProgVarCommonOps<tag::ShaderStorageBlock>::
_translate_ref(ShaderType shader_type)
{
	switch(shader_type)
	{
#ifdef GL_VERTEX_SHADER
		case ShaderType::Vertex:
		return GL_REFERENCED_BY_VERTEX_SHADER;
#endif
#ifdef GL_TESS_CONTROL_SHADER
		case ShaderType::TessControl:
		return GL_REFERENCED_BY_TESS_CONTROL_SHADER;
#endif
#ifdef GL_TESS_EVALUATION_SHADER
		case ShaderType::TessEvaluation:
		return GL_REFERENCED_BY_TESS_EVALUATION_SHADER;
#endif
#ifdef GL_GEOMETRY_SHADER
		case ShaderType::Geometry:
		return GL_REFERENCED_BY_GEOMETRY_SHADER;
#endif
#ifdef GL_FRAGMENT_SHADER
		case ShaderType::Fragment:
		return GL_REFERENCED_BY_FRAGMENT_SHADER;
#endif
#ifdef GL_COMPUTE_SHADER
		case ShaderType::Compute:
		return GL_REFERENCED_BY_COMPUTE_SHADER;
#endif
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLenum ProgVarCommonOps<tag::ShaderStorageBlock>::
_translate_max(ShaderType shader_type)
{
	switch(shader_type)
	{
#ifdef GL_VERTEX_SHADER
		case ShaderType::Vertex:
		return GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS;
#endif
#ifdef GL_TESS_CONTROL_SHADER
		case ShaderType::TessControl:
		return GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS;
#endif
#ifdef GL_TESS_EVALUATION_SHADER
		case ShaderType::TessEvaluation:
		return GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS;
#endif
#ifdef GL_GEOMETRY_SHADER
		case ShaderType::Geometry:
		return GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS;
#endif
#ifdef GL_FRAGMENT_SHADER
		case ShaderType::Fragment:
		return GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS;
#endif
#ifdef GL_COMPUTE_SHADER
		case ShaderType::Compute:
		return GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS;
#endif
	}
	return 0;
}

#endif // shader storage buffer object

} // namespace oglplus

