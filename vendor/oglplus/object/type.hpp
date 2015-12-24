/**
 *  @file oglplus/object/type.hpp
 *  @brief ObjectType enumeration
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_TYPE_1405022040_HPP
#define OGLPLUS_OBJECT_TYPE_1405022040_HPP

#include <oglplus/enums/object_type.hpp>
#include <oglplus/object/tags.hpp>

namespace oglplus {

template <typename ObjTag>
struct ObjTypeOps
{
	static oglplus::ObjectType ObjectType(void)
	{
		return oglplus::ObjectType(GL_NONE);
	}
};

#define OGLPLUS_SPEC_OBJ_TYPE_OPS(TYPE) \
template <> \
struct ObjTypeOps<tag::TYPE> \
{ \
	static oglplus::ObjectType ObjectType(void) \
	{ \
		return oglplus::ObjectType::TYPE; \
	} \
};

#ifdef GL_BUFFER
OGLPLUS_SPEC_OBJ_TYPE_OPS(Buffer)
#endif
#ifdef GL_FRAMEBUFFER
OGLPLUS_SPEC_OBJ_TYPE_OPS(Framebuffer)
#endif
#ifdef GL_PROGRAM_PIPELINE
OGLPLUS_SPEC_OBJ_TYPE_OPS(ProgramPipeline)
#endif
#ifdef GL_PROGRAM
OGLPLUS_SPEC_OBJ_TYPE_OPS(Program)
#endif
#ifdef GL_QUERY
OGLPLUS_SPEC_OBJ_TYPE_OPS(Query)
#endif
#ifdef GL_RENDERBUFFER
OGLPLUS_SPEC_OBJ_TYPE_OPS(Renderbuffer)
#endif
#ifdef GL_SAMPLER
OGLPLUS_SPEC_OBJ_TYPE_OPS(Sampler)
#endif
#ifdef GL_SHADER
OGLPLUS_SPEC_OBJ_TYPE_OPS(Shader)
#endif
#ifdef GL_TEXTURE
OGLPLUS_SPEC_OBJ_TYPE_OPS(Texture)
#endif
#ifdef GL_TRANSFORM_FEEDBACK
OGLPLUS_SPEC_OBJ_TYPE_OPS(TransformFeedback)
#endif
#ifdef GL_VERTEX_ARRAY
OGLPLUS_SPEC_OBJ_TYPE_OPS(VertexArray)
#endif

#undef OGLPLUS_SPEC_OBJ_TYPE_OPS

} // namespace oglplus

#endif // include guard
