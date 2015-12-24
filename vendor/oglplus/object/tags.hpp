/**
 *  @file oglplus/object/tags.hpp
 *  @brief GL Object tag types.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_TAGS_1405011014_HPP
#define OGLPLUS_OBJECT_TAGS_1405011014_HPP

#include <type_traits>

namespace oglplus {
namespace tag {

#define OGLPLUS_DEFINE_OBJECT_TAG(ID, OBJECT) \
struct OBJECT \
 : std::integral_constant<int, ID> \
{ \
	typedef GLuint NameType; \
};

OGLPLUS_DEFINE_OBJECT_TAG( 1, Texture)
OGLPLUS_DEFINE_OBJECT_TAG( 2, Buffer)
OGLPLUS_DEFINE_OBJECT_TAG( 3, Framebuffer)
OGLPLUS_DEFINE_OBJECT_TAG( 4, Renderbuffer)
OGLPLUS_DEFINE_OBJECT_TAG( 5, Query)
OGLPLUS_DEFINE_OBJECT_TAG( 6, ProgramPipeline)
OGLPLUS_DEFINE_OBJECT_TAG( 7, Program)
OGLPLUS_DEFINE_OBJECT_TAG( 8, TransformFeedback)
OGLPLUS_DEFINE_OBJECT_TAG( 9, Sampler)
OGLPLUS_DEFINE_OBJECT_TAG(10, VertexArray)
OGLPLUS_DEFINE_OBJECT_TAG(11, Shader)
OGLPLUS_DEFINE_OBJECT_TAG(12, PerfMonitorAMD)
OGLPLUS_DEFINE_OBJECT_TAG(13, PathNV)

#undef OGLPLUS_DEFINE_OBJECT_TAG

} // namespace tag
} // namespace oglplus

#endif // include guard
