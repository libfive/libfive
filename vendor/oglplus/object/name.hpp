/**
 *  @file oglplus/object/name.hpp
 *  @brief Base class for OpenGL "named" objects
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_NAME_1107121519_HPP
#define OGLPLUS_OBJECT_NAME_1107121519_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/object/name_tpl.hpp>
#include <oglplus/object/tags.hpp>

namespace oglplus {

typedef ObjectName<tag::Renderbuffer> RenderbufferName;
typedef ObjectName<tag::Framebuffer> FramebufferName;
typedef ObjectName<tag::Texture> TextureName;
typedef ObjectName<tag::Buffer> BufferName;
typedef ObjectName<tag::Query> QueryName;
typedef ObjectName<tag::ProgramPipeline> ProgramPipelineName;
typedef ObjectName<tag::Program> ProgramName;
typedef ObjectName<tag::TransformFeedback> TransformFeedbackName;
typedef ObjectName<tag::Sampler> SamplerName;
typedef ObjectName<tag::VertexArray> VertexArrayName;
typedef ObjectName<tag::Shader> ShaderName;
typedef ObjectName<tag::PerfMonitorAMD> PerfMonitorAMDName;
typedef ObjectName<tag::PathNV> PathNVName;

/// Returns the GLuint OpenGL name assigned to @p named object
template <typename ObjTag>
inline
GLuint GetGLName(ObjectName<ObjTag> named)
OGLPLUS_NOEXCEPT(true)
{
	return GetName(named);
}

} // namespace oglplus

#endif // include guard
