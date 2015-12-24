/**
 *  @file oglplus/gl.hpp
 *  @brief Includes GL3/gl3.h and disables gl.h/glext.h
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_GL_1107121519_HPP
#define OGLPLUS_GL_1107121519_HPP

#ifndef OGLPLUS_NO_SITE_CONFIG
#include <oglplus/config/site.hpp>
#endif

#ifndef OGLPLUS_USE_GLCOREARB_H
#define OGLPLUS_USE_GLCOREARB_H 1
#endif

#ifndef OGLPLUS_USE_GL3_H
#define OGLPLUS_USE_GL3_H 0
#endif

#ifndef OGLPLUS_USE_GLEW
#define OGLPLUS_USE_GLEW 0
#endif

#ifndef OGLPLUS_USE_GL3W
#define OGLPLUS_USE_GL3W 0
#endif


#ifndef OGLPLUS_NO_GL

# if OGLPLUS_USE_GLCOREARB_H
#  define GLCOREARB_PROTOTYPES
#  define GL_GLEXT_PROTOTYPES
#  include <GL/glcorearb.h>
#  include <GL/glext.h>

namespace oglplus {
struct GLAPIInitializer
{
	GLAPIInitializer(
		int /*gl_ver_major*/ = 3,
		int /*gl_ver_minor*/ = 3
	){ }
};
} // namespace oglplus

# elif OGLPLUS_USE_GL3_H
#  define GL3_PROTOTYPES
#  ifdef __APPLE__
#   include <OpenGL/gl3.h>
#  else
#   include <GL3/gl3.h>
#  endif
#  define __gl_h_
#  define __gl_h__
#  define __glext_h_
#  define __glext_h__

namespace oglplus {
struct GLAPIInitializer
{
	GLAPIInitializer(
		int /*gl_ver_major*/ = 3,
		int /*gl_ver_minor*/ = 3
	){ }
};
} // namespace oglplus

# elif OGLPLUS_USE_GLEW
#  include <GL/glew.h>
#  include <stdexcept>

namespace oglplus {
class GLAPIInitializer
{
public:
	GLAPIInitializer(
		int /*gl_ver_major*/ = 3,
		int /*gl_ver_minor*/ = 3
	)
	{
		glewExperimental = GL_TRUE;
		GLenum init_result = glewInit();
		glGetError();
		if(init_result != GLEW_OK)
		{
			throw std::runtime_error(
				"OpenGL/GLEW initialization error."
			);
		}
	}
};
} // namespace oglplus

# elif OGLPLUS_USE_GL3W
#  define GL3_PROTOTYPES
#  include <GL/gl3w.h>
#  include <stdexcept>

namespace oglplus {
class GLAPIInitializer
{
public:
	GLAPIInitializer(
		int gl_ver_major = 3,
		int gl_ver_minor = 3
	)
	{
		auto init_failed = gl3wInit();
		glGetError();
		if(init_failed)
		{
			throw std::runtime_error(
				"OpenGL/GL3W initialization error."
			);
		}
		if(!gl3wIsSupported(gl_ver_major, gl_ver_minor))
		{
			throw std::runtime_error(
				"Requested OpenGL version not supported"
			);
		}
	}
};
} // namespace oglplus

# else
#  error "Some library including OpenGL symbols is required!"
# endif // OGLPLUS_USE_*

#ifndef GL_VERTEX_ARRAY
#define GL_VERTEX_ARRAY 0x8074
#endif

#ifndef GL_BUFFER
#define GL_BUFFER 0x82E0
#endif

#ifndef GL_SHADER
#define GL_SHADER 0x82E1
#endif

#ifndef GL_PROGRAM
#define GL_PROGRAM 0x82E2
#endif

#ifndef GL_QUERY
#define GL_QUERY 0x82E3
#endif

#ifndef GL_PROGRAM_PIPELINE
#define GL_PROGRAM_PIPELINE 0x82E4
#endif

#ifndef GL_SAMPLER
#define GL_SAMPLER 0x82E6
#endif

#ifndef GL_TRANSFORM_FEEDBACK
#define GL_TRANSFORM_FEEDBACK 0x8E22
#endif

#ifndef GL_TEXTURE
#define GL_TEXTURE 0x1702
#endif

#ifndef GL_FRAMEBUFFER
#define GL_FRAMEBUFFER 0x8D40
#endif

#ifndef GL_RENDERBUFFER
#define GL_RENDERBUFFER 0x8D41
#endif

#ifndef GL_POLYGON_MODE
#define GL_POLYGON_MODE 0x0B40
#endif

#endif // OGLPLUS_NO_GL

#endif // include guard
