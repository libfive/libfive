/**
 *  @file oglplus/extension.hpp
 *  @brief Funcions and classes for handling and wrapping OpenGL extensions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXTENSION_1203031825_HPP
#define OGLPLUS_EXTENSION_1203031825_HPP

#include <oglplus/config/gl.hpp>
#include <oglplus/error/basic.hpp>

namespace oglplus {

/** @defgroup gl_extensions Extension wrappers
 *
 * Classes in this group implement wrappers around OpenGL extensions.
 */

void RequireExtension(const GLchar* name, bool available);

#if OGLPLUS_USE_GLEW
#define OGLPLUS_EXTENSION_AVAILABLE(VENDOR,EXTENSION) (\
	(GL_ ## VENDOR ## _ ## EXTENSION == GL_TRUE) ||\
	(GLEW_ ## VENDOR ## _ ## EXTENSION == GL_TRUE) \
)
#else
bool HasExtension(const GLchar* name);
#define OGLPLUS_EXTENSION_AVAILABLE(VENDOR,EXTENSION) (\
	HasExtension("GL_" #VENDOR "_" #EXTENSION) \
)
#endif

#define OGLPLUS_REQUIRE_EXTENSION(VENDOR, EXTENSION) \
	::oglplus::RequireExtension( \
		#VENDOR "_" #EXTENSION, \
		OGLPLUS_EXTENSION_AVAILABLE(VENDOR, EXTENSION) \
	)

#define OGLPLUS_EXTENSION_CLASS(VENDOR, EXTENSION) \
	VENDOR ## _ ## EXTENSION(bool throw_if_unavailable = true) \
	{ \
		if(throw_if_unavailable) \
			OGLPLUS_REQUIRE_EXTENSION(VENDOR, EXTENSION); \
	} \
	static bool Available(void) \
	{ \
		return OGLPLUS_EXTENSION_AVAILABLE(VENDOR, EXTENSION); \
	}

/// Allows to check is the specified extension is available
/**
 *  Example of usage:
 *  @code
 *  if(OGLPLUS_HAS_GL_EXT(ARB,debug_info))
 *  {
 *    // the extension is available
 *  }
 *  else
 *  {
 *    // the extension is not available
 *  }
 *  @endcode
 *  @ingroup gl_extensions
 */
#define OGLPLUS_HAS_GL_EXT(VENDOR,EXTENSION) \
	OGLPLUS_EXTENSION_AVAILABLE(VENDOR,EXTENSION)

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/extension.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
