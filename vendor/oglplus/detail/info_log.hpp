/**
 *  .file oglplus/detail/info_log.hpp
 *  .brief Helper function getting the OpenGL info logs
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_INFO_LOG_1107121519_HPP
#define OGLPLUS_AUX_INFO_LOG_1107121519_HPP

#include <oglplus/config/gl.hpp>
#include <oglplus/string/def.hpp>

namespace oglplus {
namespace aux {

String GetInfoLog(
	GLuint object_name,
	void (GLAPIENTRY *GetObjectiv)(GLuint, GLenum, GLint*),
	void (GLAPIENTRY *GetObjectInfoLog)(GLuint, GLsizei, GLsizei*, GLchar*),
	const char* name_GetObjectiv,
	const char* name_GetObjectInfoLog
);

} // namespace aux
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/detail/info_log.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
