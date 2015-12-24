/**
 *  .file oglplus/detail/info_log.ipp
 *  .brief Implementation of GetInfoLog
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/error/basic.hpp>
#include <oglplus/lib/incl_end.ipp>
#include <vector>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
String GetInfoLog(
	GLuint object_name,
	void (GLAPIENTRY *GetObjectiv)(GLuint, GLenum, GLint*),
	void (GLAPIENTRY *GetObjectInfoLog)(GLuint, GLsizei, GLsizei*, GLchar*),
	const char* name_GetObjectiv,
	const char* name_GetObjectInfoLog
)
{
	int length = 0;
	GetObjectiv(object_name, GL_INFO_LOG_LENGTH, &length);
	OGLPLUS_GLFUNC_CHECK(
		name_GetObjectiv,
		Error,
		NoInfo()
	);
	if(length > 0)
	{
		GLsizei real_length = 0;
		std::vector<GLchar> buffer((std::size_t(length)));
		GetObjectInfoLog(
			object_name,
			GLsizei(buffer.size()),
			&real_length,
			buffer.data()
		);
		OGLPLUS_GLFUNC_CHECK(
			name_GetObjectInfoLog,
			Error,
			NoInfo()
		);
		return String(buffer.data(), buffer.size());
	}
	else return String();
}

} // namespace aux
} // namespace oglplus

