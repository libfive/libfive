/**
 *  @file oglplus/program_pipeline.ipp
 *  @brief Implementation of ProgramPipeline functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/detail/info_log.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_separate_shader_objects

OGLPLUS_LIB_FUNC
String ObjectOps<tag::DirectState, tag::ProgramPipeline>::
GetInfoLog(void) const
{
	return aux::GetInfoLog(
		_obj_name(), OGLPLUS_GLFUNC(GetProgramPipelineiv),
		OGLPLUS_GLFUNC(GetProgramPipelineInfoLog),
		"GetProgramPipelineiv",
		"GetProgramPipelineInfoLog"
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::ProgramPipeline>&
ObjectOps<tag::DirectState, tag::ProgramPipeline>::
Validate(void)
{
	OGLPLUS_GLFUNC(ValidateProgramPipeline)(_obj_name());
	OGLPLUS_VERIFY(
		ValidateProgramPipeline,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_HANDLE_ERROR_IF(
		!IsValid(),
		GL_INVALID_OPERATION,
		ValidationError::Message(),
		ValidationError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::ProgramPipeline>&>
ObjectOps<tag::DirectState, tag::ProgramPipeline>::
Validate(std::nothrow_t)
{
	OGLPLUS_GLFUNC(ValidateProgramPipeline)(_obj_name());
	OGLPLUS_DEFERRED_CHECK(
		ValidateProgramPipeline,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_RETURN_HANDLER_IF(
		!IsValid(),
		GL_INVALID_OPERATION,
		ValidationError::Message(),
		ValidationError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

#endif // program pipeline

} // namespace oglplus

