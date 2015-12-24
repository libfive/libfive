/**
 *  @file oglplus/error/prog_var.ipp
 *  @brief Implementation of ProgVarError
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
ProgVarError::ProgVarError(const char* message)
 : Error(message)
#if !OGLPLUS_ERROR_NO_PROG_NAME
 , _prog_name(0)
#endif
{ }

OGLPLUS_LIB_FUNC
ProgramName ProgVarError::Program(void) const
{
#if !OGLPLUS_ERROR_NO_PROG_NAME
	return ProgramName(_prog_name);
#else
	return ProgramName();
#endif
}

OGLPLUS_LIB_FUNC
const char* ProgVarError::Identifier(void) const
{
#if !OGLPLUS_ERROR_NO_IDENTIFIER
	if(!_identifier.empty())
		return _identifier.c_str();
#endif
	return nullptr;
}

} // namespace oglplus

