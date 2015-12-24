/**
 *  @file oglplus/uniform_subroutines.ipp
 *  @brief Implementation of Subroutine uniform functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_shader_subroutine

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::Subroutine>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program subroutine";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::Subroutine>::
MsgUsingInactive(void)
{
	return "Using inactive program subroutine";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::SubroutineUniform>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program subroutine uniform";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::SubroutineUniform>::
MsgUsingInactive(void)
{
	return "Using inactive program subroutine uniform";
}

#endif

} // namespace oglplus

