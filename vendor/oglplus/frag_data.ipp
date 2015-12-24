/**
 *  @file oglplus/frag_data.ipp
 *  @brief Implementation of FragData
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::FragData>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program fragment data output";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::FragData>::
MsgUsingInactive(void)
{
	return "Using inactive program fragment data output";
}

} // namespace oglplus

