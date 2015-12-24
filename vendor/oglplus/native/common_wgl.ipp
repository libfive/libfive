/**
 *  @file oglplus/native/common_wgl.ipp
 *  @brief Declaration of common functions used by WGL object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <stdexcept>

namespace oglplus {
namespace native {

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void HandleNoWGLDC(void)
{
	throw std::runtime_error("Failed to get current WGL Device Context");
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void HandleNoWGLRC(void)
{
	throw std::runtime_error("Failed to get current WGL Rendering Context");
}

} // namespace native
} // namespace oglplus

