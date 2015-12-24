/**
 *  @file oglplus/native/common_glx.ipp
 *  @brief Declaration of common functions used by GLX object wrappers
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
void HandleNoGLXDisplay(void)
{
	throw std::runtime_error("Failed to get current glX Display");
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void HandleNoGLXContext(void)
{
	throw std::runtime_error("Failed to get current glX Context");
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void HandleNoGLXDrawable(void)
{
	throw std::runtime_error("Failed to get current glX Drawable");
}

} // namespace native
} // namespace oglplus

