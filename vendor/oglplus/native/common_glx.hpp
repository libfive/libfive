/**
 *  @file oglplus/native/common_glx.hpp
 *  @brief Declaration of common functions used by GLX object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_NATIVE_COMMON_GLX_1404232057_HPP
#define OGLPLUS_NATIVE_COMMON_GLX_1404232057_HPP

#include <oglplus/config/compiler.hpp>

namespace oglplus {
namespace native {

OGLPLUS_NORETURN
void HandleNoGLXDisplay(void);

OGLPLUS_NORETURN
void HandleNoGLXContext(void);

OGLPLUS_NORETURN
void HandleNoGLXDrawable(void);

} // namespace native
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/native/common_glx.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
