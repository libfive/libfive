/**
 *  @file oglplus/lib/text_stb_truetype.hpp
 *  @brief All-in-one include file for the separatelly-built STBTT functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_LIB_TEXT_STB_TRUETYPE_1208310818_HPP
#define OGLPLUS_LIB_TEXT_STB_TRUETYPE_1208310818_HPP

#if !OGLPLUS_NO_VARIADIC_TEMPLATES && GL_VERSION_3_1
#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects || GL_EXT_direct_state_access

#ifndef OGLPLUS_IMPLEMENTING_LIBRARY
#define OGLPLUS_IMPLEMENTING_LIBRARY 1
#endif

#include <oglplus/text/stb_truetype.hpp>

#undef OGLPLUS_IMPLEMENTING_LIBRARY

#endif
#endif // OGLPLUS_NO_VARIADIC_TEMPLATES && GL_VERSION

#endif // include guard
