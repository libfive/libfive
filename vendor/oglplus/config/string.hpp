/**
 *  @file oglplus/config/string.hpp
 *  @brief String-related compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_STRING_1107121519_HPP
#define OGLPLUS_CONFIG_STRING_1107121519_HPP

#include <oglplus/config/basic.hpp>

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling UTF-8 validity checks in various functions
/** Setting this preprocessor symbol to a nonzero value causes that
 *  the @c StrCRef and @c String constructors skip the UTF-8 validity checks.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. the UTF-8 validity checks are enabled, when not in low-profile mode
 *  and disabled otherwise.
 *
 *  @see String
 *  @see StrCRef
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_UTF8_CHECKS
#else
# ifndef OGLPLUS_NO_UTF8_CHECKS
#  define OGLPLUS_NO_UTF8_CHECKS OGLPLUS_LOW_PROFILE
# endif
#endif

#endif // include guard
