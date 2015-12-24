/**
 *  @file oglplus/string/def.hpp
 *  @brief String type definition and related functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_STRING_DEF_1107121519_HPP
#define OGLPLUS_STRING_DEF_1107121519_HPP

#include <string>

namespace oglplus {

/** @defgroup oglplus_strings Strings
 *
 *  Classes, types and functions in this group are related to text string
 *  and/or string literal handling.
 */

/// Checks if the range between @p begin and @p end is valid UTF-8 sequence
/**
 *  @ingroup oglplus_strings
 */
bool ValidString(const char* begin, const char* end);

/// String class
/**
 *  @ingroup oglplus_strings
 */
typedef ::std::basic_string<GLchar> String;

const String& EmptyString(void);

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/string/def.ipp>
#endif

#endif // include guard
