/**
 *  @file oglplus/string/empty.hpp
 *  @brief Empty std::string
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_STRING_EMPTY_1107121519_HPP
#define OGLPLUS_STRING_EMPTY_1107121519_HPP

#include <string>

namespace oglplus {

const std::string& EmptyStdString(void);

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/string/empty.ipp>
#endif

#endif // include guard
