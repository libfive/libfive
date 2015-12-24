/**
 *  @file oglplus/text/font2d.hpp
 *  @brief The default font for 2D text rendering
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_FONT2D_HPP
#define OGLPLUS_TEXT_FONT2D_HPP

#include <oglplus/text/stb_truetype/font2d.hpp>

namespace oglplus {
namespace text {

/// The default font for 2D text rendering
typedef STBTTFont2D Font2D;

} // namespace text
} // namespace oglplus

#endif // include guard
