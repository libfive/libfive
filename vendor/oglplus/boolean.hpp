/**
 *  @file oglplus/boolean.hpp
 *  @brief Wrapper for GLboolean
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BOOLEAN_1501311633_HPP
#define OGLPLUS_BOOLEAN_1501311633_HPP

#include <oglplus/detail/boolean.hpp>

namespace oglplus {

typedef BoolImpl<GLboolean, GLint, GL_TRUE, GL_FALSE> Boolean;

} // namespace oglplus

#endif // include guard
