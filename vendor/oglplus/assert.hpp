/**
 *  @file oglplus/assert.hpp
 *  @brief Assertions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ASSERT_1201201108_HPP
#define OGLPLUS_ASSERT_1201201108_HPP

#include <cassert>

#define OGLPLUS_ABORT(STR) assert(!bool(STR))

#endif // include guard
