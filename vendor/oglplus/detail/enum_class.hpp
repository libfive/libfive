/**
 *  @file oglplus/detail/enum_class.hpp
 *  @brief Helper macros for declaring enumerations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_ENUM_CLASS_1207191556_HPP
#define OGLPLUS_AUX_ENUM_CLASS_1207191556_HPP

#include <oglplus/config/compiler.hpp>

namespace oglplus {

#define OGLPLUS_ENUM_CLASS_FWD(NAME, TYPE) \
namespace enums { \
enum class NAME : TYPE; \
} using enums::NAME;

#define OGLPLUS_ENUM_CLASS_BEGIN(NAME, TYPE) \
namespace enums { \
enum class NAME : TYPE {

#define OGLPLUS_ENUM_CLASS_VALUE(ITEM, VALUE) \
	ITEM = VALUE

#define OGLPLUS_ENUM_CLASS_COMMA ,

#define OGLPLUS_ENUM_CLASS_END(NAME) \
}; } \
using enums::NAME;

namespace enums {

template <typename Enum>
struct EnumBaseType;

} // namespace enums
} // namespace oglplus

#endif // include guard
