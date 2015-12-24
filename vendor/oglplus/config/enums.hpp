/**
 *  @file oglplus/config/enums.hpp
 *  @brief Enumerations-related compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_ENUMS_1107121519_HPP
#define OGLPLUS_CONFIG_ENUMS_1107121519_HPP

#include <oglplus/config/basic.hpp>

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the functions returning enumerated value names
/** Setting this preprocessor symbol to a nonzero value causes that
 *  the @c EnumValueName(Enum) functions always return an empty string.
 *  When set to zero these functions return a textual name of an enumerated
 *  value passed as argument.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. enumeration value names are enabled, when not in low-profile mode
 *  and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_ENUM_VALUE_NAMES
#else
# ifndef OGLPLUS_NO_ENUM_VALUE_NAMES
#  define OGLPLUS_NO_ENUM_VALUE_NAMES OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the functions returning enumerated value ranges
/** Setting this preprocessor symbol to a nonzero value causes that
 *  the @c EnumValueRange<Enum>() functions always return an empty range.
 *  When set to zero these functions return a range of all values in the
 *  enumeration passed as template argument.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. enumeration value ranges are enabled, when not in low-profile mode
 *  and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_ENUM_VALUE_RANGES
#else
# ifndef OGLPLUS_NO_ENUM_VALUE_RANGES
#  define OGLPLUS_NO_ENUM_VALUE_RANGES OGLPLUS_LOW_PROFILE
# endif
#endif

#ifndef OGLPLUS_NO_ENUM_VALUE_CLASSES
#if OGLPLUS_NO_SCOPED_ENUM_TEMPLATE_PARAMS
#define OGLPLUS_NO_ENUM_VALUE_CLASSES 1
#else
#define OGLPLUS_NO_ENUM_VALUE_CLASSES 0
#endif
#endif

#include <oglplus/detail/enum_class.hpp>

#endif // include guard
