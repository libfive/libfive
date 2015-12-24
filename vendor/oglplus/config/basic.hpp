/**
 *  @file oglplus/config/basic.hpp
 *  @brief Basic compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_BASIC_1107121519_HPP
#define OGLPLUS_CONFIG_BASIC_1107121519_HPP

#ifndef OGLPLUS_NO_SITE_CONFIG
#include <oglplus/config/site.hpp>
#endif

/** @defgroup compile_time_config Compile-time configuration
 *
 *  @section oglplus_configuration_options Configuration options
 *
 *  This section describes compile-time preprocessor symbols that
 *  can be used to configure several aspects of @OGLplus.
 *
 *  Most of the options are set either to a zero or a non-zero
 *  integer value to disable or enable the behavior controlled
 *  by the option.
 *
 *  All options have a default value which can be overriden by
 *  setting the option before @c oglplus/config.hpp is processed
 *  either by editing the @c oglplus/config/site.hpp file or by
 *  using the @c -D compiler option (or its equivalent for defining
 *  preprocessor symbols on the command-line).
 */

#ifndef OGLPLUS_DOCUMENTATION_ONLY
#define OGLPLUS_DOCUMENTATION_ONLY 0
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time option enabling the low-profile mode
/** In the low-profile mode some features (like object descriptions,
 *  enumeration value names, some file and line info atached to exceptions,
 *  etc. useful during development and debugging) are disabled, resulting
 *  in both smaller binary executable sizes and lower run-time overhead
 *  for release builds of applications using @OGLplus.
 *
 *  Setting this option to a non-zero integer value enables the low-profile
 *  mode, setting it to zero disables it. By default the low-profile mode
 *  is disabled.
 *
 *  This option influences the default value of several other configuration
 *  options. Their values can of course be set independently if required.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_LOW_PROFILE
#else
# ifndef OGLPLUS_LOW_PROFILE
#  define OGLPLUS_LOW_PROFILE 0
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch enabling linking of some parts of OGLplus from a library.
/** Certain parts of OGLplus (mostly complicated functions and functions where
 *  static variables are used) can be built into a separate library that can
 *  be then linked to applications. If the @c OGLPLUS_LINK_LIBRARY is set to
 *  zero, then everything is inlined. If it is set to a nonzero integer value,
 *  then some functions are just declared and must be built separatelly and
 *  linked to the final executable.
 *
 *  The @c oglplus/lib.hpp file can be included when building the library
 *  or the final executable to include the implementations of all such functions.
 *
 *  By default this option is set to 0.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_LINK_LIBRARY
#else
# ifndef OGLPLUS_LINK_LIBRARY
#  define OGLPLUS_LINK_LIBRARY 0
# endif
#endif

#if OGLPLUS_LINK_LIBRARY
# define OGLPLUS_LIB_FUNC
#else
# define OGLPLUS_LIB_FUNC inline
#endif

#endif // include guard
