/**
 *  @file oglplus/config/error.hpp
 *  @brief Error reporting-related compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_ERROR_1107121519_HPP
#define OGLPLUS_CONFIG_ERROR_1107121519_HPP

#include <oglplus/config/object.hpp>
#include <oglplus/config/enums.hpp>

#ifndef OGLPLUS_ERROR_NO_GL_LIB
# define OGLPLUS_ERROR_NO_GL_LIB 0
#endif

#ifndef OGLPLUS_ERROR_NO_GL_FUNC
# define OGLPLUS_ERROR_NO_GL_FUNC 0
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorGLSymbol attribute of ErrorInfo
/**
 *  @see Error::GLFunc()
 *
 *  By default this option is set to 0, i.e. Error::GLFunc is enabled.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_GL_SYMBOL
#else
# ifndef OGLPLUS_ERROR_NO_GL_SYMBOL
#  define OGLPLUS_ERROR_NO_GL_SYMBOL 0
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorFile attribute of ErrorInfo
/**
 *  @see ErrorFile()
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. ErrorFile is enabled, when not in low-profile and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_FILE
#else
# ifndef OGLPLUS_ERROR_NO_FILE
#  define OGLPLUS_ERROR_NO_FILE OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorLine attribute of ErrorInfo
/**
 *  @see ErrorLine()
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. ErrorLine is enabled, when not in low-profile and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_LINE OGLPLUS_LOW_PROFILE
#else
# ifndef OGLPLUS_ERROR_NO_LINE
#  define OGLPLUS_ERROR_NO_LINE OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorFunc attribute of ErrorInfo
/**
 *  @see ErrorFunc()
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. ErrorFinc is enabled, when not in low-profile and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_FUNC
#else
# ifndef OGLPLUS_ERROR_NO_FUNC
#  define OGLPLUS_ERROR_NO_FUNC OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ObjectTypeName attribute of ErrorInfo
/**
 *
 *  By default this option is set to 0, i.e. ErrorClassName is enabled.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_CLASS_NAME
#else
# ifndef OGLPLUS_ERROR_NO_CLASS_NAME
#  define OGLPLUS_ERROR_NO_CLASS_NAME 0
# endif
#endif

#if OGLPLUS_NO_ENUM_VALUE_NAMES
#ifdef OGLPLUS_ERROR_NO_TARGET_NAME
#undef OGLPLUS_ERROR_NO_TARGET_NAME
#endif
#define OGLPLUS_ERROR_NO_TARGET_NAME 1
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorBindTarget attribute of ErrorInfo
/**
 *  @see ErrorBindTarget()
 *
 *  By default this option is set to 0, i.e. ErrorBindTarget is enabled,
 *  unless #OGLPLUS_NO_ENUM_VALUE_NAMES is set to a non-zero value,
 *  in which case ErrorBindTarget is disabled and returns an empty string.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_TARGET_NAME
#else
# ifndef OGLPLUS_ERROR_NO_TARGET_NAME
#  define OGLPLUS_ERROR_NO_TARGET_NAME 0
# endif
#endif

#if defined(OGLPLUS_NO_OBJECT_DESCS) && OGLPLUS_NO_OBJECT_DESCS
#ifdef OGLPLUS_ERROR_NO_OBJECT_DESC
#undef OGLPLUS_ERROR_NO_OBJECT_DESC
#endif
#define OGLPLUS_ERROR_NO_OBJECT_DESC 1
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ErrorObjectDescription attribute of ErrorInfo
/**
 *  @see ErrorObjectDescription()
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. ErrorObjectDescription is enabled, when not in low-profile and disabled
 *  otherwise and returns an empty string.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_OBJECT_DESC
#else
# ifndef OGLPLUS_ERROR_NO_OBJECT_DESC
#  define OGLPLUS_ERROR_NO_OBJECT_DESC OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the ObjectType attribute of ObjectError
/**
 *  Defaults to 0
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_OBJECT_TYPE
#else
# ifndef OGLPLUS_ERROR_NO_OBJECT_TYPE
#  define OGLPLUS_ERROR_NO_OBJECT_TYPE 0
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the BindTarget attribute of ObjectError
/**
 *  Defaults to 0
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_BIND_TARGET
#else
# ifndef OGLPLUS_ERROR_NO_BIND_TARGET
#  define OGLPLUS_ERROR_NO_BIND_TARGET 0
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the Program attribute of ProgVarError
/**
 *  Defaults to 0
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_PROG_NAME
#else
# ifndef OGLPLUS_ERROR_NO_PROG_NAME
#  define OGLPLUS_ERROR_NO_PROG_NAME 0
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling the Identifier attribute of ProgVarError
/**
 *  Defaults to 0
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_ERROR_NO_IDENTIFIER
#else
# ifndef OGLPLUS_ERROR_NO_IDENTIFIER
#  define OGLPLUS_ERROR_NO_IDENTIFIER 0
# endif
#endif

#endif // include guard
