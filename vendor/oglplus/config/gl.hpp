/**
 *  @file oglplus/config/gl.hpp
 *  @brief OpenGL-related Compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_GL_1107121519_HPP
#define OGLPLUS_CONFIG_GL_1107121519_HPP

#include <oglplus/config/basic.hpp>

#ifndef OGLPLUS_USE_GLEW
#define OGLPLUS_USE_GLEW 0
#endif

#ifndef OGLPLUS_USE_FREEGLUT
#define OGLPLUS_USE_FREEGLUT 0
#endif

// define GLAPIENTRY
#ifdef GLAPIENTRY
#undef GLAPIENTRY
#endif
// borrowed from glew.h which does define GLAPIENTRY properly
// at the beginning but undefs it at the end of the header
#if defined(__MINGW32__) || defined(__CYGWIN__)
#  define GLAPIENTRY __stdcall
#elif (defined(_MSC_VER) && _MSC_VER >= 800) ||\
	defined(_STDCALL_SUPPORTED) ||\
	defined(__BORLANDC__)
#  define GLAPIENTRY __stdcall
#else
#  define GLAPIENTRY
#endif


#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling checks of validity of pointers to functions
/** Setting this preprocessor symbol to a nonzero value causes that
 *  if the OpenGL functions are called through a pointer then the pointer
 *  is checked before it is used to call the function. If enabled and a pointer
 *  to GL function is nullptr then the MissingFunction exception is thrown.
 *
 *  This check can safely be disabled if functions from the GL API are
 *  not called through pointers.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. the function pointer checks are enabled, when not in low-profile mode,
 *  and disabled otherwise. The check however requires variadic templates.
 *  If variadic templates are not available then the checks are disabled.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_GLFUNC_CHECKS
#else
# ifndef OGLPLUS_NO_GLFUNC_CHECKS
#  define OGLPLUS_NO_GLFUNC_CHECKS OGLPLUS_LOW_PROFILE
# endif
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch entirely disabling typechecking of uniforms.
/** Setting this preprocessor symbol to a nonzero value causes that
 *  even the Uniform variables that are declared with UniformTypecheckingLevel
 *  other than None, are not typechecked.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. typechecking of uniforms is enabled when not in low-profile mode,
 *  and disabled otherwise.
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_UNIFORM_TYPECHECK
#else
# ifndef OGLPLUS_NO_UNIFORM_TYPECHECK
#  define OGLPLUS_NO_UNIFORM_TYPECHECK OGLPLUS_LOW_PROFILE
# endif
#endif

#endif // include guard
