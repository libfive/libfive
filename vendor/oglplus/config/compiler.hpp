/**
 *  @file oglplus/config/compiler.hpp
 *  @brief Configuration options based on compiler capabilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_COMPILER_1107121519_HPP
#define OGLPLUS_CONFIG_COMPILER_1107121519_HPP

#ifndef OGLPLUS_NO_SITE_CONFIG
#include <oglplus/config/site.hpp>
#endif

#ifndef OGLPLUS_USE_BOOST_CONFIG
#define OGLPLUS_USE_BOOST_CONFIG 0
#endif

#if OGLPLUS_USE_BOOST_CONFIG
#include <boost/config.hpp>
#endif

// ------- C++11 feature availability detection -------

#ifndef OGLPLUS_NO_VARIADIC_MACROS
#if	defined(BOOST_NO_CXX11_VARIADIC_MACROS) ||\
	defined(BOOST_NO_VARIADIC_MACROS)
#define OGLPLUS_NO_VARIADIC_MACROS 1
#else
#define OGLPLUS_NO_VARIADIC_MACROS 0
#endif
#endif

#ifndef OGLPLUS_NO_VARIADIC_TEMPLATES
#if	defined(BOOST_NO_CXX11_VARIADIC_TEMPLATES) ||\
	defined(BOOST_NO_VARIADIC_TEMPLATES) ||\
	defined(_MSC_VER)
#define OGLPLUS_NO_VARIADIC_TEMPLATES 1
#else
#define OGLPLUS_NO_VARIADIC_TEMPLATES 0
#endif
#endif

#ifndef OGLPLUS_NO_UNIFIED_INITIALIZATION_SYNTAX
#if	defined(BOOST_NO_CXX11_UNIFIED_INITIALIZATION_SYNTAX) ||\
	defined(BOOST_NO_UNIFIED_INITIALIZATION_SYNTAX)
#define OGLPLUS_NO_UNIFIED_INITIALIZATION_SYNTAX 1
#else
#define OGLPLUS_NO_UNIFIED_INITIALIZATION_SYNTAX 0
#endif
#endif

#ifndef OGLPLUS_NO_INITIALIZER_LISTS
#if	defined(BOOST_NO_CXX11_INITIALIZER_LISTS) ||\
	defined(BOOST_NO_INITIALIZER_LISTS)
#define OGLPLUS_NO_INITIALIZER_LISTS 1
#else
#define OGLPLUS_NO_INITIALIZER_LISTS 0
#endif
#endif

#ifndef OGLPLUS_NO_DEFAULTED_FUNCTIONS
#if	defined(BOOST_NO_CXX11_DEFAULTED_FUNCTIONS) ||\
	defined(BOOST_NO_DEFAULTED_FUNCTIONS) ||\
	defined(_MSC_VER)
#define OGLPLUS_NO_DEFAULTED_FUNCTIONS 1
#else
#define OGLPLUS_NO_DEFAULTED_FUNCTIONS 0
#endif
#endif

#ifndef OGLPLUS_NO_DELETED_FUNCTIONS
#if	defined(BOOST_NO_CXX11_DELETED_FUNCTIONS) ||\
	defined(BOOST_NO_DELETED_FUNCTIONS)
#define OGLPLUS_NO_DELETED_FUNCTIONS 1
#else
#define OGLPLUS_NO_DELETED_FUNCTIONS 0
#endif
#endif

#ifndef OGLPLUS_NO_EXPLICIT_CONVERSION_OPERATORS
#if	defined(BOOST_NO_CXX11_EXPLICIT_CONVERSION_OPERATORS) ||\
	defined(BOOST_NO_EXPLICIT_CONVERSION_OPERATORS)
#define OGLPLUS_NO_EXPLICIT_CONVERSION_OPERATORS 1
#else
#define OGLPLUS_NO_EXPLICIT_CONVERSION_OPERATORS 0
#endif
#endif

#ifndef OGLPLUS_NO_EXPLICIT_CONVERSION_OPERATORS
#define OGLPLUS_EXPLICIT explicit
#else
#define OGLPLUS_EXPLICIT
#endif

#ifndef OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS
#if	defined(BOOST_NO_CXX11_FUNCTION_TEMPLATE_DEFAULT_ARGS) ||\
	defined(BOOST_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS)
#define OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS 1
#else
#define OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS 0
#endif
#endif

#ifndef OGLPLUS_NO_UNICODE_LITERALS
#if	defined(BOOST_NO_CXX11_UNICODE_LITERALS) ||\
	defined(BOOST_NO_UNICODE_LITERALS)
#define OGLPLUS_NO_UNICODE_LITERALS 1
#else
#define OGLPLUS_NO_UNICODE_LITERALS 0
#endif
#endif

#ifndef OGLPLUS_NO_USER_DEFINED_LITERALS
/* TODO this macro is not (yet) available in Boost.
 * Update this if/when it is implemented
 */
#if	defined(BOOST_NO_CXX11_USER_DEFINED_LITERALS) ||\
	defined(BOOST_NO_USER_DEFINED_LITERALS)
#define OGLPLUS_NO_USER_DEFINED_LITERALS 1
#else
#define OGLPLUS_NO_USER_DEFINED_LITERALS 0
#endif
#endif

#ifndef OGLPLUS_NO_TEMPLATE_ALIASES
#if	defined(BOOST_NO_CXX11_TEMPLATE_ALIASES) ||\
	defined(BOOST_NO_TEMPLATE_ALIASES)
#define OGLPLUS_NO_TEMPLATE_ALIASES 1
#else
#define OGLPLUS_NO_TEMPLATE_ALIASES 0
#endif
#endif

#ifndef OGLPLUS_NO_INHERITED_CONSTRUCTORS
/* TODO this macro is not (yet) available in Boost.
 * Update this if/when it is implemented
 */
#if	defined(BOOST_NO_CXX11_INHERITED_CONSTRUCTORS) ||\
	defined(BOOST_NO_INHERITED_CONSTRUCTORS) ||\
	defined(_MSC_VER)
#define OGLPLUS_NO_INHERITED_CONSTRUCTORS 1
#else
#define OGLPLUS_NO_INHERITED_CONSTRUCTORS 0
#endif
#endif

#ifndef OGLPLUS_NO_CONSTEXPR
#if	defined(BOOST_NO_CXX11_CONSTEXPR) ||\
	defined(BOOST_NO_CONSTEXPR)
#define OGLPLUS_NO_CONSTEXPR 1
#else
#define OGLPLUS_NO_CONSTEXPR 0
#endif
#endif

#ifndef OGLPLUS_NO_OVERRIDE
#if	defined(BOOST_NO_CXX11_OVERRIDE) ||\
	defined(BOOST_NO_OVERRIDER)
#define OGLPLUS_NO_OVERRIDE 1
#else
#define OGLPLUS_NO_OVERRIDE 0
#endif
#endif

#ifndef OGLPLUS_NO_NOEXCEPT
#if	defined(BOOST_NO_CXX11_NOEXCEPT) ||\
	defined(BOOST_NO_NOEXCEPT)
#define OGLPLUS_NO_NOEXCEPT 1
#else
#define OGLPLUS_NO_NOEXCEPT 0
#endif
#endif

#ifndef OGLPLUS_NO_GENERALIZED_ATTRIBUTES
#define OGLPLUS_NO_GENERALIZED_ATTRIBUTES 1
#endif

#ifndef OGLPLUS_NO_LAMBDAS
#if	defined(BOOST_NO_CXX11_LAMBDAS) ||\
	defined(BOOST_NO_LAMBDAS)
#define OGLPLUS_NO_LAMBDAS 1
#else
#define OGLPLUS_NO_LAMBDAS 0
#endif
#endif

#ifndef OGLPLUS_NO_CHRONO
#if	defined(BOOST_NO_CXX11_HDR_CHRONO) ||\
	defined(BOOST_NO_HDR_CHRONO)
#define OGLPLUS_NO_CHRONO 1
#else
#define OGLPLUS_NO_CHRONO 0
#endif
#endif

#ifndef OGLPLUS_NO_SCOPED_ENUM_TEMPLATE_PARAMS
#ifdef _MSC_VER // TODO < specific version
#define OGLPLUS_NO_SCOPED_ENUM_TEMPLATE_PARAMS 1
#else
#define OGLPLUS_NO_SCOPED_ENUM_TEMPLATE_PARAMS 0
#endif
#endif

// ------- C++11 feature availability detection -------

#if defined(OGLPLUS_NO_NULLPTR) && OGLPLUS_NO_NULLPTR
#define nullptr 0
#endif

#if !OGLPLUS_NO_CONSTEXPR
#define OGLPLUS_CONSTEXPR constexpr
#else
#define OGLPLUS_CONSTEXPR const
#endif

#if !OGLPLUS_NO_OVERRIDE
#define OGLPLUS_OVERRIDE override
#else
#define OGLPLUS_OVERRIDE
#endif

#if !OGLPLUS_NO_NOEXCEPT
#define OGLPLUS_NOEXCEPT(...) noexcept(__VA_ARGS__)
#define OGLPLUS_NOEXCEPT_IF(...) noexcept(noexcept(__VA_ARGS__))
#define OGLPLUS_NOTHROW noexcept
#else
#define OGLPLUS_NOEXCEPT(...)
#define OGLPLUS_NOEXCEPT_IF(...)
#define OGLPLUS_NOTHROW throw()
#endif

#if !OGLPLUS_NO_GENERALIZED_ATTRIBUTES
#define OGLPLUS_NORETURN [[noreturn]]
#if defined(__clang__)
#define OGLPLUS_FALLTHROUGH [[clang::fallthrough]];
#endif
#else
#define OGLPLUS_NORETURN
#endif

#ifndef OGLPLUS_FALLTHROUGH
#define OGLPLUS_FALLTHROUGH
#endif

// -------- disable certain warnings ---------

#ifdef _MSC_VER
#pragma warning( disable : 4396 )
#endif //_MSC_VER

// -------- helper definitions ---------

#ifndef OGLPLUS_DOCUMENTATION_ONLY
#define OGLPLUS_DOCUMENTATION_ONLY 0
#endif

// helper macro that marks unused parameters
// so that the compiler does not complain
#define OGLPLUS_FAKE_USE(X) (void)X

#endif // include guard
