/**
 *  @file oglplus/error/glfunc.hpp
 *  @brief Declaration of MissingFunction exception
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ERROR_GLFUNC_1107121317_HPP
#define OGLPLUS_ERROR_GLFUNC_1107121317_HPP

#include <oglplus/error/basic.hpp>

namespace oglplus {

/// Exception class for situations when a pointer to a GL function is invalid.
/** @OGLplus optionally (based on the value of the #OGLPLUS_NO_GLFUNC_CHECKS
 *  compile-time switch) checks, if pointers to OpenGL functions are valid
 *  (i.e. not @c nullptr). OpenGL functions are usually called through pointers,
 *  when using a library such as GLEW, which tries to find and get the addresses
 *  of GL functions from the GL library dynamically at run-time. Sometimes
 *  the pointers to several of the functions remain uninitialized and usually
 *  result in a memory violation and program termination if called.
 *
 *  The MissingFunction exception class indicates that the usage
 *  of such uninitialized function pointer was detected at run-time
 *  and allows the application to terminate more gracefully.
 *
 *  @see #OGLPLUS_NO_GLFUNC_CHECKS
 *
 *  @ingroup error_handling
 */
class MissingFunction
 : public Error
{
public:
	static const char* Message(void);

	MissingFunction(const char* message)
	 : Error(message)
	{ }
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/error/glfunc.ipp>
#endif

#endif // include guard
