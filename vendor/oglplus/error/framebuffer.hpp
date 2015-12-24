/**
 *  @file oglplus/error/framebuffer.hpp
 *  @brief Framebuffer exceptions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ERROR_FRAMEBUFFER_1405022241_HPP
#define OGLPLUS_ERROR_FRAMEBUFFER_1405022241_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/framebuffer_status.hpp>

namespace oglplus {

// NOTE: Xlib.h defines this symbol
// using the preprocessor.
#ifdef Status
#undef Status
#endif

/// Incomplete framebuffer exception class
/**
 *  @ingroup error_handling
 */
class IncompleteFramebuffer
 : public ObjectError
{
private:
	FramebufferStatus _status;
public:
	static const char* Message(void);

	IncompleteFramebuffer(const char* message)
	 : ObjectError(message)
	{ }

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	IncompleteFramebuffer(const IncompleteFramebuffer&)
		= default;
	IncompleteFramebuffer& operator = (const IncompleteFramebuffer&)
		= default;
#else
	IncompleteFramebuffer(const IncompleteFramebuffer& that)
	 : ObjectError(that)
	 , _status(that._status)
	{ }
#endif

	~IncompleteFramebuffer(void)
	OGLPLUS_NOTHROW
	{ }

	IncompleteFramebuffer& Status(FramebufferStatus status)
	{
		_status = status;
		EnumParam(status);
		return *this;
	}

	FramebufferStatus Status(void) const
	{
		return _status;
	}
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/error/framebuffer.ipp>
#endif

#endif // include guard
