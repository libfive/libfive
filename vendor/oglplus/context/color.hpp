/**
 *  @file oglplus/context/color.hpp
 *  @brief Color value and mask vwrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_COLOR_1412170813_HPP
#define OGLPLUS_CONTEXT_COLOR_1412170813_HPP

#include <oglplus/boolean.hpp>

namespace oglplus {
namespace context {

/// Helper structure storing the clear color components
struct RGBAValue
{
	// private implementation detail, do not use
	GLfloat _v[4];

	RGBAValue(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	RGBAValue(GLfloat r, GLfloat g, GLfloat b, GLfloat a)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = r;
		_v[1] = g;
		_v[2] = b;
		_v[3] = a;
	}

	/// The red component
	GLfloat Red(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0];
	}

	/// The green component
	GLfloat Green(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[1];
	}

	/// The blue component
	GLfloat Blue(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[2];
	}

	/// The alpha component
	GLfloat Alpha(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[3];
	}

	friend
	bool operator == (const RGBAValue& a, const RGBAValue& b)
	OGLPLUS_NOEXCEPT(true)
	{
		for(unsigned i=0; i<4; ++i)
		{
			if((a._v[i] < b._v[i]) || (a._v[i] > b._v[i]))
			{
				return false;
			}
		}
		return true;
	}

	friend
	bool operator != (const RGBAValue& a, const RGBAValue& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return !(a == b);
	}
};

/// Helper structure storing the clear color component mask
struct RGBAMask
{
	// private implementation detail, do not use
	GLint _v[4];

	RGBAMask(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	RGBAMask(Boolean r, Boolean g, Boolean b, Boolean a)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = r._get();
		_v[1] = g._get();
		_v[2] = b._get();
		_v[3] = a._get();
	}

	/// The red component mask
	Boolean Red(void) const
	{
		return Boolean(_v[0], std::nothrow);
	}

	/// The green component mask
	Boolean Green(void) const
	{
		return Boolean(_v[1], std::nothrow);
	}

	/// The blue component mask
	Boolean Blue(void) const
	{
		return Boolean(_v[2], std::nothrow);
	}

	/// The alpha component mask
	Boolean Alpha(void) const
	{
		return Boolean(_v[3], std::nothrow);
	}

	friend
	bool operator == (const RGBAMask& a, const RGBAMask& b)
	OGLPLUS_NOEXCEPT(true)
	{
		for(unsigned i=0; i<4; ++i)
		{
			if(a._v[i] != b._v[i])
			{
				return false;
			}
		}
		return true;
	}

	friend
	bool operator != (const RGBAMask& a, const RGBAMask& b)
	OGLPLUS_NOEXCEPT(true)
	{
		return !(a == b);
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard
