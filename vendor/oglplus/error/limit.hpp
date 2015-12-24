/**
 *  @file oglplus/error/limit.hpp
 *  @brief Limited value error
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ERROR_LIMIT_1405022241_HPP
#define OGLPLUS_ERROR_LIMIT_1405022241_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/error/basic.hpp>

namespace oglplus {

/// Exception indicating exceeded implementation-defined limits
/** Instances of this class are thrown if an instance of a (usually unsigned
 *  integer) type is assigned a value that it is outside of an implementation
 *  dependent range. This includes things like limits on the number of texture
 *  units of the GPU, maximum texture dimensions, maximum number of draw
 *  buffers, vertex attributes, etc.
 *
 *  @ingroup error_handling
 */
class LimitError
 : public Error
{
private:
	GLfloat _value;
	GLfloat _limit;
public:
	static const char* MessageNeg(void);
	static const char* Message(void);

	LimitError(const char* message)
	 : Error(message)
	 , _value(0)
	 , _limit(0)
	{ }

	LimitError& Value(GLfloat value)
	{
		_value = value;
		return *this;
	}

	LimitError& Value(GLuint value)
	{
		_value = static_cast<GLfloat>(value);
		return *this;
	}

	/// The value assigned to the limited-type variable
	GLfloat Value(void) const
	OGLPLUS_OVERRIDE
	{
		return _value;
	}

	LimitError& Limit(GLfloat limit)
	{
		_limit = limit;
		return *this;
	}

	LimitError& Limit(GLuint limit)
	{
		_limit = static_cast<GLfloat>(limit);
		return *this;
	}
	/// The allowed limit of the limited-type
	GLfloat Limit(void) const
	OGLPLUS_OVERRIDE
	{
		return _limit;
	}
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/error/limit.ipp>
#endif

#endif // include guard
