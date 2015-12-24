/**
 *  @file oglplus/capability.hpp
 *  @brief Enumeration of OpenGL capabilities that can be Enabled/Disabled
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CAPABILITY_1107121519_HPP
#define OGLPLUS_CAPABILITY_1107121519_HPP

#include <oglplus/error/basic.hpp>
#include <oglplus/enums/capability.hpp>
#include <oglplus/enums/functionality.hpp>

namespace oglplus {

#if !OGLPLUS_NO_ENUM_VALUE_CLASSES
#include <oglplus/enums/capability_class.ipp>
#endif

inline void operator << (Capability capability, bool enable)
{
	if(enable)
	{
		OGLPLUS_GLFUNC(Enable)(GLenum(capability));
		OGLPLUS_VERIFY_SIMPLE(Enable);
	}
	else
	{
		OGLPLUS_GLFUNC(Disable)(GLenum(capability));
		OGLPLUS_VERIFY_SIMPLE(Disable);
	}
}

inline void operator + (Capability capability)
{
	OGLPLUS_GLFUNC(Enable)(GLenum(capability));
	OGLPLUS_VERIFY_SIMPLE(Enable);
}

inline void operator - (Capability capability)
{
	OGLPLUS_GLFUNC(Disable)(GLenum(capability));
	OGLPLUS_VERIFY_SIMPLE(Disable);
}

struct FunctionalityAndNumber
{
	GLenum _code;
};

inline FunctionalityAndNumber operator | (Functionality func, GLuint number)
{
	FunctionalityAndNumber result = { GLenum(func)+number };
	return result;
}

inline void operator << (FunctionalityAndNumber func_and_num, bool enable)
{
	if(enable)
	{
		OGLPLUS_GLFUNC(Enable)(func_and_num._code);
		OGLPLUS_VERIFY_SIMPLE(Enable);
	}
	else
	{
		OGLPLUS_GLFUNC(Disable)(func_and_num._code);
		OGLPLUS_VERIFY_SIMPLE(Disable);
	}
}

inline void operator + (FunctionalityAndNumber func_and_num)
{
	OGLPLUS_GLFUNC(Enable)(func_and_num._code);
	OGLPLUS_VERIFY_SIMPLE(Enable);
}

inline void operator - (FunctionalityAndNumber func_and_num)
{
	OGLPLUS_GLFUNC(Disable)(func_and_num._code);
	OGLPLUS_VERIFY_SIMPLE(Disable);
}

} // namespace oglplus

#endif // include guard
