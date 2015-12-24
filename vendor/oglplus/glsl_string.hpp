/**
 *  @file oglplus/glsl_string.hpp
 *  @brief Helper classes for passing source code in GLSL as parameters
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_GLSL_STRING_1207111232_HPP
#define OGLPLUS_GLSL_STRING_1207111232_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/size_type.hpp>

#include <cassert>
#include <utility>
#include <vector>
#include <array>
#if !OGLPLUS_NO_INITIALIZER_LISTS
#include <initializer_list>
#endif

namespace oglplus {

/// Class for passing a single string as a Source to a Shader
class GLSLString
{
private:
	const GLchar* _str;
	GLint _len;

	GLSLString(const GLSLString&);
public:
	GLSLString(const GLchar* str)
	OGLPLUS_NOEXCEPT(true)
	 : _str(str)
	 , _len(-1)
	{ }

	template <std::size_t N>
	GLSLString(const GLchar (&str)[N])
	OGLPLUS_NOEXCEPT(true)
	 : _str(str)
	 , _len(N)
	{ }

	GLSLString(const StrCRef& str)
	 : _str(str.begin())
	 , _len(GLint(str.size()))
	{ }

	GLSLString(const String& str)
	 : _str(str.c_str())
	 , _len(GLint(str.size()))
	{ }

	GLSLString(const std::vector<GLchar>& v)
	 : _str(v.data())
	 , _len(GLint(v.size()))
	{ }

	template <std::size_t N>
	GLSLString(const std::array<GLchar, N>& a)
	 : _str(a.data())
	 , _len(GLint(a.size()))
	{ }

	SizeType Count(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return 1;
	}

	const GLchar* const* Parts(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return &_str;
	}

	GLint const * Lengths(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return (_len<0)?(nullptr):(&_len);
	}
};


/// Class for passing a set of strings as a Source to a Shader
class GLSLStrings
{
private:
	GLsizei _count;
	const GLchar* const* _strs;
	const GLint* _lens;

	GLSLStrings(const GLSLStrings&);
public:
	GLSLStrings(SizeType count, const GLchar* const * strs)
	OGLPLUS_NOEXCEPT(true)
	 : _count(count)
	 , _strs(strs)
	 , _lens(nullptr)
	{ }

	GLSLStrings(SizeType count, const GLchar* const* strs, const GLint* lens)
	OGLPLUS_NOEXCEPT(true)
	 : _count(count)
	 , _strs(strs)
	 , _lens(lens)
	{ }

	template <std::size_t N>
	GLSLStrings(const GLchar* (&strs)[N])
	OGLPLUS_NOEXCEPT(true)
	 : _count(GLsizei(N))
	 , _strs(strs)
	 , _lens(nullptr)
	{ }

	GLSLStrings(const std::vector<const GLchar*>& v)
	 : _count(GLsizei(v.size()))
	 , _strs(v.data())
	 , _lens(nullptr)
	{ }

	template <std::size_t N>
	GLSLStrings(const std::array<const GLchar*, N>& a)
	 : _count(GLsizei(a.size()))
	 , _strs(a.data())
	 , _lens(nullptr)
	{ }

#if !OGLPLUS_NO_INITIALIZER_LISTS
	GLSLStrings(const std::initializer_list<const GLchar*>& l)
	 : _count(GLsizei(l.size()))
	 , _strs(l.begin())
	 , _lens(nullptr)
	{ }
#endif

	SizeType Count(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _count;
	}

	const GLchar* const* Parts(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _strs;
	}

	GLint const * Lengths(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _lens;
	}
};

} // namespace oglplus

#endif // include guard
