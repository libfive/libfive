/**
 *  @file oglplus/named_string.hpp
 *  @brief NamedString wrapper class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_NAMED_STRING_1202231207_HPP
#define OGLPLUS_NAMED_STRING_1202231207_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_shading_language_include

#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/named_string_type.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/string/def.hpp>

namespace oglplus {

/// Wrapper for GL NamedStrings
class NamedString
{
private:
	String _str_name;

	NamedString(const NamedString&);
public:
	/// Store the @p value, of the specified @p type under @p str_name
	static void Set(
		NamedStringType type,
		const StrCRef& str_name,
		const StrCRef& value
	)
	{
		OGLPLUS_GLFUNC(NamedStringARB)(
			GLenum(type),
			GLint(str_name.size()),
			str_name.c_str(),
			GLint(value.size()),
			value.c_str()
		);
		OGLPLUS_CHECK_SIMPLE(NamedStringARB);
	}

	/// Gets the value stored under @p str_name
	static String Get(const StrCRef& str_name)
	{
		GLint len = 0;
		OGLPLUS_GLFUNC(GetNamedStringivARB)(
			GLint(str_name.size()),
			str_name.c_str(),
			GL_NAMED_STRING_LENGTH_ARB,
			&len
		);
		OGLPLUS_CHECK_SIMPLE(GetNamedStringivARB);
		assert(!(len < 0));

		String result(std::size_t(len), '\0');
		OGLPLUS_GLFUNC(GetNamedStringARB)(
			GLint(str_name.size()),
			str_name.c_str(),
			len,
			&len,
			&result.front()
		);
		OGLPLUS_CHECK_SIMPLE(GetNamedStringARB);
		return std::move(result);

	}

	/// Deletes the value stored under @p str_name
	static void Delete(const StrCRef& str_name)
	{
		OGLPLUS_GLFUNC(DeleteNamedStringARB)(
			GLint(str_name.size()),
			str_name.c_str()
		);
		OGLPLUS_CHECK_SIMPLE(DeleteNamedStringARB);
	}

	/// Gets the type of the named string stored under @p str_name
	static NamedStringType Type(const StrCRef& str_name)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetNamedStringivARB)(
			GLint(str_name.size()),
			str_name.c_str(),
			GL_NAMED_STRING_TYPE_ARB,
			&result
		);
		OGLPLUS_CHECK_SIMPLE(GetNamedStringivARB);
		return NamedStringType(GLenum(result));
	}

	/// Checks if @p str_name is a stored string
	static Boolean IsA(const StrCRef& str_name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsNamedStringARB)(
				GLint(str_name.size()),
				str_name.c_str()
			), std::nothrow
		);
		OGLPLUS_CHECK_SIMPLE(IsNamedStringARB);
		return result;
	}

	/// Sets the @p value of the specified @p type in this NamedString
	void Set(NamedStringType type, const StrCRef& value)
	{
		Set(type, _str_name, value);
	}

	/// Sets the @p value of this NamedString
	String Get(void) const
	{
		return Get(_str_name);
	}

	/// Move-construction
	NamedString(NamedString&& tmp)
	 : _str_name(std::move(tmp._str_name))
	{ }

	/// Store a string @p value of the specified type under @p str_name
	NamedString(
		NamedStringType type,
		String&& str_name,
		const StrCRef& value
	): _str_name(std::move(str_name))
	{
		Set(type, value);
	}

	/// Delete this named string
	~NamedString(void)
	{
		if(!_str_name.empty())
		{
			Delete(_str_name);
		}
	}
};

/// Specialization of NamedString for ShaderInclude type.
class ShaderInclude
 : public NamedString
{
public:
	/// Create a shader include with the specified str_name and value
	ShaderInclude(String&& str_name, const StrCRef& value)
	 : NamedString(
		NamedStringType::ShaderInclude,
		std::move(str_name),
		value
	){ }

	ShaderInclude(ShaderInclude&& tmp)
	 : NamedString(static_cast<NamedString&&>(tmp))
	{ }

	/// Set a new value for this shader include
	void Set(const StrCRef& value)
	{
		return NamedString::Set(NamedStringType::ShaderInclude, value);
	}
};

} // namespace oglplus

#endif // GL_ARB_shading_language_include

#endif // include guard
