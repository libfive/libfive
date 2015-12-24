/**
 *  @file oglplus/string/ref_tpl.hpp
 *  @brief String reference
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_STRING_REF_TPL_1107121519_HPP
#define OGLPLUS_STRING_REF_TPL_1107121519_HPP

#include <oglplus/string/utf8.hpp>
#include <oglplus/config/compiler.hpp>
#include <array>
#include <vector>
#include <string>
#include <cstring>
#include <cstddef>
#include <cassert>

namespace oglplus {

// helper class used for implementation of concatenation of string references
template <typename Char, typename Left, typename Right>
class StrCRefChainTpl
{
private:
	Left _left;
	Right _right;
public:
	StrCRefChainTpl(Left left, Right right)
	 : _left(left)
	 , _right(right)
	{ }

	bool empty(void) const
	{
		return _left.empty() && _right.empty();
	}

	std::size_t size(void) const
	{
		return _left.size() + _right.size();
	}

	void append_to(std::basic_string<Char>& dest) const
	{
		_left.append_to(dest);
		_right.append_to(dest);
	}

	std::basic_string<Char> str(void) const
	{
		std::basic_string<Char> result;
		result.reserve(this->size()+1);
		this->append_to(result);
		return result;
	}

	OGLPLUS_EXPLICIT operator std::basic_string<Char>(void) const
	{
		return str();
	}
};

/// String const reference wrapper template
template <typename Char>
class StrCRefTpl
{
private:
	static_assert(
		sizeof(Char) == sizeof(char),
		"Characters with different size that sizeof(char) "
		"are not supported by StrCRefTpl"
	);

	const Char* _c_str;
	mutable std::size_t _size;

	static const Char* _ecs(void)
	OGLPLUS_NOEXCEPT(true)
	{ return ""; }

	void _validate(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		assert(aux::ValidUTF8(
			static_cast<const char*>(begin()),
			static_cast<const char*>(end())
		));
	}
public:
	/// Default construction (reference to an empty c-string)
	StrCRefTpl(void)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(nullptr)
	 , _size(0)
	{ }

	/// Construction from a null-terminated string
	StrCRefTpl(const Char* cstr)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(cstr)
	 , _size(0)
	{ _validate(); }

	/// Construction from a c-string and size
	StrCRefTpl(const Char* cstr, std::size_t ssize)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(cstr)
	 , _size(ssize)
	{ _validate(); }

	/// Construction from a character array with known size
	template <std::size_t N>
	StrCRefTpl(const Char (&cary)[N])
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(cary)
	 , _size(N)
	{ _validate(); }

	/// Construction from a std::basic_string<Char>
	StrCRefTpl(const std::basic_string<Char>& sstr)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(sstr.c_str())
	 , _size(sstr.size())
	{ _validate(); }

	/// Construction from a std::vector<Char>
	StrCRefTpl(const std::vector<Char>& cvec)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(cvec.data())
	 , _size(cvec.size())
	{ _validate(); }

	/// Construction from a std::array<Char, N>
	template <std::size_t N>
	StrCRefTpl(const std::array<Char, N>& cvec)
	OGLPLUS_NOEXCEPT(true)
	 : _c_str(cvec.data())
	 , _size(cvec.size())
	{ _validate(); }

	/// Return the size (length) string
	std::size_t size(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		if((_size == 0) && (_c_str != nullptr))
			_size = std::strlen(_c_str);
		return _size;
	}

	/// Returns true if the string is empty
	bool empty(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return size() == 0;
	}

	/// Iterator type
	typedef const Char* iterator;
	/// Const iterator type
	typedef iterator const_iterator;

	/// Returns iterator to the first character
	const_iterator begin(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _c_str?_c_str:_ecs();
	}

	/// Returns iterator past the last character
	const_iterator end(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return begin()+size();
	}

	/// Returns true if the string is null-terminated
	bool is_nts(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return *end() == '\0';
	}

	/// Returns a String
	std::basic_string<Char> str(void) const
	{
		return std::basic_string<Char>(begin(), end());
	}

	/// Returns the null-terminated c-string
	/**
	 *  @pre is_nts()
	 */
	const Char* c_str(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		assert(is_nts());
		return begin();
	}

	static bool Equal(const StrCRefTpl& a, const StrCRefTpl& b)
	{
		return	(a.size() == b.size()) &&
			(std::strncmp(a.begin(), b.begin(), a.size()) == 0);
	}

	static bool Equal(const StrCRefTpl& a, const Char* b)
	{
		if(a.is_nts()) return std::strcmp(a.c_str(), b) == 0;

		std::size_t size = std::strlen(b);
		return	(a.size() == size) &&
			(std::strncmp(a.begin(), b, a.size()) == 0);
	}

	friend bool operator == (const StrCRefTpl& a, const StrCRefTpl& b)
	{
		return StrCRefTpl::Equal(a, b);
	}

	friend bool operator == (const StrCRefTpl& a, const Char* b)
	{
		return StrCRefTpl::Equal(a, b);
	}

	friend bool operator == (const Char* a, const StrCRefTpl& b)
	{
		return StrCRefTpl::Equal(b, a);
	}

	friend bool operator != (const StrCRefTpl& a, const StrCRefTpl& b)
	{
		return !StrCRefTpl::Equal(a, b);
	}

	friend bool operator != (const StrCRefTpl& a, const Char* b)
	{
		return !StrCRefTpl::Equal(a, b);
	}

	friend bool operator != (const Char* a, const StrCRefTpl& b)
	{
		return !StrCRefTpl::Equal(b, a);
	}
};

template <typename Char>
inline
StrCRefChainTpl<Char, StrCRefTpl<Char>, StrCRefTpl<Char>>
operator + (StrCRefTpl<Char> left, StrCRefTpl<Char> right)
{
	return StrCRefChainTpl<Char, StrCRefTpl<Char>, StrCRefTpl<Char>>(
		left,
		right
	);
}

template <typename Char, typename Left, typename Right>
inline
StrCRefChainTpl<Char, StrCRefChainTpl<Char, Left, Right>, StrCRefTpl<Char> >
operator + (StrCRefChainTpl<Char, Left, Right> left, StrCRefTpl<Char> right)
{
	return StrCRefChainTpl<
		Char,
		StrCRefChainTpl<Char,Left,Right>,
		StrCRefTpl<Char>
	>(left, right);
}

} // namespace oglplus

#endif // include guard
