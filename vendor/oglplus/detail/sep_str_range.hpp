/**
 *  .file oglplus/detail/sep_str_range.hpp
 *  .brief Range for traversal of separated string values
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2012-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_SEP_STR_RANGE_1305291314_HPP
#define OGLPLUS_AUX_SEP_STR_RANGE_1305291314_HPP

#include <algorithm>

namespace oglplus {
namespace aux {

template <typename String>
class SepStrRangeTpl
{
private:
	String _values;
	typename String::iterator _i, _e;
public:
	typedef String ValueType;

	SepStrRangeTpl(const char* str, char sep = ' ')
	 : _values(str ? str : "")
	 , _i(_values.begin())
	 , _e(_values.end())
	{
		std::replace(_i, _e, sep, '\0');
	}

	bool Empty(void) const
	{
		return _i == _e;
	}

	String Front(void) const
	{
		assert(!Empty());
		typename String::iterator p = std::find(_i, _e, '\0');
		return String(_i, p);
	}

	void Next(void)
	{
		assert(!Empty());
		_i = std::find(_i, _e, '\0');
		if(_i != _e) ++_i;
		if((_i != _e) && (*_i == '\0'))
			_i = _e;
	}
};

} // namespace aux
} // namespace oglplus

#endif // include guard
