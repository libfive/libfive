/**
 *  @file oglplus/string/ref_ios.hpp
 *  @brief String reference iostream operators
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_STRING_REF_IOS_1107121519_HPP
#define OGLPLUS_STRING_REF_IOS_1107121519_HPP

#include <oglplus/string/ref_tpl.hpp>
#include <iostream>

namespace oglplus {

template <typename Char>
inline
std::ostream&
operator << (std::ostream& out, const StrCRefTpl<Char>& str)
{
	out.write(str.begin(), str.size());
	return out;
}

} // namespace oglplus

#endif // include guard
