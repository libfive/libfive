/**
 *  @file oglplus/prog_var/optional.hpp
 *  @brief Optional program variable wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_OPTIONAL_1405052234_HPP
#define OGLPLUS_PROG_VAR_OPTIONAL_1405052234_HPP

#include <oglplus/detail/optional.hpp>
#include <oglplus/prog_var/wrapper.hpp>

namespace oglplus {

template <typename ProgVar>
class OptionalImpl<tag::ProgVar, ProgVar>
 : public ProgVar
{
public:
	OptionalImpl(ProgramName program, StrCRef identifier)
	 : ProgVar(program, identifier, false)
	{ }
};

} // namespace oglplus

#endif // include guard
