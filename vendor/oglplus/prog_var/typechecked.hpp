/**
 *  @file oglplus/prog_var/typechecked.hpp
 *  @brief Typechecked program variable wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_TYPECHECKED_1405052234_HPP
#define OGLPLUS_PROG_VAR_TYPECHECKED_1405052234_HPP

#include <oglplus/detail/typechecked.hpp>
#include <oglplus/prog_var/wrapper.hpp>

namespace oglplus {

template <typename ProgVar_>
class TypecheckedImpl<tag::ProgVar, ProgVar_>
 : public ProgVar_
{
private:
	typedef typename Classify<ProgVar_>::VarTag VarTag;
public:
	OGLPLUS_IMPLEMENT_PROG_VAR_CTRS(VarTag, TypecheckedImpl, ProgVar_)
};

template <typename ProgVar>
struct Classify<Typechecked<ProgVar>>
 : Classify<ProgVar>
{ };

} // namespace oglplus

#endif // include guard
