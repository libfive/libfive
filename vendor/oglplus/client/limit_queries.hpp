/**
 *  @file oglplus/client/limit_queries.hpp
 *  @brief Wrappers for GL limit queries
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_LIMIT_QUERIES_1412071213_HPP
#define OGLPLUS_CLIENT_LIMIT_QUERIES_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/limit_queries.hpp>

namespace oglplus {
namespace client {
namespace aux {

template <LimitQuery Query>
class Limits
{
public:
	typedef	typename oglplus::enums::EnumAssocGLType<
		LimitQuery,
		Query
	>::Type value_type;

	static
	value_type Get(void)
	{
		return context::LimitQueries::Limit<Query>();
	}

	static
	value_type Get(GLuint index)
	{
		return context::LimitQueries::Limit<Query>(index);
	}

	operator value_type(void) const
	{
		return Get();
	}
};

} // namespace aux
} // namespace client


namespace client {

class LimitQueries
 : public context::LimitQueries
{
public:
	oglplus::enums::EnumToClass<
		Nothing,
		LimitQuery,
		aux::Limits
	> Limits;
};

} // namespace client
} // namespace oglplus

#endif // include guard
