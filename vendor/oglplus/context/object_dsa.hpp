/**
 *  @file oglplus/context/object_dsa.hpp
 *  @brief Functions giving direct state acess to OGLplus Objects
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_OBJECT_DSA_1404232057_HPP
#define OGLPLUS_CONTEXT_OBJECT_DSA_1404232057_HPP

#include <oglplus/fwd.hpp>

namespace oglplus {
namespace context {

/// Object direct state access operations
/**
 *  @ingroup ogl_context
 */
class ObjectDSA
{
public:
	/// Returns a direct state access wrapper for @p object
	template <typename OpsTag, typename ObjTag>
	static Reference<ObjectOps<tag::DirectState, ObjTag>>
	Direct(const ObjectOps<OpsTag, ObjTag>& object)
	{
		return object;
	}

	template <typename OpsTag, typename ObjTag>
	static Reference<ObjectOps<tag::DirectState, ObjTag>>
	Direct(
		typename ObjectOps<OpsTag, ObjTag>::Target target,
		const ObjectOps<OpsTag, ObjTag>& object
	)
	{
		Reference<ObjectOps<tag::DirectState, ObjTag>> result(object);
		result.target = target;
		return result;
	}

	template <typename OpsTag, typename ObjTag>
	static Reference<ObjectOps<tag::DirectStateEXT, ObjTag>>
	DirectEXT(const ObjectOps<OpsTag, ObjTag>& object)
	{
		return object;
	}

	template <typename OpsTag, typename ObjTag>
	static Reference<ObjectOps<tag::DirectStateEXT, ObjTag>>
	DirectEXT(
		typename ObjectOps<OpsTag, ObjTag>::Target target,
		const ObjectOps<OpsTag, ObjTag>& object
	)
	{
		Reference<ObjectOps<tag::DirectStateEXT, ObjTag>> result(object);
		result.target = target;
		return result;
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard
