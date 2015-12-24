/**
 *  @file oglplus/context/limit_queries.hpp
 *  @brief Wrappers for implementation-dependent-limit queries
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_LIMIT_QUERIES_1201040722_HPP
#define OGLPLUS_CONTEXT_LIMIT_QUERIES_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/limit_query.hpp>

namespace oglplus {
namespace context {

/// Wrapper for implementation-dependent limit queries
/**
 *  @ingroup ogl_context
 */
class LimitQueries
{
public:
	static GLint Limit(LimitQuery query, TypeTag<int>)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GLenum(query), &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	static GLint Limit(LimitQuery query, GLuint index, TypeTag<int>)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegeri_v)(GLenum(query), index, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegeri_v);
		return result;
	}

	static GLfloat Limit(LimitQuery query, TypeTag<float>)
	{
		GLfloat result = 0;
		OGLPLUS_GLFUNC(GetFloatv)(GLenum(query), &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_viewport_array
	static GLfloat Limit(LimitQuery query, GLuint index, TypeTag<float>)
	{
		GLfloat result = 0;
		OGLPLUS_GLFUNC(GetFloati_v)(GLenum(query), index, &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloati_v);
		return result;
	}
#endif

	template <LimitQuery Query>
	static typename enums::EnumAssocGLType<
		LimitQuery,
		Query
	>::Type Limit(void)
	{
		return Limit(
			Query,
			TypeTag<
				typename enums::EnumAssocType<
					LimitQuery,
					Query
				>::Type
			>()
		);
	}

	template <LimitQuery Query>
	static typename enums::EnumAssocGLType<
		LimitQuery,
		Query
	>::Type Limit(GLuint index)
	{
		return Limit(
			Query,
			index,
			TypeTag<
				typename enums::EnumAssocType<
					LimitQuery,
					Query
				>::Type
			>()
		);
	}

	/// Gets the implementation-dependent limit value
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static GLint IntLimit(LimitQuery query)
	{
		return Limit(query, TypeTag<int>());
	}

	/// Gets the implementation-dependent indexed limit value
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static GLint IntLimit(LimitQuery query, GLuint index)
	{
		return Limit(query, index, TypeTag<int>());
	}

	/// Gets the implementation-dependent limit value
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static GLfloat FloatLimit(LimitQuery query)
	{
		return Limit(query, TypeTag<float>());
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_viewport_array
	/// Gets the implementation-dependent indexed limit value
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static GLfloat FloatLimit(LimitQuery query, GLuint index)
	{
		return Limit(query, index, TypeTag<float>());
	}
#endif

	static void ThrowIfOverLimit(
		LimitQuery limit,
		GLint value,
		GLint max_limit
	)
	{
		OGLPLUS_HANDLE_ERROR_IF(
			value > max_limit,
			GL_INVALID_VALUE,
			LimitError::Message(),
			LimitError,
			GLFunc(EnumValueName(limit).c_str())
		);
	}

	/// Raises a LimitError if @p value is greater than the specified @p limit
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *
	 *  @throws LimitError
	 */
	static void RequireAtLeast(LimitQuery limit, GLint value)
	{
		ThrowIfOverLimit(limit, value, IntLimit(limit));
	}

	/// Raises a LimitError if @p value is greater than the specified @p limit
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *
	 *  @throws LimitError
	 */
	static void RequireAtLeast(LimitQuery limit, GLuint index, GLint value)
	{
		ThrowIfOverLimit(limit, value, IntLimit(limit, index));
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard
