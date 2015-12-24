/**
 *  @file oglplus/query.hpp
 *  @brief Query object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_QUERY_1107121519_HPP
#define OGLPLUS_QUERY_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/object/reference.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/enums/query_target.hpp>
#include <oglplus/enums/conditional_render_mode.hpp>
#include <oglplus/boolean.hpp>
#include <cassert>

namespace oglplus {

/// Class wrapping query construction/destruction functions
/** @note Do not use this class directly, use Query instead.
 *
 *  @glsymbols
 *  @glfunref{GenQueries}
 *  @glfunref{DeleteQueries}
 *  @glfunref{IsQuery}
 */
template <>
class ObjGenDelOps<tag::Query>
{
protected:
	static void Gen(tag::Generate, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(GenQueries)(count, names);
		OGLPLUS_CHECK_SIMPLE(GenQueries);
	}
#if GL_VERSION_4_5 || GL_ARB_direct_state_access
	static void Gen(
		tag::Create,
		GLenum target,
		GLsizei count,
		GLuint* names
	)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(CreateQueries)(target, count, names);
		OGLPLUS_CHECK_SIMPLE(CreateQueries);
	}

	GLenum _type;

	void Gen(tag::Create create, GLsizei count, GLuint* names)
	{
		Gen(create, _type, count, names);
	}
#endif

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(DeleteQueries)(count, names);
		OGLPLUS_VERIFY_SIMPLE(DeleteQueries);
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsQuery)(name), 
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsQuery);
		return result;
	}
};

template <>
struct ObjectSubtype<tag::Query>
{
	typedef QueryTarget Type;
};

class QueryActivator;

template <typename ResultType>
class QueryExecution;

/// Class wrapping query functions with (direct state access)
/** @note Do not use this class directly, use Query instead.
 */
template <>
class ObjectOps<tag::DirectState, tag::Query>
 : public ObjZeroOps<tag::DirectState, tag::Query>
{
protected:
	ObjectOps(QueryName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::Query>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::Query> _base;

	ObjectOps(ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	{ }

	ObjectOps(const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	{ }

	ObjectOps& operator = (ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		return *this;
	}

	ObjectOps& operator = (const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		return *this;
	}
#endif
	/// Query execution target
	typedef QueryTarget Target;

	/// Begin the query on the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BeginQuery}
	 */
	void Begin(Target target)
	{
		OGLPLUS_GLFUNC(BeginQuery)(GLenum(target), _obj_name());
		OGLPLUS_VERIFY(
			BeginQuery,
			ObjectError,
			Object(*this).
			EnumParam(target)
		);
	}

	/// End the query on the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{EndQuery}
	 */
	void End(Target target)
	{
		OGLPLUS_GLFUNC(EndQuery)(GLenum(target));
		OGLPLUS_VERIFY(
			EndQuery,
			ObjectError,
			Object(*this).
			EnumParam(target)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Begin conditional render on the query in the specified mode
	/**
	 *  @glsymbols
	 *  @glfunref{BeginConditionalRender}
	 */
	void BeginConditionalRender(ConditionalRenderMode mode)
	{
		OGLPLUS_GLFUNC(BeginConditionalRender)(_obj_name(), GLenum(mode));
		OGLPLUS_VERIFY(
			BeginConditionalRender,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
	}

	/// Ends currently active conditional render
	/**
	 *  @glsymbols
	 *  @glfunref{EndConditionalRender}
	 */
	static void EndConditionalRender(void)
	{
		OGLPLUS_GLFUNC(EndConditionalRender)();
		OGLPLUS_VERIFY_SIMPLE(EndConditionalRender);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_timer_query
	/// Do a counter query on the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{QueryCounter}
	 */
	void Counter(Target target)
	{
		OGLPLUS_GLFUNC(QueryCounter)(_obj_name(), GLenum(target));
		OGLPLUS_VERIFY(
			QueryCounter,
			ObjectError,
			Object(*this).
			EnumParam(target)
		);
	}

	/// Do a timestamp query
	/**
	 *  @glsymbols
	 *  @glfunref{QueryCounter}
	 *  @gldefref{TIMESTAMP}
	 */
	void Timestamp(void)
	{
		Counter(Target::Timestamp);
	}
#endif

	/// Returns true if the query result is available
	/**
	 *  @glsymbols
	 *  @glfunref{GetQueryObject}
	 *  @gldefref{QUERY_RESULT_AVAILABLE}
	 */
	Boolean ResultAvailable(void) const
	{
		Boolean result;
		OGLPLUS_GLFUNC(GetQueryObjectiv)(
			_obj_name(),
			GL_QUERY_RESULT_AVAILABLE,
			result._ptr()
		);
		OGLPLUS_VERIFY(
			GetQueryObjectiv,
			ObjectError,
			Object(*this)
		);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Get the query result
	/**
	 *  @glsymbols
	 *  @glfunref{GetQueryObject}
	 *  @gldefref{QUERY_RESULT}
	 */
	void Result(GLint& result) const
	{
		OGLPLUS_GLFUNC(GetQueryObjectiv)(
			_obj_name(),
			GL_QUERY_RESULT,
			&result
		);
		OGLPLUS_VERIFY(
			GetQueryObjectiv,
			ObjectError,
			Object(*this)
		);
	}
#endif // GL_VERSION_3_0

	/// Get the query result
	/**
	 *  @glsymbols
	 *  @glfunref{GetQueryObject}
	 *  @gldefref{QUERY_RESULT}
	 */
	void Result(GLuint& result) const
	{
		OGLPLUS_GLFUNC(GetQueryObjectuiv)(
			_obj_name(),
			GL_QUERY_RESULT,
			&result
		);
		OGLPLUS_VERIFY(
			GetQueryObjectuiv,
			ObjectError,
			Object(*this)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_timer_query
	/// Get the query result
	/**
	 *  @glvoereq{3,3,ARB,timer_query}
	 *  @glsymbols
	 *  @glfunref{GetQueryObject}
	 *  @gldefref{QUERY_RESULT}
	 */
	void Result(GLint64& result) const
	{
		OGLPLUS_GLFUNC(GetQueryObjecti64v)(
			_obj_name(),
			GL_QUERY_RESULT,
			&result
		);
		OGLPLUS_VERIFY(
			GetQueryObjecti64v,
			ObjectError,
			Object(*this)
		);
	}

	/// Get the query result
	/**
	 *  @glvoereq{3,3,ARB,timer_query}
	 *  @glsymbols
	 *  @glfunref{GetQueryObject}
	 *  @gldefref{QUERY_RESULT}
	 */
	void Result(GLuint64& result) const
	{
		OGLPLUS_GLFUNC(GetQueryObjectui64v)(
			_obj_name(),
			GL_QUERY_RESULT,
			&result
		);
		OGLPLUS_VERIFY(
			GetQueryObjectui64v,
			ObjectError,
			Object(*this)
		);
	}
#endif // timer query

	/// Waits for and queries the result
	template <typename ResultType>
	void WaitForResult(ResultType& result) const
	{
		while(!ResultAvailable())
			OGLPLUS_GLFUNC(Finish)();
		Result(result);
	}

	/// The activator class
	typedef QueryActivator Activator;

	Activator Activate(Target target);

	/// Executes this query on the specified @p target and gets the @p result
	/** This function creates an instance of the QueryExecution class which
	 *  begins a query on the specified @p target when it is constructed
	 *  and ends this query and gets its @p result when it is destroyed.
	 */
	template <typename ResultType>
	QueryExecution<ResultType>
	Execute(Target target, ResultType& result);
};

/// Query operations with direct state access
typedef ObjectOps<tag::DirectState, tag::Query>
	QueryOps;

/// RAII Query activator/deactivator
/**
 *  @see Query
 */
class QueryActivator
{
protected:
	QueryName _query;
	QueryTarget _target;
private:
	bool _alive;
	QueryActivator(const QueryActivator&);
public:
	/// Begins a @p query on the specified @p target
	QueryActivator(
		QueryName query,
		QueryTarget target
	): _query(query)
	 , _target(target)
	 , _alive(false)
	{
		Reference<QueryOps>(_query).Begin(_target);
		_alive = true;
	}

	/// Activators are moveable
	QueryActivator(QueryActivator&& temp)
	 : _query(std::move(temp._query))
	 , _target(temp._target)
	 , _alive(temp._alive)
	{
		temp._alive = false;
	}

	/// Ends the query
	~QueryActivator(void)
	{
		try { Finish(); }
		catch(...) { }
	}

	/// Explicitly ends the query
	bool Finish(void)
	{
		if(_alive)
		{
			Reference<QueryOps>(_query).End(_target);
			_alive = false;
			return true;
		}
		else return false;
	}
};

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
/// RAII conditional render activator/deactivator
/**
 *  @see Query
 */
class ConditionalRender
{
private:
	bool _alive;
	ConditionalRender(const ConditionalRender&);
public:
	/// Begins conditional render on  @p query in the specified @p mode
	ConditionalRender(
		QueryName query,
		ConditionalRenderMode mode
	): _alive(false)
	{
		Reference<QueryOps>(query).BeginConditionalRender(mode);
		_alive = true;
	}

	/// ConditionalRenders are moveable
	ConditionalRender(ConditionalRender&& temp)
	 : _alive(temp._alive)
	{
		temp._alive = false;
	}

	/// Ends the conditional render
	~ConditionalRender(void)
	{
		try { Finish(); }
		catch(...) { }
	}

	/// Explicitly ends the conditional render
	bool Finish(void)
	{
		if(_alive)
		{
			QueryOps::EndConditionalRender();
			_alive = false;
			return true;
		}
		else return false;
	}
};
#endif

/// A helper class automatically executing a query
/** Instances of this class begin the query in the constructor
 *  and end the query in the destructor. It is more convenient
 *  not to use this class directly, use the Execute() function
 *  instead.
 *
 *  @see Query::Execute
 */
template <typename ResultType>
class QueryExecution : public QueryActivator
{
private:
	ResultType& _result;
public:
	QueryExecution(
		QueryName query,
		QueryTarget target,
		ResultType& result
	): QueryActivator(query, target)
	 , _result(result)
	{ }

	QueryExecution(QueryExecution&& temp)
	 : QueryActivator(static_cast<QueryActivator&&>(temp))
	 , _result(temp._result)
	{ }

	~QueryExecution(void)
	{
		try { WaitForResult(); }
		catch(...) { }
	}

	void WaitForResult(void)
	{
		if(this->Finish())
		{
			Reference<QueryOps>(this->_query).WaitForResult(_result);
		}
	}
};

inline
QueryActivator
ObjectOps<tag::DirectState, tag::Query>::
Activate(Target target)
{
	return QueryActivator(*this, target);
}

template <typename ResultType>
inline
QueryExecution<ResultType>
ObjectOps<tag::DirectState, tag::Query>::
Execute(QueryTarget target, ResultType& result)
{
	return QueryExecution<ResultType>(*this, target, result);
}

/// An @ref oglplus_object encapsulating the asynchronous query functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<QueryOps> Query;

} // namespace oglplus

#endif // include guard
