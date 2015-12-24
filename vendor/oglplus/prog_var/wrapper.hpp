/**
 *  @file oglplus/prog_var/wrapper.hpp
 *  @brief Program variable wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_WRAPPER_1405052234_HPP
#define OGLPLUS_PROG_VAR_WRAPPER_1405052234_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/typecheck.hpp>
#include <cassert>
#include <new>

namespace oglplus {

template <typename VarTag>
class ProgVarCommonOps
 : public ProgVarLoc<VarTag>
{
protected:
	ProgVarCommonOps(ProgVarLoc<VarTag> pvloc)
	 : ProgVarLoc<VarTag>(pvloc)
	{ }
public:
};

/// Program variable (vertex attrib / uniform ) wrapper
template <typename OpsTag, typename VarTag, typename ChkTag, typename T>
class ProgVar
 : public ProgVarGetSetOps<OpsTag, VarTag, typename AdjustProgVar<T>::BaseType>
 , public ProgVarTypecheck<ChkTag, VarTag>
{
private:
	typedef typename AdjustProgVar<T>::BaseType BaseType;
	typedef ProgVarGetSetOps<OpsTag, VarTag, BaseType> BaseGetSetOps;
	typedef ProgVarTypecheck<ChkTag, VarTag> Typecheck;

	static inline
	BaseType* _no_tc(void)
	{
		return nullptr;
	}
public:
	/// Default construction
	ProgVar(void)
	 : BaseGetSetOps(ProgVarLoc<VarTag>())
	 , Typecheck(_no_tc())
	{ }

	/// Variable from a ProgVarLoc
	ProgVar(ProgVarLoc<VarTag> pvloc)
	 : BaseGetSetOps(pvloc)
	 , Typecheck(_no_tc())
	{ }

	/// Variable with the specified @p location in the specified @p program
	ProgVar(ProgramName program, GLuint location)
	 : BaseGetSetOps(ProgVarLoc<VarTag>(program, GLint(location)))
	 , Typecheck(_no_tc())
	{ }

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier)
	 : BaseGetSetOps(ProgVarLoc<VarTag>(program, identifier))
	 , Typecheck(_no_tc())
	{
		Typecheck::CheckType(program, this->_location, identifier);
	}

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier, bool active_only)
	 : BaseGetSetOps(ProgVarLoc<VarTag>(program, identifier, active_only))
	 , Typecheck(_no_tc())
	{
		Typecheck::CheckType(program, this->_location, identifier);
	}

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier, std::nothrow_t)
	 : BaseGetSetOps(ProgVarLoc<VarTag>(program, identifier, false))
	 , Typecheck(_no_tc())
	{
		Typecheck::CheckType(program, this->_location, identifier);
	}

	ProgVar& BindTo(StrCRef identifier)
	{
		BaseGetSetOps::BindTo(identifier);
		Typecheck::CheckType(
			ProgramName(this->_program),
			this->_location,
			identifier
		);
		return *this;
	}

	ProgVar operator[](std::size_t offset) const
	{
		return ProgVar(
			ProgramName(this->_program),
			this->_location+GLuint(offset)
		);
	}

	ProgVar& operator = (ProgVarLoc<VarTag> pvloc)
	{
		BaseGetSetOps::Assign(pvloc);
		return *this;
	}

	/// Parameter value type
	typedef typename AdjustProgVar<T>::ValueType ParamType;

	/// Set the variable value
	void Set(const ParamType &value)
	{
		BaseGetSetOps::RequireActive();
		BaseGetSetOps::SetValue(AdjustProgVar<T>::Adjust(value));
	}

	template <typename S>
	void Set(SizeImpl<S> value)
	{
		Set(ParamType(value));
	}

	/// Set multiple values
	void Set(std::size_t count, const T* values)
	{
		BaseGetSetOps::RequireActive();
		BaseGetSetOps::SetValues(count, values);
	}

	void Set(const std::vector<T>& values)
	{
		Set(values.size(), values.data());
	}

	template <typename T0, typename T1>
	void Set(T0 v0, T1 v1)
	{
		Set(ParamType(v0, v1));
	}

	template <typename T0, typename T1, typename T2>
	void Set(T0 v0, T1 v1, T2 v2)
	{
		Set(ParamType(v0, v1, v2));
	}

	template <typename T0, typename T1, typename T2, typename T3>
	void Set(T0 v0, T1 v1, T2 v2, T3 v3)
	{
		Set(ParamType(v0, v1, v2, v3));
	}

	/// Sets the variable value if it is active
	void TrySet(const ParamType &value)
	{
		if(this->IsActive())
		{
			BaseGetSetOps::SetValue(AdjustProgVar<T>::Adjust(value));
		}
	}

	/// Sets the variable value
	ProgVar& operator = (const ParamType &value)
	{
		this->Set(value);
		return *this;
	}
};

template <typename OpsTag, typename VarTag>
class ProgVar<OpsTag, VarTag, tag::NoTypecheck, void>
 : public ProgVarCommonOps<VarTag>
{
private:
	typedef ProgVarCommonOps<VarTag> BaseOps;
public:
	/// Default construction
	ProgVar(void)
	 : BaseOps(ProgVarLoc<VarTag>())
	{ }

	/// Variable from a ProgVarLoc
	ProgVar(ProgVarLoc<VarTag> pvloc)
	 : BaseOps(pvloc)
	{ }

	/// Variable with the specified @p location in the specified @p program
	ProgVar(ProgramName program, GLuint location)
	 : BaseOps(ProgVarLoc<VarTag>(program, GLint(location)))
	{ }

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier)
	 : BaseOps(ProgVarLoc<VarTag>(program, identifier))
	{ }

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier, bool active_only)
	 : BaseOps(ProgVarLoc<VarTag>(program, identifier, active_only))
	{ }

	/// Variable with the specified @p identifier in the specified @p program
	ProgVar(ProgramName program, StrCRef identifier, std::nothrow_t)
	 : BaseOps(ProgVarLoc<VarTag>(program, identifier, false))
	{ }
};

#if !OGLPLUS_NO_INHERITED_CONSTRUCTORS
#define OGLPLUS_IMPLEMENT_PROG_VAR_CTRS(VAR_TAG, PROG_VAR, BASE) \
	using BASE::BASE;

#else
#define OGLPLUS_IMPLEMENT_PROG_VAR_CTRS(VAR_TAG, PROG_VAR, BASE) \
	PROG_VAR(void) : BASE() { } \
	PROG_VAR(ProgVarLoc<VAR_TAG> pvloc) : BASE(pvloc) { } \
	PROG_VAR(ProgramName program, GLuint location) \
	 : BASE(program, location) { } \
	PROG_VAR(ProgramName program, StrCRef identifier) \
	 : BASE(program, identifier) { } \
	PROG_VAR(ProgramName program, StrCRef identifier, bool active_only) \
	 : BASE(program, identifier, active_only) { } \
	PROG_VAR(ProgramName program, StrCRef identifier, std::nothrow_t nt) \
	 : BASE(program, identifier, nt) { }
#endif

#if !OGLPLUS_NO_TEMPLATE_ALIASES

// OGLPLUS_DECLARE_PROG_VAR
#define OGLPLUS_DECLARE_PROG_VAR(PROG_VAR, OPS_TAG, VAR_TAG, CHK_TAG) \
template <typename T> \
using PROG_VAR = ProgVar<OPS_TAG, VAR_TAG, CHK_TAG, T>;

#else

// OGLPLUS_DECLARE_PROG_VAR
#define OGLPLUS_DECLARE_PROG_VAR(PROG_VAR, OPS_TAG, VAR_TAG, CHK_TAG) \
template <typename T> \
class PROG_VAR \
 : public ProgVar<OPS_TAG, VAR_TAG, CHK_TAG, T> \
{ \
private:\
	typedef ProgVar<OPS_TAG, VAR_TAG, CHK_TAG, T> Base;\
public:\
	OGLPLUS_IMPLEMENT_PROG_VAR_CTRS(VAR_TAG, PROG_VAR, Base) \
	PROG_VAR& operator = (ProgVarLoc<VAR_TAG> pvloc) \
	{ \
		Base::Assign(pvloc); \
		return *this; \
	} \
	PROG_VAR& operator = (typename Base::ParamType value) \
	{ \
		this->Set(value); \
		return *this; \
	} \
}; \
template <typename T> \
struct Classify<PROG_VAR<T>> \
 : Classify<ProgVar<OPS_TAG, VAR_TAG, CHK_TAG, T>> \
{ };
#endif

} // namespace oglplus

#endif // include guard
