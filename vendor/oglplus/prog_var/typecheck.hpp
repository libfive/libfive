/**
 *  @file oglplus/prog_var/typecheck.hpp
 *  @brief Helper classes and functions used for program variable typechecking
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_TYPECHECK_1405052241_HPP
#define OGLPLUS_PROG_VAR_TYPECHECK_1405052241_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/config/basic.hpp>
#include <oglplus/config/compiler.hpp>
#include <oglplus/prog_var/type_ops.hpp>
#include <oglplus/detail/enum_class.hpp>

#include <cassert>

namespace oglplus {

template <typename T>
struct GLSLtoCppTypeMatcher;

#if !OGLPLUS_NO_UNIFORM_TYPECHECK

OGLPLUS_ENUM_CLASS_FWD(SLDataType, GLenum)

template <oglplus::SLDataType>
struct SLtoCpp;

template <typename T>
struct GLSLtoCppTypeMatcher
{
	static bool _matches(GLenum /*sl_type*/)
	{
		return false;
	}
};

template <oglplus::SLDataType SLType>
struct GLSLtoCppTypeMatcher<oglplus::SLtoCpp<SLType> >
{
	static bool _matches(GLenum sl_type)
	{
		return sl_type == GLenum(SLType);
	}
};

template <>
struct GLSLtoCppTypeMatcher<bool>
{
	static bool _matches(GLenum sl_type)
	{
		return sl_type == GL_BOOL;
	}
};

template <>
struct GLSLtoCppTypeMatcher<GLint>
{
	static bool _matches(GLenum sl_type);
};

template <>
struct GLSLtoCppTypeMatcher<GLuint>
{
	static bool _matches(GLenum sl_type);
};

template <>
struct GLSLtoCppTypeMatcher<GLfloat>
{
	static bool _matches(GLenum sl_type)
	{
		return sl_type == GL_FLOAT;
	}
};

#if defined(GL_DOUBLE)
template <>
struct GLSLtoCppTypeMatcher<GLdouble>
{
	static bool _matches(GLenum sl_type)
	{
		return sl_type == GL_DOUBLE;
	}
};
#endif

#if defined(GL_UNSIGNED_INT64_ARB)
template <>
struct GLSLtoCppTypeMatcher<GLuint64>
{
	static bool _matches(GLenum sl_type)
	{
		return sl_type == GL_UNSIGNED_INT64_ARB;
	}
};
#endif

struct GLSLtoCppTypeMatcher_Vec
{
	static std::size_t _type_idx(bool*) { return 0; }
	static std::size_t _type_idx(GLint*) { return 1; }
	static std::size_t _type_idx(GLuint*) { return 2; }
	static std::size_t _type_idx(GLfloat*) { return 3; }
#if defined(GL_DOUBLE)
	static std::size_t _type_idx(GLdouble*) { return 4; }
#endif

	static bool _does_match(
		GLenum sl_type,
		std::size_t type_idx,
		std::size_t dim
	);
};

template <typename T, std::size_t N>
struct GLSLtoCppTypeMatcher<oglplus::Vector<T, N> >
 : public GLSLtoCppTypeMatcher_Vec
{
	static_assert(N <= 4, "Invalid vector size");

	static T* _type_sel(void) { return nullptr; }

	static bool _matches(GLenum sl_type)
	{
		return GLSLtoCppTypeMatcher_Vec::_does_match(
			sl_type,
			GLSLtoCppTypeMatcher_Vec::_type_idx(_type_sel()),
			N
		);
	}
};

struct GLSLtoCppTypeMatcher_Mat
{
	static std::size_t _type_idx(GLfloat*) { return 0; }
#ifdef GL_DOUBLE
	static std::size_t _type_idx(GLdouble*) { return 1; }
#endif

	static bool _does_match(
		GLenum sl_type,
		std::size_t type_idx,
		std::size_t rows,
		std::size_t cols
	);
};

template <typename T, std::size_t Rows, std::size_t Cols>
struct GLSLtoCppTypeMatcher<oglplus::Matrix<T, Rows, Cols> >
 : public GLSLtoCppTypeMatcher_Mat
{
	static_assert(Rows >= 2, "Invalid matrix size");
	static_assert(Cols >= 2, "Invalid matrix size");

	static_assert(Rows <= 4, "Invalid matrix size");
	static_assert(Cols <= 4, "Invalid matrix size");

	static T* _type_sel(void) { return nullptr; }

	static bool _matches(GLenum sl_type)
	{
		return GLSLtoCppTypeMatcher_Mat::_does_match(
			sl_type,
			GLSLtoCppTypeMatcher_Mat::_type_idx(_type_sel()),
			Rows,
			Cols
		);
	}
};

template <>
class ProgVarTypecheck<tag::Typecheck, tag::Uniform>
 : public ProgVarTypeOps<tag::Uniform>
{
private:
	bool (*_type_matches)(GLenum);

	static void _do_check(
		bool (*)(GLenum),
		GLenum var_type,
		ProgramName program,
		GLint location,
		StrCRef identifier
	);
public:
	template <typename TypeSel>
	ProgVarTypecheck(TypeSel*)
	 : _type_matches(&GLSLtoCppTypeMatcher<TypeSel>::_matches)
	{ }

	void CheckType(
		ProgramName program,
		GLint location,
		StrCRef identifier
	) const
	{
		_do_check(
			_type_matches,
			ProgVarTypeOps<tag::Uniform>::GetType(
				program,
				location,
				identifier
			),
			program,
			location,
			identifier
		);
	}
};
#endif // !OGLPLUS_NO_UNIFORM_TYPECHECK

// Default/No typechecking
template <typename ChkTag, typename VarTag>
class ProgVarTypecheck
{
public:
	ProgVarTypecheck(void*)
	OGLPLUS_NOEXCEPT(true)
	{ }

	void CheckType(
		ProgramName program,
		GLint location,
		StrCRef identifier
	) const
	{
		(void)program;
		(void)location;
		(void)identifier;
	}
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/prog_var/typecheck.ipp>
#endif // OGLPLUS_LINK_LIB

#endif // include guard
