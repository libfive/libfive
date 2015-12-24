/**
 *  @file oglplus/vertex_attrib.hpp
 *  @brief VertexAttrib wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_VERTEX_ATTRIB_1107121519_HPP
#define OGLPLUS_VERTEX_ATTRIB_1107121519_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/vertex_attrib_slot.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/object/name.hpp>
#include <oglplus/object/sequence.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/varpara_fns.hpp>
#include <oglplus/prog_var/set_ops.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <oglplus/utils/type_tag.hpp>

#include <type_traits>

namespace oglplus {

template <>
class ProgVarLocOps<tag::VertexAttrib>
{
private:
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
public:
	/// Bind the vertex attribute location
	/**
	 *  @see GetLocation
	 *  @see QueryLocation
	 *
	 *  @glsymbols
	 *  @glfunref{BindAttribLocation}
	 */
	static void BindLocation(
		ProgramName program,
		VertexAttribSlot location,
		StrCRef identifier
	)
	{
		OGLPLUS_GLFUNC(BindAttribLocation)(
			GetGLName(program),
			GLuint(location),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			BindAttribLocation,
			ProgVarError,
			Program(program).
			Identifier(identifier).
			Index(GLuint(location))
		);
	}

	/// Finds the vertex attribute location, throws on failure if active_only
	/** Finds the location of the input vertex attribute specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such attribute exists or if it is not active.
	 *  For a non-throwing version see QueryActiveLocation().
	 *
	 *  @see GetCommonLocation
	 *  @see QueryActiveLocation
	 *  @see BindLocation
	 *
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	static GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	)
	{
		GLint result = OGLPLUS_GLFUNC(GetAttribLocation)(
			GetGLName(program),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetAttribLocation,
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);
		OGLPLUS_HANDLE_ERROR_IF(
			active_only && (result < 0),
			GL_INVALID_OPERATION,
			MsgGettingInactive(),
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);
		return result;
	}

	static VertexAttribSlot GetLocation(
		ProgramName program,
		StrCRef identifier
	)
	{
		return VertexAttribSlot(GetLocation(program, identifier, true));
	}

	/// Queries the vertex attribute location, returns false on failure
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	static bool QueryActiveLocation(
		ProgramName program,
		StrCRef identifier,
		VertexAttribSlot& location
	)
	{
		GLint result = GetLocation(program, identifier, false);
		if(result < 0) return false;
		location = VertexAttribSlot(result);
		return true;
	}

	/// Allows to query the vertex attribute @p location in multiple @p programs
	/** This function returns a temporary object that allows to query
	 *  the @p location of the specified @p identifier in several programs.
	 *  The returned object has two functions called @c In and @c And
	 *  which are equivalent and take a Program as the argument. Both
	 *  these functions return in turn a new instance of the temporary
	 *  which allows to check in another program, and so on.
	 *  The temporary is also convertible to @c bool indicating whether
	 *  a common location was found in all programs in the chain.
	 *
	 *  @code
	 *  VertexArray vao;
	 *  Buffer buf;
	 *  Program prog1, prog2, prog3, prog4;
	 *  // build the programs, load data into the buffer, ...
	 *  vao.Bind();
	 *  buffer.Bind(Buffer::Target::Array);
	 *  VertexAttribSlot location;
	 *  if(VertexAttrib::QueryCommonLocation(
	 *      MakeGroup(prog1, prog2, prog3),
	 *      "Position",
	 *      location
	 *  ))
	 *  {
	 *      VertexArrayAttrib attr(location);
	 *      attr.Setup(n_per_vertex, DataType::Float);
	 *      attr.Enable();
	 *  }
	 *  else
	 *  {
	 *      // handle the error or bind the locations manually
	 *  }
	 *  @endcode
	 *
	 *  @note Never store the resulting object in a named variable
	 *  nor use it after the call to this overload of QueryCommonLocation
	 *  has finished. Doing this causes undefined behavior.
	 *
	 *  @see GetLocation
	 *  @see GetCommonLocation
	 *  @see QueryLocation
	 *  @see BindLocation
	 *
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	static bool QueryCommonLocation(
		const Sequence<ProgramName>& programs,
		StrCRef identifier,
		VertexAttribSlot& location
	);

	/// Returns vertex attr. location in multiple programs if it's consistent
	/** Finds the location of the input vertex attribute specified
	 *  by @p identifier in every program in @p programs.
	 *  Throws Error if no such attribute exists or if it is not active
	 *  in some of the programs or if the attribute has different locations
	 *  in different programs. Otherwise returns the vertex attribute
	 *  position.
	 *
	 *  @code
	 *  VertexArray vao;
	 *  Buffer buf;
	 *  Program prog1, prog2, prog3;
	 *  // build the programs, load data into the buffer, ...
	 *  vao.Bind();
	 *  buffer.Bind(Buffer::Target::Array);
	 *  try
	 *  {
	 *      VertexArrayAttrib attr(
	 *          VertexAttribOps::GetCommonLocation(
	 *              MakeGroup(prog1, prog2, prog3),
	 *              "Position"
	 *          )
	 *      );
	 *      attr.Setup(n_per_vertex, DataType::Float);
	 *      attr.Enable();
	 *  }
	 *  catch(ProgVarError& error)
	 *  {
	 *      // handle the error or bind the locations manually
	 *  }
	 *
	 *  @endcode
	 *
	 *  @see GetLocation
	 *  @see GetCommonLocation
	 *  @see QueryLocation
	 *  @see BindLocation
	 *
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	static VertexAttribSlot GetCommonLocation(
		const Sequence<ProgramName>& programs,
		StrCRef identifier
	);
};

template <>
class ProgVarSetters<tag::ImplicitSel, tag::VertexAttrib, tag::NativeTypes>
{
protected:
	OGLPLUS_ERROR_CONTEXT(VertexAttrib, VertexAttrib)

	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, f, t, GLfloat)
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, d, t, GLdouble)
	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, s, t, GLshort)

	OGLPLUS_AUX_VARPARA_FNS(VertexAttribI, i, t, GLint)
	OGLPLUS_AUX_VARPARA_FNS(VertexAttribI, ui, t, GLuint)
#endif

	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, fv, v, GLfloat)
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, dv, v, GLdouble)
	OGLPLUS_AUX_VARPARA_FNS(VertexAttrib, sv, v, GLshort)

	OGLPLUS_AUX_VARPARA_FNS(VertexAttribI, i, v, GLint)
	OGLPLUS_AUX_VARPARA_FNS(VertexAttribI, ui, v, GLuint)
#endif
};

template <>
class ProgVarCommonOps<tag::VertexAttrib>
 : public ProgVarLoc<tag::VertexAttrib>
{
protected:
	ProgVarCommonOps(VertexAttribLoc valoc)
	 : ProgVarLoc<tag::VertexAttrib>(valoc)
	{ }

	// Functions for autodetection of values-per-vertex
	template <typename T>
	static GLint _get_vpv(TypeTag<T>) { return 1; }

	template <typename T, std::size_t N>
	static GLint _get_vpv(TypeTag<Vector<T, N>>) { return N; }

	template <typename T, std::size_t Rows, std::size_t Cols>
	static GLint _get_vpv(TypeTag<Matrix<T, Rows, Cols>>) { return Rows*Cols; }

	// Functions for autodetection of element type
	template <typename T>
	static T _get_et(TypeTag<T>);

	template <typename T, std::size_t N>
	static T _get_et(TypeTag<Vector<T, N>>);

	template <typename T, std::size_t Rows, std::size_t Cols>
	static T _get_et(TypeTag<Matrix<T, Rows, Cols>>);

	GLuint _attrib_index(void) const
	{
		if(this->_location < 0)
		{
			return GL_INVALID_INDEX;
		}
		return GLuint(this->_location);
	}
public:
	void Bind(StrCRef identifier)
	{
		BindLocation(
			this->Program(),
			this->_location,
			identifier
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3
	/// Set the vertex attrib divisor
	/**
	 *  @glverreq{3,3}
	 *  @glsymbols
	 *  @glfunref{VertexAttribDivisor}
	 */
	void Divisor(GLuint divisor) const
	{
		OGLPLUS_GLFUNC(VertexAttribDivisor)(
			_attrib_index(),
			divisor
		);
		OGLPLUS_CHECK_SIMPLE(VertexAttribDivisor);
	}
#endif
};

/// Encapsulates vertex attribute value set functionality
/**
 *  @ingroup shader_variables
 */
template <typename OpsTag, typename T>
class ProgVarGetSetOps<OpsTag, tag::VertexAttrib, T>
 : public ProgVarCommonOps<tag::VertexAttrib>
 , public ProgVarBaseSetOps<OpsTag, tag::VertexAttrib, tag::NativeTypes, T, 16>
{
protected:
	ProgVarGetSetOps(VertexAttribLoc valoc)
	 : ProgVarCommonOps<tag::VertexAttrib>(valoc)
	{ }
public:
	/// Set the value of the vertex attribute
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttrib}
	 */
	void SetValue(T value)
	{
		this->_do_set(_program, _location, value);
	}
};

template <typename OpsTag, typename T, std::size_t N>
class ProgVarGetSetOps<OpsTag, tag::VertexAttrib, Vector<T, N>>
 : public ProgVarCommonOps<tag::VertexAttrib>
 , public ProgVarBaseSetOps<OpsTag, tag::VertexAttrib, tag::NativeTypes, T, 4>
{
protected:
	ProgVarGetSetOps(VertexAttribLoc valoc)
	 : ProgVarCommonOps<tag::VertexAttrib>(valoc)
	{ }
public:
	void SetValue(const Vector<T, N>& value)
	{
		this->template _do_set<N>(_program, _location, Data(value));
	}
};

template <typename OpsTag, typename T, std::size_t R, std::size_t C>
class ProgVarGetSetOps<OpsTag, tag::VertexAttrib, Matrix<T, R, C>>
 : public ProgVarCommonOps<tag::VertexAttrib>
 , public ProgVarBaseSetOps<OpsTag, tag::VertexAttrib, tag::NativeTypes, T, 16>
{
protected:
	ProgVarGetSetOps(VertexAttribLoc valoc)
	 : ProgVarCommonOps<tag::VertexAttrib>(valoc)
	{ }
public:
	void SetValue(const Matrix<T, R, C>& value)
	{
		this->template _do_set<R*C>(_program, _location, Data(value));
	}
};

OGLPLUS_DECLARE_PROG_VAR(
	VertexAttrib,
	tag::ImplicitSel,
	tag::VertexAttrib,
	tag::NoTypecheck
)

/// Encapsulates vertex array attribute functionality
/**
 *  @ingroup shader_variables
 */
class VertexArrayAttrib
 : public ProgVarCommonOps<tag::VertexAttrib>
{
private:
	GLuint _attrib_index(void) const
	{
		if(this->_location < 0)
		{
			return GL_INVALID_INDEX;
		}
		return GLuint(this->_location);
	}
public:
	/// References the vertex attribute array at @p location
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	VertexArrayAttrib(VertexAttribSlot location)
	 : ProgVarCommonOps<tag::VertexAttrib>(VertexAttribLoc(GLint(location)))
	{ }

	/// References the vertex attribute array at @p location
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	VertexArrayAttrib(ProgramName program, VertexAttribSlot location)
	 : ProgVarCommonOps<tag::VertexAttrib>(
		VertexAttribLoc(program, GLint(location))
	){ }

	/// References the vertex attrib array @p identifier of the @p program
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	VertexArrayAttrib(ProgramName program, StrCRef identifier)
	 : ProgVarCommonOps<tag::VertexAttrib>(
		VertexAttribLoc(program, identifier)
	){ }

	/// Setup the properties of this vertex attribute array
	/** Equivalent to
	 *  @code
	 *  Pointer(valuer_per_vertex, data_type, false, 0, NULL)
	 *  @endcode
	 *  if @p data_type is DataType::Float or to
	 *  @code
	 *  LPointer(valuer_per_vertex, data_type, 0, NULL)
	 *  @endcode
	 *  if @p data_type is DataType::Double or to
	 *  @code
	 *  IPointer(valuer_per_vertex, data_type, 0, NULL)
	 *  @endcode
	 *  otherwise.
	 *
	 *  @note Consider using the templated version of Setup(), because
	 *  it is more portable. For example instead of:
	 *  @code
	 *  attr.Setup(3, DataType::Float);
	 *  @endcode
	 *  use
	 *  @code
	 *  attr.Setup<Vec3f>();
	 *  @endcode
	 *  or
	 *  @code
	 *  attr.Setup<GLfloat>(3);
	 *  @endcode
	 *
	 *  @see Pointer
	 *  @see IPointer
	 *  @see LPointer
	 *
	 *  @glsymbols
	 *  @glfunref{VertexAttribPointer}
	 */
	const VertexArrayAttrib& Setup(
		GLint values_per_vertex,
		DataType data_type
	) const
	{
		if(data_type == DataType::Float)
		{
			Pointer(
				values_per_vertex,
				data_type,
				false,
				0,
				nullptr
			);
		}
#ifdef GL_DOUBLE
		else if(data_type == DataType::Double)
		{
			LPointer(
				values_per_vertex,
				data_type,
				0,
				nullptr
			);
		}
#endif
		else
		{
			IPointer(
				values_per_vertex,
				data_type,
				0,
				nullptr
			);
		}
		return *this;
	}

	const VertexArrayAttrib& Setup(
		GLint values_per_vertex,
		std::integral_constant<DataType, DataType::Float>
	) const
	{
		return Pointer(
			values_per_vertex,
			DataType::Float,
			false,
			0,
			nullptr
		);
	}

#ifdef GL_DOUBLE
	const VertexArrayAttrib& Setup(
		GLint values_per_vertex,
		std::integral_constant<DataType, DataType::Double>
	) const
	{
		return LPointer(
			values_per_vertex,
			DataType::Double,
			0,
			nullptr
		);
	}
#endif

	template <DataType DataTypeValue>
	const VertexArrayAttrib& Setup(
		GLint values_per_vertex,
		std::integral_constant<DataType, DataTypeValue>
	) const
	{
		return IPointer(
			values_per_vertex,
			DataTypeValue,
			0,
			nullptr
		);
	}

	/// Setup the properties of this vertex attribute array
	/**
	 *  @see Pointer
	 *  @see IPointer
	 *  @see LPointer
	 *
	 *  @glsymbols
	 *  @glfunref{VertexAttribPointer}
	 */
	template <typename T>
	const VertexArrayAttrib& Setup(GLuint n = 1) const
	{
		typedef decltype(_get_et(TypeTag<T>())) elem_type;

		return Setup(
			_get_vpv(TypeTag<T>())*GLint(n),
			typename DataTypeCT<elem_type>::type()
		);
	}

	/// Setup the properties of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribPointer}
	 */
	const VertexArrayAttrib& Pointer(
		GLint values_per_vertex,
		DataType data_type,
		Boolean normalized,
		SizeType stride,
		const void* pointer
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribPointer)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			normalized._get(),
			stride,
			pointer
		);
		OGLPLUS_CHECK(
			VertexAttribPointer,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}

	/// Setup the properties of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribPointer}
	 */
	const VertexArrayAttrib& IPointer(
		GLint values_per_vertex,
		DataType data_type,
		SizeType stride,
		const void* pointer
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribIPointer)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			stride,
			pointer
		);
		OGLPLUS_CHECK(
			VertexAttribIPointer,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}


#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2 || GL_ARB_vertex_attrib_64bit
	/// Setup the properties of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribPointer}
	 */
	const VertexArrayAttrib& LPointer(
		GLint values_per_vertex,
		DataType data_type,
		SizeType stride,
		const void* pointer
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribLPointer)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			stride,
			pointer
		);
		OGLPLUS_CHECK(VertexAttribLPointer,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}
#else
	const VertexArrayAttrib& LPointer(
		GLint,
		DataType,
		SizeType,
		const void*
	) const
	{
		assert(!
		"The glVertexAttribLPointer function is "
		"required but not available! Double-precision "
		"vertex attribute values are not supported."
		);
		return *this;
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_vertex_attrib_binding
	/// Setup the format of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribFormat}
	 */
	const VertexArrayAttrib& Format(
		GLint values_per_vertex,
		DataType data_type,
		Boolean normalized,
		GLuint relative_offset
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribFormat)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			normalized._get(),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexAttribFormat,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}

	/// Setup the format of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribIFormat}
	 */
	const VertexArrayAttrib& IFormat(
		GLint values_per_vertex,
		DataType data_type,
		GLuint relative_offset
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribIFormat)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexAttribIFormat,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}

	/// Setup the format of this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{VertexAttribLFormat}
	 */
	const VertexArrayAttrib& LFormat(
		GLint values_per_vertex,
		DataType data_type,
		GLuint relative_offset
	) const
	{
		OGLPLUS_GLFUNC(VertexAttribLFormat)(
			_attrib_index(),
			values_per_vertex,
			GLenum(data_type),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexAttribLFormat,
			Error,
			EnumParam(data_type).
			Index(_location)
		);
		return *this;
	}
#endif

	/// Enables this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{EnableVertexArrayAttrib}
	 */
	const VertexArrayAttrib& Enable(void) const
	{
		OGLPLUS_GLFUNC(EnableVertexAttribArray)(_attrib_index());
		OGLPLUS_VERIFY(
			EnableVertexArrayAttrib,
			Error,
			Index(_location)
		);
		return *this;
	}

	/// Disables this vertex attribute array
	/**
	 *  @glsymbols
	 *  @glfunref{DisableVertexArrayAttrib}
	 */
	const VertexArrayAttrib& Disable(void) const
	{
		OGLPLUS_GLFUNC(DisableVertexAttribArray)(_attrib_index());
		OGLPLUS_VERIFY(
			DisableVertexArrayAttrib,
			Error,
			Index(_location)
		);
		return *this;
	}
};

/// Syntax sugar for construction of a VertexArrayAttrib object
/** Constructs an instance of VertexArrayAttrib for a vertex attribute
 *  identified by @p identifier in a @p program.
 *
 *  @see VertexArrayAttrib
 */
template <std::size_t N>
inline VertexArrayAttrib operator | (
	ProgramName program,
	const GLchar (&identifier)[N]
)
{
	return VertexArrayAttrib(program, identifier);
}

/// Syntax sugar for construction of a VertexArrayAttrib object
/** Constructs an instance of VertexArrayAttrib for a vertex attribute
 *  at the specified @p location in a @p program.
 *
 *  @see VertexArrayAttrib
 */
inline VertexArrayAttrib operator | (
	ProgramName program,
	GLuint location
)
{
	return VertexArrayAttrib(program, VertexAttribSlot(location));
}

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/vertex_attrib.ipp>
#endif

#endif // include guard
