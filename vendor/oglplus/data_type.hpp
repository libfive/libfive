/**
 *  @file oglplus/data_type.hpp
 *  @brief Data type-related declarations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DATA_TYPE_1107121519_HPP
#define OGLPLUS_DATA_TYPE_1107121519_HPP

#include <oglplus/enums/data_type.hpp>
#include <oglplus/enums/sl_data_type.hpp>
#include <oglplus/utils/type_tag.hpp>
#include <type_traits>

namespace oglplus {

// Compile-time data type getter
template <typename T>
struct DataTypeCT;

template <>
struct DataTypeCT<GLbyte>
 : public std::integral_constant<DataType, DataType::Byte>
{ };

template <>
struct DataTypeCT<GLshort>
 : public std::integral_constant<DataType, DataType::Short>
{ };

template <>
struct DataTypeCT<GLint>
 : public std::integral_constant<DataType, DataType::Int>
{ };

template <>
struct DataTypeCT<GLubyte>
 : public std::integral_constant<DataType, DataType::UnsignedByte>
{ };

template <>
struct DataTypeCT<GLushort>
 : public std::integral_constant<DataType, DataType::UnsignedShort>
{ };

template <>
struct DataTypeCT<GLuint>
 : public std::integral_constant<DataType, DataType::UnsignedInt>
{ };

template <>
struct DataTypeCT<GLfloat>
 : public std::integral_constant<DataType, DataType::Float>
{ };

#ifdef GL_DOUBLE
template <>
struct DataTypeCT<GLdouble>
 : public std::integral_constant<DataType, DataType::Double>
{ };
#endif

/// Returns the DataType for the specified type @p T
template <typename T>
inline DataType GetDataType(void)
OGLPLUS_NOEXCEPT(true)
{
	return DataType(DataTypeCT<T>::value);
}

namespace aux {

template <typename T>
std::true_type _get_is_gl_data_type(T*, typename DataTypeCT<T>::type* = nullptr);
std::false_type _get_is_gl_data_type(...);

template <typename T>
struct _is_gl_data_type
{
	typedef decltype(_get_is_gl_data_type(&TypeTag<T>())) type;
};

} // namespace aux

// Checks if type T is an OpenGL data type
template <typename T>
struct IsGLDataType
 : public aux::_is_gl_data_type<T>::type
{ };

} // namespace oglplus

#endif // include guard
