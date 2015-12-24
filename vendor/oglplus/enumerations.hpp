/**
 *  @file oglplus/enumerations.hpp
 *  @brief Enumeration-related declarations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ENUMERATIONS_1107121519_HPP
#define OGLPLUS_ENUMERATIONS_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/config/enums.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/detail/enum_class.hpp>
#include <oglplus/detail/base_range.hpp>
#include <oglplus/utils/type_tag.hpp>
#include <vector>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY

/// Returns the name of the GL enumerated value for an OGLplus enum value
/** This function is overloaded for the enumerated types defined by @OGLplus
 *  and returns the GL constant name (without the 'GL_' prefix) as a c-string.
 *  @note The returned c-string is managed by the EnumValueName functions
 *  and should NOT be freed by the caller.
 *
 *  The result of this function is influenced by the #OGLPLUS_NO_ENUM_VALUE_NAMES
 *  preprocessor-symbol. If it is set to a nonzero value then EnumValueName(Enum)
 *  returns an empty string.
 *
 *  @ingroup enumerations
 *  @see OGLPLUS_NO_ENUM_VALUE_NAMES
 */
StrCRef EnumValueName(Enum enum_value);

/// Returns a @c Range of values in an @p Enumeration
/** This template function is available for the enumerated types defined by
 *  @OGLplus and returns a Range that allows to traverse all values of
 *  a particular @p Enumeration type.
 *
 *  The result of this function is influenced by the #OGLPLUS_NO_ENUM_VALUE_RANGES
 *  preprocessor-symbol. If it is set to a nonzero value then EnumValueRange<E>()
 *  returns an empty range.
 *
 *  @ingroup enumerations
 *  @see OGLPLUS_NO_ENUM_VALUE_RANGES
 */
template <typename Enumeration>
Range<Enum> EnumValueRange(void);

#else

namespace enums {

template <typename Base, typename Enum, template <Enum> class Transform>
class EnumToClass;

template <typename Enum>
struct EnumBaseType
{
	typedef GLenum Type;
};

template <typename Enum, Enum Value>
struct EnumAssocType
{
	typedef void Type;
};

template <typename Enum, Enum Value>
struct EnumAssocGLType
{
	static GLfloat _get(TypeTag<float>);
	static GLint _get(TypeTag<int>);
	static GLboolean _get(TypeTag<bool>);

	template <typename T>
	static T _get(TypeTag<T>);

	typedef decltype(_get(
		TypeTag<
			typename EnumAssocType<
				Enum,
				Value
			>::Type
		>()
	)) Type;
};

inline StrCRef ValueName_(GLenum*, GLenum)
{
	return StrCRef();
}

template <typename EnumType>
inline StrCRef EnumValueName(EnumType enum_value)
{
#if !OGLPLUS_NO_ENUM_VALUE_NAMES
	typedef typename EnumBaseType<EnumType>::Type BaseType;
	return ValueName_(
		&TypeTag<EnumType>(),
		BaseType(enum_value)
	);
#else
	OGLPLUS_FAKE_USE(enum_value);
	return StrCRef();
#endif
}

template <typename EnumType>
inline aux::CastIterRange<
	const typename EnumBaseType<EnumType>::Type*,
	EnumType
> EnumValueRange(void)
{
#if !OGLPLUS_NO_ENUM_VALUE_RANGES
	return ValueRange_(&TypeTag<EnumType>());
#else
	const typename EnumBaseType<EnumType>::Type *x = nullptr;
	return aux::CastIterRange<
		const typename EnumBaseType<EnumType>::Type*,
		EnumType
	>(x, x);

#endif
}

} // namespace enums
using enums::EnumValueName;
using enums::EnumValueRange;

namespace aux {

template <typename Enum, bool Copy>
class EnumArray;


template <typename Enum>
class EnumArray<Enum, false>
{
private:
	std::size_t _count;
	const GLenum* _enums;
protected:
	EnumArray(
		std::size_t count,
		const Enum* enums
	): _count(count)
	 , _enums(reinterpret_cast<const GLenum*>(enums))
	{ }
public:
	std::size_t Count(void) const
	{
		return _count;
	}

	const GLenum* Values(void) const
	{
		return _enums;
	}
};

template <typename Enum>
class EnumArray<Enum, true>
{
private:
	std::vector<GLenum> _enums;
protected:
	EnumArray(
		std::size_t count,
		const Enum* enums
	): _enums(count)
	{
		for(std::size_t i=0; i!=count; ++i)
		{
			_enums[i] = GLenum(enums[i]);
		}
	}
public:
	std::size_t Count(void) const
	{
		return _enums.size();
	}

	const GLenum* Values(void) const
	{
		return _enums.data();
	}
};

} // namespace aux

/// This class can be used to pass an array of multiple enums to functions
/**
 *  @note The lifetime of an instance of EnumArray must not exceed the lifetime
 *  of the original array of Enum from which the EnumArray was initialized.
 *
 *  @ingroup enumerations
 */
template <typename Enum>
class EnumArray
 : public aux::EnumArray<Enum, sizeof(Enum) != sizeof(GLenum)>
{
private:
	typedef aux::EnumArray<Enum, sizeof(Enum) != sizeof(GLenum)> Base_;
public:
	template <std::size_t N>
	EnumArray(const Enum (&enums)[N])
	 : Base_(N, enums)
	{ }

	EnumArray(const std::vector<Enum>& enums)
	 : Base_(enums.size(), enums.data())
	{ }

	EnumArray(std::size_t count, const Enum* enums)
	 : Base_(count, enums)
	{ }
};

#endif // OGLPLUS_DOCUMENTATION_ONLY

} // namespace oglplus

#endif // include guard
