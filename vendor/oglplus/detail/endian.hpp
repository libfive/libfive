/**
 *  @file oglplus/detail/endian.hpp
 *  @brief Utilities related to endiannes
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_ENDIAN_1210241959_HPP
#define OGLPLUS_AUX_ENDIAN_1210241959_HPP

#include <oglplus/detail/enum_class.hpp>
#include <cassert>
#include <algorithm>
#include <cstdint>

namespace oglplus {
namespace aux {

OGLPLUS_ENUM_CLASS_BEGIN(Endian, bool)
#if OGLPLUS_DOCUMENTATION_ONLY
	/// Indicates little endian byte order
	Little,
	/// Indicates big endian byte order
	Big
#else
	OGLPLUS_ENUM_CLASS_VALUE(Little, false)
	OGLPLUS_ENUM_CLASS_COMMA
	OGLPLUS_ENUM_CLASS_VALUE(Big, true)
#endif
OGLPLUS_ENUM_CLASS_END(Endian)

class EndianHelper
{
private:
	union {
		std::uint32_t x;
		std::uint8_t y[4];
	} _hlp;
public:
	EndianHelper(void)
	{
		_hlp.x = 0x12345678;
		assert(sizeof(_hlp.x) == sizeof(_hlp.y));
		assert(
			(IsLittle() && !IsBig()) ||
			(!IsLittle() && IsBig())
		);
	}

	bool IsBig(void) const
	{
		return	(_hlp.y[0] == 0x12) &&
			(_hlp.y[1] == 0x34) &&
			(_hlp.y[2] == 0x56) &&
			(_hlp.y[3] == 0x78);
	}

	bool IsLittle(void) const
	{
		return	(_hlp.y[3] == 0x12) &&
			(_hlp.y[2] == 0x34) &&
			(_hlp.y[1] == 0x56) &&
			(_hlp.y[0] == 0x78);
	}
};

inline Endian NativeByteOrder(void)
{
	static Endian result(Endian(EndianHelper().IsBig()));
	return result;
}

struct EndianNoReorder
{
	template <typename T>
	static inline T Reorder(T value)
	{
		return value;
	}
};

struct EndianDoReorder
{
	template <typename T>
	static T DoReorder(T value)
	{
		union {
			T x;
			std::uint8_t y[sizeof(T)];
		} _hlp;
		_hlp.x = value;

		assert(sizeof(_hlp.x) == sizeof(_hlp.y));

		std::reverse(_hlp.y, _hlp.y+sizeof(T));
		return _hlp.x;
	}

	template <typename T>
	static T DoForward(T value)
	{
		return value;
	}

	template <typename T>
	static T Reorder(T value)
	{
		if(sizeof(T) == 1)
			return DoForward(value);
		else return DoReorder(value);
	}
};

template <typename T>
inline T ReorderFromTo(Endian from, Endian to, T value)
{
	if(from == to) return EndianNoReorder::Reorder(value);
	else return EndianDoReorder::Reorder(value);
}

template <typename T>
inline T ReorderToNative(Endian from, T value)
{
	return ReorderFromTo(from, NativeByteOrder(), value);
}

template <Endian From, Endian To>
struct EndianReorderer;

template <>
struct EndianReorderer<Endian::Little, Endian::Little>
 : EndianNoReorder
{ };

template <>
struct EndianReorderer<Endian::Big, Endian::Big>
 : EndianNoReorder
{ };

template <>
struct EndianReorderer<Endian::Big, Endian::Little>
 : EndianDoReorder
{ };

template <>
struct EndianReorderer<Endian::Little, Endian::Big>
 : EndianDoReorder
{ };

template <Endian From>
struct Reorder
{
	template <Endian To, typename T>
	static inline T As(T value)
	{
		return EndianReorderer<From, To>::Reorder(value);
	}
};

} // namespace aux
} // namespace oglplus

#endif // include guard
