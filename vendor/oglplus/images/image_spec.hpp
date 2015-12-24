/**
 *  @file oglplus/images/image_spec.hpp
 *  @brief Texture image specification
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_IMAGE_SPEC_1202210916_HPP
#define OGLPLUS_IMAGES_IMAGE_SPEC_1202210916_HPP

#include <oglplus/pixel_data.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/one_of.hpp>
#include <oglplus/assert.hpp>

namespace oglplus {
namespace images {

/// Class containing data for texture image specification
struct ImageSpecData
{
	typedef OneOf<
		GLenum,
		std::tuple<
			DataType,
			PixelDataType
		>
	> PixDataType;

	GLsizei width;
	GLsizei height;
	GLsizei depth;
	PixelDataFormat format;
	PixelDataInternalFormat internal_format;
	PixDataType data_type;
	const void* data_ptr;

	ImageSpecData(
		SizeType w,
		SizeType h,
		SizeType d,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		PixDataType type,
		const void* data
	): width(w)
	 , height(h)
	 , depth(d)
	 , format(fmt)
	 , internal_format(ifmt)
	 , data_type(type)
	 , data_ptr(data)
	{ }
};

struct ImageSpec
 : ImageSpecData
{
	typedef ImageSpecData _base;
	static PixelDataInternalFormat _conv(PixelDataFormat fmt)
	{
		return PixelDataInternalFormat(GLenum(fmt));
	}
	static PixelDataFormat _conv(PixelDataInternalFormat ifmt)
	{
		return PixelDataFormat(GLenum(ifmt));
	}

	ImageSpec(void)
	 : _base(
		1, 1, 1,
		PixelDataFormat(),
		PixelDataInternalFormat(),
		PixelDataType(),
		nullptr
	)
	{ }

	ImageSpec(
		SizeType w,
		SizeType h,
		PixelDataInternalFormat ifmt
	): _base(w, h, 1, _conv(ifmt), ifmt, PixelDataType(), nullptr)
	{ }

	ImageSpec(
		SizeType w,
		SizeType h,
		PixelDataFormat fmt,
		PixDataType type
	): _base(w, h, 1, fmt, _conv(fmt), type, nullptr)
	{ }

	ImageSpec(
		SizeType w,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		PixDataType type
	): _base(w, 1, 1, fmt, ifmt, type, nullptr)
	{ }

	ImageSpec(
		SizeType w,
		SizeType h,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		PixDataType type
	): _base(w, h, 1, fmt, ifmt, type, nullptr)
	{ }

	ImageSpec(
		SizeType w,
		SizeType h,
		SizeType d,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		PixDataType type
	): _base(w, h, d, fmt, ifmt, type, nullptr)
	{ }

	template <typename T>
	ImageSpec(
		SizeType w,
		SizeType h,
		PixelDataFormat fmt,
		const T* data,
		typename std::enable_if<
			IsGLDataType<T>::value,
			const T*
		>::type = nullptr
	): _base(w, h, 1, fmt, _conv(fmt), GetDataType<T>(), data)
	{ }

	template <typename T>
	ImageSpec(
		SizeType w,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		const T* data,
		typename std::enable_if<
			IsGLDataType<T>::value,
			const T*
		>::type = nullptr

	): _base(w, 1, 1, fmt, ifmt, GetDataType<T>(), data)
	{ }

	template <typename T>
	ImageSpec(
		SizeType w,
		SizeType h,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		const T* data,
		typename std::enable_if<
			IsGLDataType<T>::value,
			const T*
		>::type = nullptr

	): _base(w, h, 1, fmt, ifmt, GetDataType<T>(), data)
	{ }

	template <typename T>
	ImageSpec(
		SizeType w,
		SizeType h,
		SizeType d,
		PixelDataFormat fmt,
		PixelDataInternalFormat ifmt,
		const T* data,
		typename std::enable_if<
			IsGLDataType<T>::value,
			const T*
		>::type = nullptr

	): _base(w, h, d, fmt, ifmt, GetDataType<T>(), data)
	{ }

	ImageSpec& Format(PixelDataFormat fmt)
	{
		format = fmt;
		return *this;
	}

	ImageSpec& Format(PixelDataInternalFormat ifmt)
	{
		internal_format = ifmt;
		return *this;
	}

	ImageSpec& Type(PixDataType type)
	{
		data_type = type;
		return *this;
	}

	ImageSpec& NextDim(SizeType dim)
	{
		assert(dim > 0);
		if(width <= 1)
		{
			width = dim;
		}
		else if(height <= 1)
		{
			height = dim;
		}
		else if(depth <= 1)
		{
			depth = dim;
		}
		else OGLPLUS_ABORT("Too many dimensions specified!");
		return *this;
	}
};

inline ImageSpec& operator << (ImageSpec& that, SizeType dim)
{
	return that.NextDim(dim);
}

inline ImageSpec&& operator << (ImageSpec&& that, SizeType dim)
{
	return std::move(that.NextDim(dim));
}

inline ImageSpec& operator << (ImageSpec& that, PixelDataFormat f)
{
	return that.Format(f);
}

inline ImageSpec&& operator << (ImageSpec&& that, PixelDataFormat f)
{
	return std::move(that.Format(f));
}

inline ImageSpec& operator << (ImageSpec& that, PixelDataInternalFormat f)
{
	return that.Format(f);
}

inline ImageSpec&& operator << (ImageSpec&& that, PixelDataInternalFormat f)
{
	return std::move(that.Format(f));
}

} // namespace images
} // namespace oglplus

#endif // include guard
