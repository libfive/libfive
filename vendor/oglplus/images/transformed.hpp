/**
 *  @file oglplus/images/transformed.hpp
 *  @brief Filter transforming image RGB components by a 4x4 matrix
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_TRANSFORMED_1107121519_HPP
#define OGLPLUS_IMAGES_TRANSFORMED_1107121519_HPP

#include <oglplus/images/filtered.hpp>
#include <oglplus/math/matrix.hpp>

namespace oglplus {
namespace images {

/// A filter transforming image RGB components by a 4x4 matrix
/**
 *  @ingroup image_load_gen
 */
template <typename T, typename std::size_t N>
class TransformComponents
 : public FilteredImage<T, N>
{
private:
	struct _filter
	{
		Mat4d _matrix;

		_filter(const Mat4d& matrix)
		 : _matrix(matrix)
		{ }

		template <typename Extractor, typename Sampler>
		Vector<T, N> operator()(
			const Extractor& extractor,
			const Sampler& sampler,
			T one
		) const
		{
			const Vector<double, 4> c(Vector<double, 4>(
				extractor(sampler(0, 0, 0)),
				1.0
			));
			const Vector<double, N> res = _matrix*c*one;
			return Vector<T, N>(res);
		}
	};
public:
	typedef FilteredImage<T, N> Filtered;

	TransformComponents(const Image& input, const Mat4d& matrix)
	 : Filtered(
		input,
		_filter(matrix),
		typename Filtered::DefaultSampler(),
		typename Filtered::FromRGB()
	)
	{
		this->_format = PixelDataFormat::RGB;
		this->_internal = PixelDataInternalFormat::RGB;
	}
};

/// A filter flipping/reorienting image axes
/**
 *  @ingroup image_load_gen
 */
template <typename T, std::size_t N>
class FlipImageAxes
 : public FilteredImage<T, N>
{
private:
	typedef FilteredImage<T, N> Filtered;

	static Matrix<double, 4, 4> _make_matrix(
		int x_axis,
		int y_axis,
		int z_axis
	)
	{
		typedef Vector<double, 3> V3d;
		V3d x = (x_axis<0) ? -V3d::Unit(-x_axis) : V3d::Unit(x_axis);
		V3d y = (y_axis<0) ? -V3d::Unit(-y_axis) : V3d::Unit(y_axis);
		V3d z = (z_axis<0) ? -V3d::Unit(-z_axis) : V3d::Unit(z_axis);

		return Matrix<double, 4, 4>(
			x.x(), x.y(), x.z(), 0,
			y.x(), y.y(), y.z(), 0,
			z.x(), z.y(), z.z(), 0,
			    0,     0,     0, 1
		);
	}
public:
	FlipImageAxes(const Image& image, int x_axis, int y_axis, int z_axis)
	 : Filtered(
		image,
		typename Filtered::DefaultFilter(),
		typename Filtered::template MatrixTransformSampler<
			typename Filtered::RepeatSample
		>(_make_matrix(x_axis, y_axis, z_axis)),
		typename Filtered::template FirstNComponents<N>()
	)
	{ }
};

} // images
} // oglplus

#endif // include guard
