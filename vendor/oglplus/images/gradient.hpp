/**
 *  @file oglplus/images/gradient.hpp
 *  @brief Gradient image generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_GRADIENT_1211090818_HPP
#define OGLPLUS_IMAGES_GRADIENT_1211090818_HPP

#include <oglplus/images/image.hpp>

#include <cassert>
#include <map>

namespace oglplus {
namespace images {

/// Creates a 1D, 2D or 3D linear gradient image
/**
 *  @ingroup image_load_gen
 */
class LinearGradient
 : public Image
{
private:
	template <
		typename P,
		typename T,
		std::size_t N
	> static void _make_gradient(
		const Vector<T, N>& background,
		SizeType dimension,
		const std::map<P, Vector<T, N>>& points,
		std::vector<Vector<T, N>>& gradient
	)
	{
		auto point_cur = points.begin(), point_end = points.end();
		auto grad_cur = gradient.begin(), grad_end = gradient.end();

		const P step = P(1)/P(dimension);

		P prev_point = P(0);
		P curr_point = P(0);
		P next_point = P(1);

		Vector<T, N> prev_color = background;
		Vector<T, N> next_color = background;


		for(GLsizei p=0; p!=dimension; ++p)
		{
			if(point_cur != point_end)
			{
				if(point_cur->first <= curr_point)
				{
					prev_point = point_cur->first;
					auto npp = point_cur;
					++npp;
					if(npp != point_end)
					{
						prev_color = point_cur->second;
						next_point = npp->first;
						next_color = npp->second;
						point_cur = npp;
					}
					else
					{
						prev_color = background;
						next_point = P(1);
						next_color = background;
					}
				}
				else
				{
					next_point = point_cur->first;
				}
			}
			P factor = P(0);
			if(prev_point != next_point)
				factor =(curr_point - prev_point)/
					(next_point - prev_point);

			assert(grad_cur != grad_end);
			*grad_cur = prev_color*(P(1)-factor)+next_color*factor;
			++grad_cur;

			curr_point += step;
		}
		assert(grad_cur == grad_end);
		OGLPLUS_FAKE_USE(grad_end);
	}

	static GLubyte _cc_max(void)
	{
		return ~GLubyte(0);
	}

	template <typename T>
	static T _clamp(T v)
	{
		if(v < T(0)) return T(0);
		if(v > T(1)) return T(1);
		return v;
	}

	template <typename T, std::size_t N>
	static void _apply_gradient(
		const std::vector<Vector<T, N>>& grad0,
		GLubyte* dp,
		GLubyte* de
	)
	{
		auto gb0 = grad0.begin(), ge0 = grad0.end();

		for(auto gp0=gb0; gp0!=ge0; ++gp0)
		{
			Vector<T, N> color = *gp0;
			for(std::size_t c=0; c!=N; ++c)
			{
				assert(dp != de);
				*dp++ = GLubyte(_clamp(color.At(c))*_cc_max());
			}
		}
		OGLPLUS_FAKE_USE(de);
		assert(dp == de);
	}

	template <typename Combine, typename T, std::size_t N>
	static void _apply_gradient(
		Combine combine,
		const std::vector<Vector<T, N>>& grad0,
		const std::vector<Vector<T, N>>& grad1,
		GLubyte* dp,
		GLubyte* de
	)
	{
		auto gb0 = grad0.begin(), ge0 = grad0.end();
		auto gb1 = grad1.begin(), ge1 = grad1.end();

		for(auto gp0=gb0; gp0!=ge0; ++gp0)
		for(auto gp1=gb1; gp1!=ge1; ++gp1)
		{
			Vector<T, N> color = combine(*gp0, *gp1);
			for(std::size_t c=0; c!=N; ++c)
			{
				assert(dp != de);
				*dp++ = GLubyte(_clamp(color.At(c))*_cc_max());
			}
		}
		OGLPLUS_FAKE_USE(de);
		assert(dp == de);
	}

	template <typename Combine, typename T, std::size_t N>
	static void _apply_gradient(
		Combine combine,
		const std::vector<Vector<T, N>>& grad0,
		const std::vector<Vector<T, N>>& grad1,
		const std::vector<Vector<T, N>>& grad2,
		GLubyte* dp,
		GLubyte* de
	)
	{
		auto gb0 = grad0.begin(), ge0 = grad0.end();
		auto gb1 = grad1.begin(), ge1 = grad1.end();
		auto gb2 = grad2.begin(), ge2 = grad2.end();

		for(auto gp0=gb0; gp0!=ge0; ++gp0)
		for(auto gp1=gb1; gp1!=ge1; ++gp1)
		for(auto gp2=gb2; gp2!=ge2; ++gp2)
		{
			Vector<T, N> color = combine(*gp0, *gp1, *gp2);
			for(std::size_t c=0; c!=N; ++c)
			{
				assert(dp != de);
				*dp++ = GLubyte(_clamp(color.At(c))*_cc_max());
			}
		}
		OGLPLUS_FAKE_USE(de);
		assert(dp == de);
	}
public:
	struct AddComponents
	{
		template <typename T, std::size_t N>
		Vector<T, N> operator()(
			const Vector<T, N>& a,
			const Vector<T, N>& b
		) const
		{
			return a + b;
		}

		template <typename T, std::size_t N>
		Vector<T, N> operator()(
			const Vector<T, N>& a,
			const Vector<T, N>& b,
			const Vector<T, N>& c
		) const
		{
			return a + b + c;
		}
	};

	template <typename P, typename T, std::size_t N>
	LinearGradient(
		SizeType width,
		const Vector<T, N>& background,
		const std::map<P, Vector<T, N>>& x_points
	): Image(width, 1, 1, N, &TypeTag<GLubyte>())
	{
		std::vector<Vector<T, N>> x_gradient((std::size_t(width)));
		_make_gradient(
			background,
			width,
			x_points,
			x_gradient
		);
		_apply_gradient(x_gradient, _begin_ub(), _end_ub());
	}

	template <typename P, typename T, std::size_t N, typename Combine>
	LinearGradient(
		SizeType width,
		SizeType height,
		const Vector<T, N>& background,
		const std::map<P, Vector<T, N>>& x_points,
		const std::map<P, Vector<T, N>>& y_points,
		Combine combine
	): Image(width, height, 1, N, &TypeTag<GLubyte>())
	{
		std::vector<Vector<T, N>> y_gradient((std::size_t(height)));
		_make_gradient(
			background,
			height,
			y_points,
			y_gradient
		);

		std::vector<Vector<T, N>> x_gradient((std::size_t(width)));
		_make_gradient(
			background,
			width,
			x_points,
			x_gradient
		);

		_apply_gradient(
			combine,
			y_gradient,
			x_gradient,
			_begin_ub(),
			_end_ub()
		);
	}

	template <typename P, typename T, std::size_t N, typename Combine>
	LinearGradient(
		SizeType width,
		SizeType height,
		SizeType depth,
		const Vector<T, N>& background,
		const std::map<P, Vector<T, N>>& x_points,
		const std::map<P, Vector<T, N>>& y_points,
		const std::map<P, Vector<T, N>>& z_points,
		Combine combine
	): Image(width, height, depth, N, &TypeTag<GLubyte>())
	{
		std::vector<Vector<T, N>> z_gradient(depth);
		_make_gradient(
			background,
			depth,
			z_points,
			z_gradient
		);

		std::vector<Vector<T, N>> y_gradient(height);
		_make_gradient(
			background,
			height,
			y_points,
			y_gradient
		);

		std::vector<Vector<T, N>> x_gradient(width);
		_make_gradient(
			background,
			width,
			x_points,
			x_gradient
		);

		_apply_gradient(
			combine,
			z_gradient,
			y_gradient,
			x_gradient,
			_begin_ub(),
			_end_ub()
		);
	}
};

} // images
} // oglplus

#endif // include guard
