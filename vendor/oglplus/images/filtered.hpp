/**
 *  @file oglplus/images/filtered.hpp
 *  @brief Base class for image filters
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_FILTERED_1107121519_HPP
#define OGLPLUS_IMAGES_FILTERED_1107121519_HPP

#include <oglplus/images/image.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/matrix.hpp>

#include <cassert>
#include <cmath>

namespace oglplus {
namespace images {

/// Base class for various image filters
/**
 *  @note Do not use this class directly, use the derived filters instead.
 *  @ingroup image_load_gen
 */
template <typename T, unsigned CH>
class FilteredImage
 : public Image
{
private:
	static_assert(
		CH > 0 && CH <= 4,
		"Number of channels must be between 1 and 4"
	);
private:
	template <typename Filter, typename Sampler, typename Extractor>
	void _calculate(
		const Image& input,
		Filter filter,
		Sampler sampler,
		Extractor extractor,
		T one
	)
	{
		sampler.SetInput(input);
		auto p = this->_begin<T>();
		GLsizei w = input.Width(), h = input.Height(), d = input.Depth();

		for(GLsizei k=0; k<d; ++k)
		for(GLsizei j=0; j<h; ++j)
		for(GLsizei i=0; i<w; ++i)
		{
			sampler.SetOrigin(i, j, k);

			Vector<T, CH> outv = filter(extractor, sampler, one);

			for(unsigned ci=0; ci!=CH; ++ci)
			{
				assert(p != this->_end<T>());
				*p = outv.At(ci);
				++p;
			}
		}
		assert(p == this->_end<T>());
	}
public:
	struct DefaultFilter
	{
		template <typename Extractor, typename Sampler>
		Vector<T, CH> operator()(
			const Extractor& extractor,
			const Sampler& sampler,
			T one
		) const
		{
			return Vector<T, CH>(extractor(sampler(0, 0, 0))*one);
		}
	};

	struct NoCoordTransform
	{
		void operator()(int, int, int, unsigned, unsigned, unsigned) const
		{
		}
	};

	class MatrixCoordTransform
	{
	private:
		Matrix<double, 4, 4> _transf;
	public:
		MatrixCoordTransform(const Matrix<double, 4, 4>& transf)
		 : _transf(transf)
		{ }

		void operator()(
			int& x, int& y, int& z,
			double w, double h, double d
		) const
		{
			Vector<double, 4> in((x+0.5)/w, (y+0.5)/h, (z+0.5)/d, 1);
			Vector<double, 4> out = _transf * in;

			x = int(std::floor(out.x()*w));
			y = int(std::floor(out.y()*h));
			z = int(std::floor(out.z()*d));
		}
	};

	struct RepeatSample
	{
		Vector<GLdouble, 4> operator()(
			const Image& image,
			unsigned width,
			unsigned height,
			unsigned depth,
			int xpos,
			int ypos,
			int zpos
		) const
		{
			if(xpos >= int(width)) xpos %= width;
			while(xpos < 0) xpos += width;

			if(ypos >= int(height)) ypos %= height;
			while(ypos < 0) ypos += height;

			if(zpos >= int(depth)) zpos %= depth;
			while(zpos < 0) zpos += depth;

			assert((xpos >= 0) && (xpos < int(width)));
			assert((ypos >= 0) && (ypos < int(height)));
			assert((zpos >= 0) && (zpos < int(depth)));

			return image.Pixel(xpos, ypos, zpos);
		}
	};

	template <typename Transform, typename SampleFunc>
	class SamplerTpl
	{
	private:
		Transform _transf;
		SampleFunc _sample;

		const Image* _image;
		int _ori_x, _ori_y, _ori_z;
	public:
		SamplerTpl(
			const Transform& transf = Transform(),
			const SampleFunc& sample = SampleFunc()
		): _transf(transf)
		 , _sample(sample)
		 , _image(nullptr)
		 , _ori_x(0)
		 , _ori_y(0)
		 , _ori_z(0)
		{ }

		void SetInput(const Image& image)
		{
			_image = &image;
		}

		void SetOrigin(
			GLsizei x,
			GLsizei y,
			GLsizei z
		)
		{
			_ori_x = x;
			_ori_y = y;
			_ori_z = z;

			assert(_image);

			_transf(
				_ori_x,
				_ori_y,
				_ori_z,
				_image->Width(),
				_image->Height(),
				_image->Depth()
			);
		}

		Vector<GLdouble, 4> operator()(
			int xoffs,
			int yoffs,
			int zoffs
		) const
		{
			assert(_image != nullptr);
			return _sample(
				*_image,
				_image->Width(),
				_image->Height(),
				_image->Depth(),
				_ori_x+xoffs,
				_ori_y+yoffs,
				_ori_z+zoffs
			);
		}
	};

	template <typename SampleFunc>
	struct SimpleSampler
	 : SamplerTpl<NoCoordTransform, SampleFunc>
	{
		SimpleSampler(const SampleFunc& sample = SampleFunc())
		 : SamplerTpl<NoCoordTransform, SampleFunc>(
			NoCoordTransform(),
			sample
		)
		{ }
	};

	struct DefaultSampler
	 : SimpleSampler<RepeatSample>
	{ };

	template <typename SampleFunc>
	struct MatrixTransformSampler
	 : SamplerTpl<MatrixCoordTransform, SampleFunc>
	{
		MatrixTransformSampler(
			const Matrix<double, 4, 4>& transf,
			const SampleFunc& sample = SampleFunc()
		): SamplerTpl<MatrixCoordTransform, SampleFunc>(
			MatrixCoordTransform(transf),
			sample
		)
		{ }
	};

	/// Extractor that allows to specify which component to use as input
	template <unsigned I>
	struct FromComponentI
	{
		double operator()(const Vector<double, 4>& v) const
		{
			return v.At(I);
		}
	};

	template <unsigned N>
	struct FirstNComponents
	{
		Vector<double, N> operator()(const Vector<double, 4>& v) const
		{
			return Vector<double, N>(v);
		}
	};

	/// Extractor selecting the Red component of the input image
	typedef FromComponentI<0> FromRed;
	/// Extractor selecting the Green component of the input image
	typedef FromComponentI<1> FromGreen;
	/// Extractor selecting the Blue component of the input image
	typedef FromComponentI<2> FromBlue;
	/// Extractor selecting the Alpha component of the input image
	typedef FromComponentI<3> FromAlpha;

	typedef FirstNComponents<3> FromRGB;
	typedef FirstNComponents<4> FromRGBA;

	template <typename Filter, typename Sampler, typename Extractor>
	FilteredImage(
		const Image& input,
		Filter filter,
		Sampler sampler,
		Extractor extractor
	): Image(
		input.Width(),
		input.Height(),
		input.Depth(),
		CH,
		&TypeTag<T>()
	)
	{
		_calculate(
			input,
			filter,
			sampler,
			extractor,
			this->_one(TypeTag<T>())
		);
	}
};

} // images
} // oglplus

#endif // include guard
