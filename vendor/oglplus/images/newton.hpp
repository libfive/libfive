/**
 *  @file oglplus/images/newton.hpp
 *  @brief Newton fractal image generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_NEWTON_1107121519_HPP
#define OGLPLUS_IMAGES_NEWTON_1107121519_HPP

#include <oglplus/images/image.hpp>
#include <oglplus/math/vector.hpp>

#include <cassert>

namespace oglplus {
namespace images {

/// A generator of Newton fractal 2D images
/** This class generates two dimensional RGB images of rendering of the newton
 *  polynomial root finding numeric method of a specified function (X^3-1 by
 *  default). You can also specify two colors for the gradient used for the
 *  coloring of the fractal.
 *
 *  The polynomials to be used are specified by a class wrapping two static
 *  functions @c f and @c df. @c f is returns the value of the polynomial
 *  for a specified complex value and @c df returns the value of the first
 *  derivative of the polynomial for a specified complex value as shown
 *  in the following code example:
 *  @code
 *  struct MyPoly
 *  {
 *    static Vec2f f(Vec2f x);
 *    static Vec2f df(Vec2f x);
 *  };
 *  @endcode
 *
 *  @ingroup image_load_gen
 */
class NewtonFractal
 : public Image
{
private:
	// complex number division
	static Vec2f _cdiv(Vec2f a, Vec2f b)
	{
		float d = Dot(b, b);
		if(d == 0.0f) return a;
		else return Vec2f(
			(a.x()*b.x() + a.y()*b.y()) / d,
			(a.y()*b.x() - a.x()*b.y()) / d
		);
	}

	template <typename T>
	static T _mix(T a, T b, float coef)
	{
		return a*(1.0f - coef) + b*coef;
	}

	template <typename Function, typename Mixer, std::size_t N>
	void _make(
		GLsizei width,
		GLsizei height,
		Function,
		Mixer mixer,
		Vec2f lb,
		Vec2f rt,
		Vector<float, N> c1,
		Vector<float, N> c2
	)
	{
		auto p = this->_begin<GLfloat>();

		for(GLsizei i=0; i!=width; ++i)
		for(GLsizei j=0; j!=height; ++j)
		{
			Vec2f z(
				_mix(lb.x(), rt.x(), float(i)/float(width-1)),
				_mix(lb.y(), rt.y(), float(j)/float(height-1))
			);
			std::size_t n, max = 256;
			for(n = 0; n != max; ++n)
			{
				Vec2f zn = z - _cdiv(
					Function::f(z),
					Function::df(z)
				);
				if(Distance(zn, z) < 0.00001f) break;
				z = zn;
			}
			Vector<float, N> c = _mix(
				c1,
				c2,
				float(mixer(float(n) / float(max-1)))
			);
			for(n=0; n!=N; ++n)
			{
				assert(p != this->_end<GLfloat>());
				*p = c.At(n);
				++p;
			}
		}
		assert(p == this->_end<GLfloat>());
	}
public:
	/// The X^3-1 function and its derivation
	struct X3Minus1
	{
		static Vec2f f(Vec2f n)
		{
			return Vec2f(
				n.x()*n.x()*n.x() - 3.f*n.x()*n.y()*n.y() - 1.f,
				-n.y()*n.y()*n.y() + 3.f*n.x()*n.x()*n.y()
			);
		}

		static Vec2f df(Vec2f n)
		{
			return 3.0f * Vec2f(
				n.x()*n.x() - n.y()*n.y(),
				2.0f * n.x() * n.y()
			);
		}
	};

	/// The X^4-1 function and its derivation
	struct X4Minus1
	{
		static Vec2f f(Vec2f n)
		{
			return Vec2f(
				n.x()*n.x()*n.x()*n.x() +
				n.y()*n.y()*n.y()*n.y() -
				6.f*n.x()*n.x()*n.y()*n.y() - 1.f,
				4.f*n.x()*n.x()*n.x()*n.y() -
				4.f*n.x()*n.y()*n.y()*n.y()
			);
		}

		static Vec2f df(Vec2f n)
		{
			return 4.0f * Vec2f(
				n.x()*n.x()*n.x() - 3.f*n.x()*n.y()*n.y(),
				-n.y()*n.y()*n.y() + 3.f*n.x()*n.x()*n.y()
			);
		}
	};

	typedef X3Minus1 DefaultFunction;

	struct NoopMixer
	{
		template <typename T>
		T operator()(T value) const
		{
			return value;
		}
	};

	struct PowMixer
	{
		float _exponent;

		PowMixer(float exponent)
		 : _exponent(exponent)
		{ }

		template <typename T>
		T operator()(T value) const
		{
			return std::pow(value, T(_exponent));
		}
	};

	typedef NoopMixer DefaultMixer;

#if OGLPLUS_DOCUMENTATION_ONLY || !OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS
	/// Creates a RGB texture using c1 and c2 for colorizing of the fractal
	/**
	 *  @param width the width of the image in pixels
	 *  @param height the height of the image in pixels
	 *  @param c1 the "low" end (minimal number of iterations) of the
	 *    gradient used for colorizing of the fractal
	 *  @param c2 the "high" end (maximal number of iterations) of the
	 *    gradient used for colorizing of the fractal
	 *  @param lb the left bottom coordinate of the viewport in the complex
	 *    space to be rendered.
	 *  @param rt the right top coordinate of the viewport in the complex
	 *    space to be rendered.
	 *  @param func the object wrapping the functions calculating the value
	 *    of the polynomial and its first derivative (see the class
	 *    documentation).
	 *  @param mixer function controling the colorization
	 */
	template <
		typename Function = DefaultFunction,
		typename Mixer = DefaultMixer
	>
	NewtonFractal(
		SizeType width,
		SizeType height,
		Vec3f c1,
		Vec3f c2,
		Vec2f lb = Vec2f(-1.0f, -1.0f),
		Vec2f rt = Vec2f( 1.0f,  1.0f),
		Function func = Function(),
		Mixer mixer = Mixer()
	): Image(width, height, 1, 3, &TypeTag<GLfloat>())
	{
		_make(width, height, func, mixer, lb, rt, c1, c2);
	}

	/// Creates a Red texture colorized from black to red
	/**
	 *  @param width the width of the image in pixels
	 *  @param height the height of the image in pixels
	 *  @param func the object wrapping the functions calculating the value
	 *    of the polynomial and its first derivative (see the class
	 *    documentation).
	 *  @param mixer function controling the colorization
	 */
	template <
		typename Function = DefaultFunction,
		typename Mixer = DefaultMixer
	>
	NewtonFractal(
		SizeType width,
		SizeType height,
		Function func = Function(),
		Mixer mixer = Mixer()
	): Image(width, height, 1, 1, &TypeTag<GLfloat>())
	{
		_make(
			width, height,
			func,
			mixer,
			Vec2f(-1.0f, -1.0f), Vec2f(1.0f, 1.0f),
			Vec1f(0.0f), Vec1f(1.0f)
		);
	}
#else
	template <typename Function, typename Mixer>
	NewtonFractal(
		SizeType width,
		SizeType height,
		Vec3f c1,
		Vec3f c2,
		Vec2f lb,
		Vec2f rt,
		Function func,
		Mixer mixer
	): Image(width, height, 1, 3, (GLfloat*)0)
	{
		_make(width, height, func, mixer, lb, rt, c1, c2);
	}

	template <typename Function, typename Mixer>
	NewtonFractal(
		SizeType width,
		SizeType height,
		Function func,
		Mixer mixer
	): Image(width, height, 1, 1, (GLfloat*)0)
	{
		_make(
			width, height,
			func,
			mixer,
			Vec2f(-1.0f, -1.0f), Vec2f(1.0f, 1.0f),
			Vec1f(0.0f), Vec1f(1.0f)
		);
	}
#endif
};

} // images
} // oglplus

#endif // include guard
