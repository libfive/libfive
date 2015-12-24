/**
 *  @file oglplus/images/cell.hpp
 *  @brief Voronoi/Worley cell image generators
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_CELL_1107121519_HPP
#define OGLPLUS_IMAGES_CELL_1107121519_HPP

#include <oglplus/images/image.hpp>
#include <oglplus/assert.hpp>

#include <vector>

namespace oglplus {
namespace images {

template <typename T, unsigned CH>
class CellImageGen
 : public Image
{
private:
	static PixelDataFormat _fmt(unsigned c)
	{
		switch(c)
		{
			case 1: return PixelDataFormat::Red;
			case 2: return PixelDataFormat::RG;
			case 3: return PixelDataFormat::RGB;
			case 4: return PixelDataFormat::RGBA;
			default:;
		}
		OGLPLUS_ABORT("Invalid number of channels!");
		return PixelDataFormat();
	}

	template <typename X>
	static PixelDataInternalFormat _ifmt(TypeTag<X>, unsigned c)
	{
		switch(c)
		{
			case 1: return PixelDataInternalFormat::Red;
			case 2: return PixelDataInternalFormat::RG;
			case 3: return PixelDataInternalFormat::RGB;
			case 4: return PixelDataInternalFormat::RGBA;
			default:;
		}
		OGLPLUS_ABORT("Invalid number of channels!");
		return PixelDataInternalFormat();
	}
public:
	struct EulerDistance
	{
		GLdouble operator()(
			std::size_t dims,
			const Vec3d& tc,
			const Vec3d& cc,
			const Vec3d& co,
			const Vec3d& is
		) const
		{
			GLdouble result = 0.0;
			for(std::size_t d=0; d!=dims; ++d)
			{
				GLdouble dist = (tc[d]-(cc[d]+co[d]/is[d]))*is[d];
				result += dist*dist;
			}
			return std::sqrt(result);
		}
	};

	template <typename GetDistance, typename GetValue>
	CellImageGen(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input,
		GetDistance get_distance,
		GetValue get_value
	): Image(
		input.Width() *cell_w,
		input.Height()*cell_h,
		input.Depth() *cell_d,
		CH, &TypeTag<T>(),
		_fmt(CH), _ifmt(TypeTag<T>(), CH)
	)
	{
		const T one = this->_one(TypeTag<T>());

		const GLdouble i_w = 1.0/GLsizei(Width());
		const GLdouble i_h = 1.0/GLsizei(Height());
		const GLdouble i_d = 1.0/GLsizei(Depth());

		const GLsizei iw = input.Width();
		const GLsizei ih = input.Height();
		const GLsizei id = input.Depth();

		std::size_t dims = 1;
		if(ih*cell_h > 1) dims = 2;
		if(id*cell_d > 1) dims = 3;

		const GLsizei kmin = (dims == 3)?-1:0;
		const GLsizei kmax = (dims == 3)?+2:1;
		const GLsizei jmin = (dims >= 2)?-1:0;
		const GLsizei jmax = (dims >= 2)?+2:1;
		const GLsizei imin = -1;
		const GLsizei imax = +2;

		Vec3d colors[27];
		GLdouble dists[27];

		const Vec3d is(iw, ih, id);

		auto pos = this->_begin<T>();

		for(GLsizei z=0; z<Depth(); ++z)
		{
			GLsizei cz = z/cell_d;

			for(GLsizei y=0; y<Height(); ++y)
			{
				GLsizei cy = y/cell_h;

				for(GLsizei x=0; x<Width(); ++x)
				{
					GLsizei cx = x/cell_w;

					Vec3d tc = Vec3d(x*i_w, y*i_h, z*i_d);

					GLsizei l=0;

					for(GLsizei k=kmin; k<kmax; ++k)
					for(GLsizei j=jmin; j<jmax; ++j)
					for(GLsizei i=imin; i<imax; ++i)
					{
						GLsizei ccz = cz+k;
						GLsizei ccy = cy+j;
						GLsizei ccx = cx+i;

						Vec3d cc(
							ccx*cell_w*i_w,
							ccy*cell_h*i_h,
							ccz*cell_d*i_d
						);

						ccz = (ccz+id)%id;
						ccy = (ccy+ih)%ih;
						ccx = (ccx+iw)%iw;

						colors[l] = input.Pixel(ccx, ccy, ccz).xyz();
						Vec3d co = colors[l];

						dists[l] = get_distance(dims, tc, cc, co, is);

						++l;
					}

					Vector<GLdouble, CH> value = get_value(dists, colors, l);

					for(std::size_t c=0; c!=CH; ++c)
					{
						assert(pos != this->_end<T>());
						GLdouble vc = value.At(c);
						*pos++ = T(one*vc);
					}
				}
			}
		}
		assert(pos == this->_end<T>());
	}
};

class WorleyCellGen
 : public CellImageGen<GLubyte, 1>
{
private:
	typedef CellImageGen<GLubyte, 1> Base;

	template <typename ValueCalc>
	struct DistanceValue
	{
		ValueCalc _calc_value;
		const unsigned _order;
		std::vector<GLdouble> _d;

		DistanceValue(ValueCalc calc_value, unsigned order)
		 : _calc_value(calc_value)
		 , _order(order)
		 , _d(order)
		{ }

		Vec1d operator()(
			const GLdouble* dists,
			const Vec3d*,
			SizeType count
		)
		{
			for(unsigned o=0; o!=_order; ++o)
				_d[o] = 2;

			for(GLsizei c=0; c<count; ++c)
			{
				for(unsigned o=0; o!=_order; ++o)
				{
					if(_d[o] > dists[c])
					{
						if(o+1 != _order)
						{
							_d[o+1] = _d[o];
						}
						_d[o] = dists[c];
						break;
					}
				}
			}
			GLfloat result = GLfloat(_calc_value(_d));
			if(result > 1) result = 1;
			if(result < 0) result = 0;
			return Vec1d(result);
		}
	};
public:
	template <typename ValueCalc>
	WorleyCellGen(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input,
		ValueCalc calc_value,
		unsigned order
	): CellImageGen<GLubyte, 1>(
		cell_w, cell_h, cell_d,
		input,
		Base::EulerDistance(),
		DistanceValue<ValueCalc>(calc_value, order)
	){ }
};

} // images
} // oglplus

#endif // include guard
