/**
 *  @file oglplus/images/metaballs.ipp
 *  @brief Implementation of images::Metaballs and MetaStarts
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/math/angle.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/lib/incl_end.ipp>

#include <cassert>
#include <cstdlib>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
BaseMetaballs::BaseMetaballs(
	SizeType width,
	SizeType height,
	const GLfloat* balls,
	std::size_t size,
	std::size_t n
): Image(
	width,
	height,
	1,
	1,
	&TypeTag<GLfloat>(),
	PixelDataFormat::Red,
	PixelDataInternalFormat::R32F
)
{
	assert(size % n == 0);

	const auto fc = FullCircle();
	auto a = this->_begin<GLfloat>();

	for(GLsizei y=0; y!=height; ++y)
	{
		const GLfloat j = (y+0.5f)/GLfloat(height);

		for(GLsizei x=0; x!=width; ++x)
		{
			GLfloat v = 0.0f;
			const GLfloat i = (x+0.5f)/GLfloat(width);
			const Vec2f p(i, j);

			for(std::size_t b=0; b!=size; b+=n)
			{
				const Vec2f c(balls+b, 2);

				for(int yo=-1; yo!=2; ++yo)
				for(int xo=-1; xo!=2; ++xo)
				{
					const Vec2f o(xo, yo);
					const Vec2f d = p - c + o;

					GLfloat r = balls[b+2];

					if(n > 3)
					{
						GLfloat w = ArcTan(d.y(), d.x())/fc;
						w += balls[b+2];
						w = Sin(fc*w*balls[b+3]);

						if(n > 4) r += balls[b+4]*r*w;
						else r += 0.25f*r*w;
					}

					float t = (r*r/Dot(d,d))-0.25f;
					v += (t>0.0f)?t:0.0f;
				}
			}
			assert(a != this->_end<GLfloat>());
			*a++ = v;
		}
	}
	assert(a == this->_end<GLfloat>());
}

std::vector<GLfloat> RandomMetaballs::_make_balls(
	std::size_t count,
	GLfloat rad_min,
	GLfloat rad_max
)
{
	std::vector<GLfloat> result(count*3);

	const GLfloat irm = 1.0f/RAND_MAX;
	const GLfloat rdirm = irm*(rad_max-rad_min);

	for(std::size_t i=0; i!=count; ++i)
	{
		result[3*i+0] = std::rand()*irm;
		result[3*i+1] = std::rand()*irm;
		result[3*i+2] = rad_min+std::rand()*rdirm;
	}

	return std::move(result);
}

OGLPLUS_LIB_FUNC
RandomMetaballs::RandomMetaballs(
	SizeType width,
	SizeType height,
	std::size_t count,
	GLfloat rad_min,
	GLfloat rad_max
): BaseMetaballs(
	width,
	height,
	_make_balls(count, rad_min, rad_max).data(),
	3*count,
	3
)
{ }


std::vector<GLfloat> RandomMetastars::_make_stars(
	std::size_t count,
	GLfloat rad_min,
	GLfloat rad_max,
	GLfloat dif_min,
	GLfloat dif_max,
	GLuint ptc_min,
	GLuint ptc_max
)
{
	std::vector<GLfloat> result(count*5);

	const GLfloat irm = 1.0f/RAND_MAX;
	const GLfloat rdirm = irm*(rad_max-rad_min);
	const GLfloat pdirm = irm*(ptc_max-ptc_min+1);
	const GLfloat ddirm = irm*(dif_max-dif_min);

	for(std::size_t i=0; i!=count; ++i)
	{
		result[5*i+0] = std::rand()*irm;
		result[5*i+1] = std::rand()*irm;
		result[5*i+2] = rad_min+std::rand()*rdirm;
		result[5*i+3] = ptc_min+std::rand()*pdirm;
		result[5*i+4] = dif_min+std::rand()*ddirm;
	}

	return std::move(result);
}

OGLPLUS_LIB_FUNC
RandomMetastars::RandomMetastars(
	SizeType width,
	SizeType height,
	std::size_t count,
	GLfloat rad_min,
	GLfloat rad_max,
	GLfloat dif_min,
	GLfloat dif_max,
	GLuint ptc_min,
	GLuint ptc_max
): BaseMetaballs(
	width,
	height,
	_make_stars(
		count,
		rad_min,
		rad_max,
		dif_min,
		dif_max,
		ptc_min,
		ptc_max
	).data(),
	5*count,
	5
)
{ }

} // namespace images
} // namespace oglplus

