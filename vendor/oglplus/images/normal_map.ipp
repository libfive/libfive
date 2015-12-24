/**
 *  @file oglplus/images/normal_map.ipp
 *  @brief Implementation of images::NormalMap
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <cassert>
#include <cstdlib>

namespace oglplus {
namespace images {

struct NormalMap_filter
{
	template <typename Extractor, typename Sampler>
	Vector<GLfloat, 4> operator()(
		const Extractor& extractor,
		const Sampler& sampler,
		GLfloat one
	) const
	{
		typedef GLdouble number;
		number s = 0.05;

		number sc  = extractor(sampler( 0, 0, 0));
		number spx = extractor(sampler(+1, 0, 0));
		number spy = extractor(sampler( 0,+1, 0));
		number snx = extractor(sampler(-1, 0, 0));
		number sny = extractor(sampler( 0,-1, 0));
		Vector<number, 3> vpx(+s, 0, (spx-sc));
		Vector<number, 3> vpy(0, +s, (spy-sc));
		Vector<number, 3> vnx(-s, 0, (snx-sc));
		Vector<number, 3> vny(0, -s, (sny-sc));
		return Vector<number, 4>(
			Normalized(
				Cross(vpx, vpy) +
				Cross(vpy, vnx) +
				Cross(vnx, vny) +
				Cross(vny, vpx)
			),
			sc
		) * one;
	}
};

OGLPLUS_LIB_FUNC
NormalMap::NormalMap(const Image& image)
 : Filtered(
	image,
	NormalMap_filter(),
	Filtered::DefaultSampler(),
	Filtered::FromRed()
)
{
	this->_format = PixelDataFormat::RGBA;
	this->_internal = PixelDataInternalFormat::RGBA16F;
}

OGLPLUS_LIB_FUNC
NormalMap::NormalMap(const Image& image, Filtered::FromRed)
 : Filtered(
	image,
	NormalMap_filter(),
	Filtered::DefaultSampler(),
	Filtered::FromRed()
)
{
	this->_format = PixelDataFormat::RGBA;
	this->_internal = PixelDataInternalFormat::RGBA16F;
}

OGLPLUS_LIB_FUNC
NormalMap::NormalMap(const Image& image, Filtered::FromAlpha)
 : Filtered(
	image,
	NormalMap_filter(),
	Filtered::DefaultSampler(),
	Filtered::FromAlpha()
)
{
	this->_format = PixelDataFormat::RGBA;
	this->_internal = PixelDataInternalFormat::RGBA16F;
}

} // images
} // oglplus

