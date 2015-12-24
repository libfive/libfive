/**
 *  @file oglplus/images/sort_nw.ipp
 *  @brief Implementation of images::SortNWMap
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <cassert>

namespace oglplus {
namespace images {

inline unsigned SortNWMap::_pot(unsigned n)
{
	return 1 << n;
}

inline unsigned SortNWMap::_next_log(unsigned n)
{
	unsigned pot = 1;
	unsigned result = 0;
	while(n > pot)
	{
		pot <<= 1;
		++result;
	}
	return result;
}

inline unsigned SortNWMap::_num_steps(unsigned size)
{
	assert(size > 0);
	unsigned result = 0;
	for(unsigned i=0, l=_next_log(size); i!=l; ++i)
	{
		result += i+1;
	}
	return result;
}

OGLPLUS_LIB_FUNC
SortNWMap::SortNWMap(unsigned size)
 : Image(size, _num_steps(size), 1, 1, &TypeTag<T>())
{
	this->_format = PixelDataFormat::RedInteger;
	this->_internal = PixelDataInternalFormat::R16UI;
	this->_bzero();
	unsigned y = 0;
	for(unsigned m = _next_log(size), l=0; l<m; ++l)
	{
		for(unsigned b = l+1; b>0; --b)
		{
			unsigned p = _pot(b);
			unsigned q = _pot(b-1);
			unsigned r = _pot(l+1-b);
			unsigned x;
			T enc;

			for(unsigned c=0, d=size/p; c<d; ++c)
			{
				bool direction = (c/r) % 2 == 0;
				for(unsigned e=0; e<q; ++e)
				{
					x = c*p+e;
					enc  = T(q << 2); // step
					enc |= 0x0; // step sign (positive)
					enc |= direction?0x0:0x1;
					this->_at<T>(x, y) = enc;

					x = x+q;
					enc  = T(q << 2); // step
					enc |= 0x2; // step sign (negative)
					enc |= direction?0x0:0x1;
					this->_at<T>(x, y) = enc;
				}
			}
			unsigned f = (size/p);
			for(unsigned c=0, d=size%q; c<d; ++c)
			{
				if(f*p+c+q < size)
				{
					bool direction = (f/r) % 2 == 0;
					x = f*p+c;
					enc  = T(q << 2); // step
					enc |= 0x0; // step sign (positive)
					enc |= direction?0x0:0x1;
					this->_at<T>(x, y) = enc;

					x = x+q;
					enc  = T(q << 2); // step
					enc |= 0x2; // step sign (negative)
					enc |= direction?0x0:0x1;
					this->_at<T>(x, y) = enc;
				}
			}
			++y;
		}
	}
}

} // images
} // oglplus

