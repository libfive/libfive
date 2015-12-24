//  File implement/oglplus/enums/texture_filter_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_filter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	TextureFilter
> ValueRange_(TextureFilter*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_TEXTUREFILTER)
#define OGLPLUS_IMPL_EVR_TEXTUREFILTER
{
static const GLenum _values[] = {
#if defined GL_NEAREST
GL_NEAREST,
#endif
#if defined GL_LINEAR
GL_LINEAR,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	TextureFilter
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

