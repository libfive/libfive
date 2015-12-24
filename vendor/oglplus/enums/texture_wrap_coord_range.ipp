//  File implement/oglplus/enums/texture_wrap_coord_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_wrap_coord.txt'
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
	TextureWrapCoord
> ValueRange_(TextureWrapCoord*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_TEXTUREWRAPCOORD)
#define OGLPLUS_IMPL_EVR_TEXTUREWRAPCOORD
{
static const GLenum _values[] = {
#if defined GL_TEXTURE_WRAP_S
GL_TEXTURE_WRAP_S,
#endif
#if defined GL_TEXTURE_WRAP_T
GL_TEXTURE_WRAP_T,
#endif
#if defined GL_TEXTURE_WRAP_R
GL_TEXTURE_WRAP_R,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	TextureWrapCoord
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

