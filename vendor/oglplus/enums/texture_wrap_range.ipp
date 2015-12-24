//  File implement/oglplus/enums/texture_wrap_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_wrap.txt'
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
	TextureWrap
> ValueRange_(TextureWrap*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_TEXTUREWRAP)
#define OGLPLUS_IMPL_EVR_TEXTUREWRAP
{
static const GLenum _values[] = {
#if defined GL_CLAMP_TO_EDGE
GL_CLAMP_TO_EDGE,
#endif
#if defined GL_REPEAT
GL_REPEAT,
#endif
#if defined GL_CLAMP_TO_BORDER
GL_CLAMP_TO_BORDER,
#endif
#if defined GL_MIRRORED_REPEAT
GL_MIRRORED_REPEAT,
#endif
#if defined GL_MIRROR_CLAMP_TO_EDGE
GL_MIRROR_CLAMP_TO_EDGE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	TextureWrap
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

