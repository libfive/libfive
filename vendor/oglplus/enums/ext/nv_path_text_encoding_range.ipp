//  File implement/oglplus/enums/ext/nv_path_text_encoding_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_text_encoding.txt'
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
	PathNVTextEncoding
> ValueRange_(PathNVTextEncoding*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVTEXTENCODING)
#define OGLPLUS_IMPL_EVR_PATHNVTEXTENCODING
{
static const GLenum _values[] = {
#if defined GL_UTF8_NV
GL_UTF8_NV,
#endif
#if defined GL_UTF16_NV
GL_UTF16_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVTextEncoding
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

