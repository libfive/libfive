//  File implement/oglplus/enums/access_specifier_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/access_specifier.txt'
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
	AccessSpecifier
> ValueRange_(AccessSpecifier*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_ACCESSSPECIFIER)
#define OGLPLUS_IMPL_EVR_ACCESSSPECIFIER
{
static const GLenum _values[] = {
#if defined GL_READ_ONLY
GL_READ_ONLY,
#endif
#if defined GL_WRITE_ONLY
GL_WRITE_ONLY,
#endif
#if defined GL_READ_WRITE
GL_READ_WRITE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	AccessSpecifier
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

