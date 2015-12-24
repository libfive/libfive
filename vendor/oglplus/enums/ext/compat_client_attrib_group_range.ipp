//  File implement/oglplus/enums/ext/compat_client_attrib_group_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_client_attrib_group.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLbitfield*,
	CompatibilityClientAttributeGroup
> ValueRange_(CompatibilityClientAttributeGroup*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_COMPATIBILITYCLIENTATTRIBUTEGROUP)
#define OGLPLUS_IMPL_EVR_COMPATIBILITYCLIENTATTRIBUTEGROUP
{
static const GLbitfield _values[] = {
#if defined GL_CLIENT_VERTEX_ARRAY_BIT
GL_CLIENT_VERTEX_ARRAY_BIT,
#endif
#if defined GL_CLIENT_PIXEL_STORE_BIT
GL_CLIENT_PIXEL_STORE_BIT,
#endif
#if defined GL_CLIENT_ALL_ATTRIB_BITS
GL_CLIENT_ALL_ATTRIB_BITS,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	CompatibilityClientAttributeGroup
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

