//  File implement/oglplus/enums/context_profile_bit_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_profile_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ContextProfileBit*,
	GLbitfield value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_CONTEXTPROFILEBIT)
#define OGLPLUS_IMPL_EVN_CONTEXTPROFILEBIT
{
switch(value)
{
#if defined GL_CONTEXT_CORE_PROFILE_BIT
	case GL_CONTEXT_CORE_PROFILE_BIT: return StrCRef("CONTEXT_CORE_PROFILE_BIT");
#endif
#if defined GL_CONTEXT_COMPATIBILITY_PROFILE_BIT
	case GL_CONTEXT_COMPATIBILITY_PROFILE_BIT: return StrCRef("CONTEXT_COMPATIBILITY_PROFILE_BIT");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums

