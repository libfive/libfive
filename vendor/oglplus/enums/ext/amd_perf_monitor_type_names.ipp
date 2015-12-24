//  File implement/oglplus/enums/ext/amd_perf_monitor_type_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/amd_perf_monitor_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PerfMonitorAMDType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PERFMONITORAMDTYPE)
#define OGLPLUS_IMPL_EVN_PERFMONITORAMDTYPE
{
switch(value)
{
#if defined GL_UNSIGNED_INT
	case GL_UNSIGNED_INT: return StrCRef("UNSIGNED_INT");
#endif
#if defined GL_FLOAT
	case GL_FLOAT: return StrCRef("FLOAT");
#endif
#if defined GL_UNSIGNED_INT64_AMD
	case GL_UNSIGNED_INT64_AMD: return StrCRef("UNSIGNED_INT64_AMD");
#endif
#if defined GL_PERCENTAGE_AMD
	case GL_PERCENTAGE_AMD: return StrCRef("PERCENTAGE_AMD");
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

