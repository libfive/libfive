//  File implement/oglplus/enums/ext/amd_perf_monitor_type_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	PerfMonitorAMDType
> ValueRange_(PerfMonitorAMDType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PERFMONITORAMDTYPE)
#define OGLPLUS_IMPL_EVR_PERFMONITORAMDTYPE
{
static const GLenum _values[] = {
#if defined GL_UNSIGNED_INT
GL_UNSIGNED_INT,
#endif
#if defined GL_FLOAT
GL_FLOAT,
#endif
#if defined GL_UNSIGNED_INT64_AMD
GL_UNSIGNED_INT64_AMD,
#endif
#if defined GL_PERCENTAGE_AMD
GL_PERCENTAGE_AMD,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PerfMonitorAMDType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

