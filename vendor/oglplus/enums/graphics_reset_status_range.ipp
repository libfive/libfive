//  File implement/oglplus/enums/graphics_reset_status_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/graphics_reset_status.txt'
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
	GraphicsResetStatus
> ValueRange_(GraphicsResetStatus*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_GRAPHICSRESETSTATUS)
#define OGLPLUS_IMPL_EVR_GRAPHICSRESETSTATUS
{
static const GLenum _values[] = {
#if defined GL_NO_ERROR
GL_NO_ERROR,
#endif
#if defined GL_GUILTY_CONTEXT_RESET
GL_GUILTY_CONTEXT_RESET,
#endif
#if defined GL_INNOCENT_CONTEXT_RESET
GL_INNOCENT_CONTEXT_RESET,
#endif
#if defined GL_UNKNOWN_CONTEXT_RESET
GL_UNKNOWN_CONTEXT_RESET,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	GraphicsResetStatus
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

