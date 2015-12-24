//  File implement/oglplus/enums/framebuffer_status_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	FramebufferStatus*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_FRAMEBUFFERSTATUS)
#define OGLPLUS_IMPL_EVN_FRAMEBUFFERSTATUS
{
switch(value)
{
#if defined GL_FRAMEBUFFER_COMPLETE
	case GL_FRAMEBUFFER_COMPLETE: return StrCRef("FRAMEBUFFER_COMPLETE");
#endif
#if defined GL_FRAMEBUFFER_UNDEFINED
	case GL_FRAMEBUFFER_UNDEFINED: return StrCRef("FRAMEBUFFER_UNDEFINED");
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
	case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: return StrCRef("FRAMEBUFFER_INCOMPLETE_ATTACHMENT");
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
	case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: return StrCRef("FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT");
#endif
#if defined GL_FRAMEBUFFER_UNSUPPORTED
	case GL_FRAMEBUFFER_UNSUPPORTED: return StrCRef("FRAMEBUFFER_UNSUPPORTED");
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
	case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: return StrCRef("FRAMEBUFFER_INCOMPLETE_MULTISAMPLE");
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
	case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS: return StrCRef("FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS");
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

