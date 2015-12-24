//  File implement/oglplus/enums/framebuffer_attachment_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_attachment.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	FramebufferAttachment*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_FRAMEBUFFERATTACHMENT)
#define OGLPLUS_IMPL_EVN_FRAMEBUFFERATTACHMENT
{
switch(value)
{
#if defined GL_COLOR_ATTACHMENT0
	case GL_COLOR_ATTACHMENT0: return StrCRef("COLOR_ATTACHMENT0");
#endif
#if defined GL_COLOR_ATTACHMENT1
	case GL_COLOR_ATTACHMENT1: return StrCRef("COLOR_ATTACHMENT1");
#endif
#if defined GL_COLOR_ATTACHMENT2
	case GL_COLOR_ATTACHMENT2: return StrCRef("COLOR_ATTACHMENT2");
#endif
#if defined GL_COLOR_ATTACHMENT3
	case GL_COLOR_ATTACHMENT3: return StrCRef("COLOR_ATTACHMENT3");
#endif
#if defined GL_COLOR_ATTACHMENT4
	case GL_COLOR_ATTACHMENT4: return StrCRef("COLOR_ATTACHMENT4");
#endif
#if defined GL_COLOR_ATTACHMENT5
	case GL_COLOR_ATTACHMENT5: return StrCRef("COLOR_ATTACHMENT5");
#endif
#if defined GL_COLOR_ATTACHMENT6
	case GL_COLOR_ATTACHMENT6: return StrCRef("COLOR_ATTACHMENT6");
#endif
#if defined GL_COLOR_ATTACHMENT7
	case GL_COLOR_ATTACHMENT7: return StrCRef("COLOR_ATTACHMENT7");
#endif
#if defined GL_COLOR_ATTACHMENT8
	case GL_COLOR_ATTACHMENT8: return StrCRef("COLOR_ATTACHMENT8");
#endif
#if defined GL_COLOR_ATTACHMENT9
	case GL_COLOR_ATTACHMENT9: return StrCRef("COLOR_ATTACHMENT9");
#endif
#if defined GL_COLOR_ATTACHMENT10
	case GL_COLOR_ATTACHMENT10: return StrCRef("COLOR_ATTACHMENT10");
#endif
#if defined GL_COLOR_ATTACHMENT11
	case GL_COLOR_ATTACHMENT11: return StrCRef("COLOR_ATTACHMENT11");
#endif
#if defined GL_COLOR_ATTACHMENT12
	case GL_COLOR_ATTACHMENT12: return StrCRef("COLOR_ATTACHMENT12");
#endif
#if defined GL_COLOR_ATTACHMENT13
	case GL_COLOR_ATTACHMENT13: return StrCRef("COLOR_ATTACHMENT13");
#endif
#if defined GL_COLOR_ATTACHMENT14
	case GL_COLOR_ATTACHMENT14: return StrCRef("COLOR_ATTACHMENT14");
#endif
#if defined GL_COLOR_ATTACHMENT15
	case GL_COLOR_ATTACHMENT15: return StrCRef("COLOR_ATTACHMENT15");
#endif
#if defined GL_DEPTH_ATTACHMENT
	case GL_DEPTH_ATTACHMENT: return StrCRef("DEPTH_ATTACHMENT");
#endif
#if defined GL_STENCIL_ATTACHMENT
	case GL_STENCIL_ATTACHMENT: return StrCRef("STENCIL_ATTACHMENT");
#endif
#if defined GL_DEPTH_STENCIL_ATTACHMENT
	case GL_DEPTH_STENCIL_ATTACHMENT: return StrCRef("DEPTH_STENCIL_ATTACHMENT");
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

