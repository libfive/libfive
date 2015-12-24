//  File implement/oglplus/enums/framebuffer_attachment_class.ipp
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
template <typename Base, template<FramebufferAttachment> class Transform>
class EnumToClass<Base, FramebufferAttachment, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_COLOR_ATTACHMENT0
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	Transform<FramebufferAttachment::Color> Color;
#  pragma pop_macro("Color")
# else
	Transform<FramebufferAttachment::Color> Color;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT1
# if defined Color1
#  pragma push_macro("Color1")
#  undef Color1
	Transform<FramebufferAttachment::Color1> Color1;
#  pragma pop_macro("Color1")
# else
	Transform<FramebufferAttachment::Color1> Color1;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT2
# if defined Color2
#  pragma push_macro("Color2")
#  undef Color2
	Transform<FramebufferAttachment::Color2> Color2;
#  pragma pop_macro("Color2")
# else
	Transform<FramebufferAttachment::Color2> Color2;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT3
# if defined Color3
#  pragma push_macro("Color3")
#  undef Color3
	Transform<FramebufferAttachment::Color3> Color3;
#  pragma pop_macro("Color3")
# else
	Transform<FramebufferAttachment::Color3> Color3;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT4
# if defined Color4
#  pragma push_macro("Color4")
#  undef Color4
	Transform<FramebufferAttachment::Color4> Color4;
#  pragma pop_macro("Color4")
# else
	Transform<FramebufferAttachment::Color4> Color4;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT5
# if defined Color5
#  pragma push_macro("Color5")
#  undef Color5
	Transform<FramebufferAttachment::Color5> Color5;
#  pragma pop_macro("Color5")
# else
	Transform<FramebufferAttachment::Color5> Color5;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT6
# if defined Color6
#  pragma push_macro("Color6")
#  undef Color6
	Transform<FramebufferAttachment::Color6> Color6;
#  pragma pop_macro("Color6")
# else
	Transform<FramebufferAttachment::Color6> Color6;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT7
# if defined Color7
#  pragma push_macro("Color7")
#  undef Color7
	Transform<FramebufferAttachment::Color7> Color7;
#  pragma pop_macro("Color7")
# else
	Transform<FramebufferAttachment::Color7> Color7;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT8
# if defined Color8
#  pragma push_macro("Color8")
#  undef Color8
	Transform<FramebufferAttachment::Color8> Color8;
#  pragma pop_macro("Color8")
# else
	Transform<FramebufferAttachment::Color8> Color8;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT9
# if defined Color9
#  pragma push_macro("Color9")
#  undef Color9
	Transform<FramebufferAttachment::Color9> Color9;
#  pragma pop_macro("Color9")
# else
	Transform<FramebufferAttachment::Color9> Color9;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT10
# if defined Color10
#  pragma push_macro("Color10")
#  undef Color10
	Transform<FramebufferAttachment::Color10> Color10;
#  pragma pop_macro("Color10")
# else
	Transform<FramebufferAttachment::Color10> Color10;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT11
# if defined Color11
#  pragma push_macro("Color11")
#  undef Color11
	Transform<FramebufferAttachment::Color11> Color11;
#  pragma pop_macro("Color11")
# else
	Transform<FramebufferAttachment::Color11> Color11;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT12
# if defined Color12
#  pragma push_macro("Color12")
#  undef Color12
	Transform<FramebufferAttachment::Color12> Color12;
#  pragma pop_macro("Color12")
# else
	Transform<FramebufferAttachment::Color12> Color12;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT13
# if defined Color13
#  pragma push_macro("Color13")
#  undef Color13
	Transform<FramebufferAttachment::Color13> Color13;
#  pragma pop_macro("Color13")
# else
	Transform<FramebufferAttachment::Color13> Color13;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT14
# if defined Color14
#  pragma push_macro("Color14")
#  undef Color14
	Transform<FramebufferAttachment::Color14> Color14;
#  pragma pop_macro("Color14")
# else
	Transform<FramebufferAttachment::Color14> Color14;
# endif
#endif
#if defined GL_COLOR_ATTACHMENT15
# if defined Color15
#  pragma push_macro("Color15")
#  undef Color15
	Transform<FramebufferAttachment::Color15> Color15;
#  pragma pop_macro("Color15")
# else
	Transform<FramebufferAttachment::Color15> Color15;
# endif
#endif
#if defined GL_DEPTH_ATTACHMENT
# if defined Depth
#  pragma push_macro("Depth")
#  undef Depth
	Transform<FramebufferAttachment::Depth> Depth;
#  pragma pop_macro("Depth")
# else
	Transform<FramebufferAttachment::Depth> Depth;
# endif
#endif
#if defined GL_STENCIL_ATTACHMENT
# if defined Stencil
#  pragma push_macro("Stencil")
#  undef Stencil
	Transform<FramebufferAttachment::Stencil> Stencil;
#  pragma pop_macro("Stencil")
# else
	Transform<FramebufferAttachment::Stencil> Stencil;
# endif
#endif
#if defined GL_DEPTH_STENCIL_ATTACHMENT
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
	Transform<FramebufferAttachment::DepthStencil> DepthStencil;
#  pragma pop_macro("DepthStencil")
# else
	Transform<FramebufferAttachment::DepthStencil> DepthStencil;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_COLOR_ATTACHMENT0
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	 , Color(_base())
#  pragma pop_macro("Color")
# else
	 , Color(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT1
# if defined Color1
#  pragma push_macro("Color1")
#  undef Color1
	 , Color1(_base())
#  pragma pop_macro("Color1")
# else
	 , Color1(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT2
# if defined Color2
#  pragma push_macro("Color2")
#  undef Color2
	 , Color2(_base())
#  pragma pop_macro("Color2")
# else
	 , Color2(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT3
# if defined Color3
#  pragma push_macro("Color3")
#  undef Color3
	 , Color3(_base())
#  pragma pop_macro("Color3")
# else
	 , Color3(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT4
# if defined Color4
#  pragma push_macro("Color4")
#  undef Color4
	 , Color4(_base())
#  pragma pop_macro("Color4")
# else
	 , Color4(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT5
# if defined Color5
#  pragma push_macro("Color5")
#  undef Color5
	 , Color5(_base())
#  pragma pop_macro("Color5")
# else
	 , Color5(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT6
# if defined Color6
#  pragma push_macro("Color6")
#  undef Color6
	 , Color6(_base())
#  pragma pop_macro("Color6")
# else
	 , Color6(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT7
# if defined Color7
#  pragma push_macro("Color7")
#  undef Color7
	 , Color7(_base())
#  pragma pop_macro("Color7")
# else
	 , Color7(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT8
# if defined Color8
#  pragma push_macro("Color8")
#  undef Color8
	 , Color8(_base())
#  pragma pop_macro("Color8")
# else
	 , Color8(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT9
# if defined Color9
#  pragma push_macro("Color9")
#  undef Color9
	 , Color9(_base())
#  pragma pop_macro("Color9")
# else
	 , Color9(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT10
# if defined Color10
#  pragma push_macro("Color10")
#  undef Color10
	 , Color10(_base())
#  pragma pop_macro("Color10")
# else
	 , Color10(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT11
# if defined Color11
#  pragma push_macro("Color11")
#  undef Color11
	 , Color11(_base())
#  pragma pop_macro("Color11")
# else
	 , Color11(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT12
# if defined Color12
#  pragma push_macro("Color12")
#  undef Color12
	 , Color12(_base())
#  pragma pop_macro("Color12")
# else
	 , Color12(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT13
# if defined Color13
#  pragma push_macro("Color13")
#  undef Color13
	 , Color13(_base())
#  pragma pop_macro("Color13")
# else
	 , Color13(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT14
# if defined Color14
#  pragma push_macro("Color14")
#  undef Color14
	 , Color14(_base())
#  pragma pop_macro("Color14")
# else
	 , Color14(_base())
# endif
#endif
#if defined GL_COLOR_ATTACHMENT15
# if defined Color15
#  pragma push_macro("Color15")
#  undef Color15
	 , Color15(_base())
#  pragma pop_macro("Color15")
# else
	 , Color15(_base())
# endif
#endif
#if defined GL_DEPTH_ATTACHMENT
# if defined Depth
#  pragma push_macro("Depth")
#  undef Depth
	 , Depth(_base())
#  pragma pop_macro("Depth")
# else
	 , Depth(_base())
# endif
#endif
#if defined GL_STENCIL_ATTACHMENT
# if defined Stencil
#  pragma push_macro("Stencil")
#  undef Stencil
	 , Stencil(_base())
#  pragma pop_macro("Stencil")
# else
	 , Stencil(_base())
# endif
#endif
#if defined GL_DEPTH_STENCIL_ATTACHMENT
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
	 , DepthStencil(_base())
#  pragma pop_macro("DepthStencil")
# else
	 , DepthStencil(_base())
# endif
#endif
	{ }
};

} // namespace enums

