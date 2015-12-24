//  File implement/oglplus/enums/framebuffer_buffer_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_buffer.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<FramebufferBuffer> class Transform>
class EnumToClass<Base, FramebufferBuffer, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_COLOR
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	Transform<FramebufferBuffer::Color> Color;
#  pragma pop_macro("Color")
# else
	Transform<FramebufferBuffer::Color> Color;
# endif
#endif
#if defined GL_DEPTH
# if defined Depth
#  pragma push_macro("Depth")
#  undef Depth
	Transform<FramebufferBuffer::Depth> Depth;
#  pragma pop_macro("Depth")
# else
	Transform<FramebufferBuffer::Depth> Depth;
# endif
#endif
#if defined GL_STENCIL
# if defined Stencil
#  pragma push_macro("Stencil")
#  undef Stencil
	Transform<FramebufferBuffer::Stencil> Stencil;
#  pragma pop_macro("Stencil")
# else
	Transform<FramebufferBuffer::Stencil> Stencil;
# endif
#endif
#if defined GL_DEPTH_STENCIL
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
	Transform<FramebufferBuffer::DepthStencil> DepthStencil;
#  pragma pop_macro("DepthStencil")
# else
	Transform<FramebufferBuffer::DepthStencil> DepthStencil;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_COLOR
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	 , Color(_base())
#  pragma pop_macro("Color")
# else
	 , Color(_base())
# endif
#endif
#if defined GL_DEPTH
# if defined Depth
#  pragma push_macro("Depth")
#  undef Depth
	 , Depth(_base())
#  pragma pop_macro("Depth")
# else
	 , Depth(_base())
# endif
#endif
#if defined GL_STENCIL
# if defined Stencil
#  pragma push_macro("Stencil")
#  undef Stencil
	 , Stencil(_base())
#  pragma pop_macro("Stencil")
# else
	 , Stencil(_base())
# endif
#endif
#if defined GL_DEPTH_STENCIL
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

