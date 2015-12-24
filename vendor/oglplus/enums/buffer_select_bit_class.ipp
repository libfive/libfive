//  File implement/oglplus/enums/buffer_select_bit_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_select_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BufferSelectBit> class Transform>
class EnumToClass<Base, BufferSelectBit, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_COLOR_BUFFER_BIT
# if defined ColorBuffer
#  pragma push_macro("ColorBuffer")
#  undef ColorBuffer
	Transform<BufferSelectBit::ColorBuffer> ColorBuffer;
#  pragma pop_macro("ColorBuffer")
# else
	Transform<BufferSelectBit::ColorBuffer> ColorBuffer;
# endif
#endif
#if defined GL_DEPTH_BUFFER_BIT
# if defined DepthBuffer
#  pragma push_macro("DepthBuffer")
#  undef DepthBuffer
	Transform<BufferSelectBit::DepthBuffer> DepthBuffer;
#  pragma pop_macro("DepthBuffer")
# else
	Transform<BufferSelectBit::DepthBuffer> DepthBuffer;
# endif
#endif
#if defined GL_STENCIL_BUFFER_BIT
# if defined StencilBuffer
#  pragma push_macro("StencilBuffer")
#  undef StencilBuffer
	Transform<BufferSelectBit::StencilBuffer> StencilBuffer;
#  pragma pop_macro("StencilBuffer")
# else
	Transform<BufferSelectBit::StencilBuffer> StencilBuffer;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_COLOR_BUFFER_BIT
# if defined ColorBuffer
#  pragma push_macro("ColorBuffer")
#  undef ColorBuffer
	 , ColorBuffer(_base())
#  pragma pop_macro("ColorBuffer")
# else
	 , ColorBuffer(_base())
# endif
#endif
#if defined GL_DEPTH_BUFFER_BIT
# if defined DepthBuffer
#  pragma push_macro("DepthBuffer")
#  undef DepthBuffer
	 , DepthBuffer(_base())
#  pragma pop_macro("DepthBuffer")
# else
	 , DepthBuffer(_base())
# endif
#endif
#if defined GL_STENCIL_BUFFER_BIT
# if defined StencilBuffer
#  pragma push_macro("StencilBuffer")
#  undef StencilBuffer
	 , StencilBuffer(_base())
#  pragma pop_macro("StencilBuffer")
# else
	 , StencilBuffer(_base())
# endif
#endif
	{ }
};

} // namespace enums

