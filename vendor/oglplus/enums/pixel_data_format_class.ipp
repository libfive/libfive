//  File implement/oglplus/enums/pixel_data_format_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PixelDataFormat> class Transform>
class EnumToClass<Base, PixelDataFormat, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEPTH_COMPONENT
# if defined DepthComponent
#  pragma push_macro("DepthComponent")
#  undef DepthComponent
	Transform<PixelDataFormat::DepthComponent> DepthComponent;
#  pragma pop_macro("DepthComponent")
# else
	Transform<PixelDataFormat::DepthComponent> DepthComponent;
# endif
#endif
#if defined GL_DEPTH_STENCIL
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
	Transform<PixelDataFormat::DepthStencil> DepthStencil;
#  pragma pop_macro("DepthStencil")
# else
	Transform<PixelDataFormat::DepthStencil> DepthStencil;
# endif
#endif
#if defined GL_STENCIL_INDEX
# if defined StencilIndex
#  pragma push_macro("StencilIndex")
#  undef StencilIndex
	Transform<PixelDataFormat::StencilIndex> StencilIndex;
#  pragma pop_macro("StencilIndex")
# else
	Transform<PixelDataFormat::StencilIndex> StencilIndex;
# endif
#endif
#if defined GL_RED
# if defined Red
#  pragma push_macro("Red")
#  undef Red
	Transform<PixelDataFormat::Red> Red;
#  pragma pop_macro("Red")
# else
	Transform<PixelDataFormat::Red> Red;
# endif
#endif
#if defined GL_GREEN
# if defined Green
#  pragma push_macro("Green")
#  undef Green
	Transform<PixelDataFormat::Green> Green;
#  pragma pop_macro("Green")
# else
	Transform<PixelDataFormat::Green> Green;
# endif
#endif
#if defined GL_BLUE
# if defined Blue
#  pragma push_macro("Blue")
#  undef Blue
	Transform<PixelDataFormat::Blue> Blue;
#  pragma pop_macro("Blue")
# else
	Transform<PixelDataFormat::Blue> Blue;
# endif
#endif
#if defined GL_RG
# if defined RG
#  pragma push_macro("RG")
#  undef RG
	Transform<PixelDataFormat::RG> RG;
#  pragma pop_macro("RG")
# else
	Transform<PixelDataFormat::RG> RG;
# endif
#endif
#if defined GL_RGB
# if defined RGB
#  pragma push_macro("RGB")
#  undef RGB
	Transform<PixelDataFormat::RGB> RGB;
#  pragma pop_macro("RGB")
# else
	Transform<PixelDataFormat::RGB> RGB;
# endif
#endif
#if defined GL_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	Transform<PixelDataFormat::RGBA> RGBA;
#  pragma pop_macro("RGBA")
# else
	Transform<PixelDataFormat::RGBA> RGBA;
# endif
#endif
#if defined GL_BGR
# if defined BGR
#  pragma push_macro("BGR")
#  undef BGR
	Transform<PixelDataFormat::BGR> BGR;
#  pragma pop_macro("BGR")
# else
	Transform<PixelDataFormat::BGR> BGR;
# endif
#endif
#if defined GL_BGRA
# if defined BGRA
#  pragma push_macro("BGRA")
#  undef BGRA
	Transform<PixelDataFormat::BGRA> BGRA;
#  pragma pop_macro("BGRA")
# else
	Transform<PixelDataFormat::BGRA> BGRA;
# endif
#endif
#if defined GL_RED_INTEGER
# if defined RedInteger
#  pragma push_macro("RedInteger")
#  undef RedInteger
	Transform<PixelDataFormat::RedInteger> RedInteger;
#  pragma pop_macro("RedInteger")
# else
	Transform<PixelDataFormat::RedInteger> RedInteger;
# endif
#endif
#if defined GL_GREEN_INTEGER
# if defined GreenInteger
#  pragma push_macro("GreenInteger")
#  undef GreenInteger
	Transform<PixelDataFormat::GreenInteger> GreenInteger;
#  pragma pop_macro("GreenInteger")
# else
	Transform<PixelDataFormat::GreenInteger> GreenInteger;
# endif
#endif
#if defined GL_BLUE_INTEGER
# if defined BlueInteger
#  pragma push_macro("BlueInteger")
#  undef BlueInteger
	Transform<PixelDataFormat::BlueInteger> BlueInteger;
#  pragma pop_macro("BlueInteger")
# else
	Transform<PixelDataFormat::BlueInteger> BlueInteger;
# endif
#endif
#if defined GL_RG_INTEGER
# if defined RGInteger
#  pragma push_macro("RGInteger")
#  undef RGInteger
	Transform<PixelDataFormat::RGInteger> RGInteger;
#  pragma pop_macro("RGInteger")
# else
	Transform<PixelDataFormat::RGInteger> RGInteger;
# endif
#endif
#if defined GL_RGB_INTEGER
# if defined RGBInteger
#  pragma push_macro("RGBInteger")
#  undef RGBInteger
	Transform<PixelDataFormat::RGBInteger> RGBInteger;
#  pragma pop_macro("RGBInteger")
# else
	Transform<PixelDataFormat::RGBInteger> RGBInteger;
# endif
#endif
#if defined GL_RGBA_INTEGER
# if defined RGBAInteger
#  pragma push_macro("RGBAInteger")
#  undef RGBAInteger
	Transform<PixelDataFormat::RGBAInteger> RGBAInteger;
#  pragma pop_macro("RGBAInteger")
# else
	Transform<PixelDataFormat::RGBAInteger> RGBAInteger;
# endif
#endif
#if defined GL_BGR_INTEGER
# if defined BGRInteger
#  pragma push_macro("BGRInteger")
#  undef BGRInteger
	Transform<PixelDataFormat::BGRInteger> BGRInteger;
#  pragma pop_macro("BGRInteger")
# else
	Transform<PixelDataFormat::BGRInteger> BGRInteger;
# endif
#endif
#if defined GL_BGRA_INTEGER
# if defined BGRAInteger
#  pragma push_macro("BGRAInteger")
#  undef BGRAInteger
	Transform<PixelDataFormat::BGRAInteger> BGRAInteger;
#  pragma pop_macro("BGRAInteger")
# else
	Transform<PixelDataFormat::BGRAInteger> BGRAInteger;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DEPTH_COMPONENT
# if defined DepthComponent
#  pragma push_macro("DepthComponent")
#  undef DepthComponent
	 , DepthComponent(_base())
#  pragma pop_macro("DepthComponent")
# else
	 , DepthComponent(_base())
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
#if defined GL_STENCIL_INDEX
# if defined StencilIndex
#  pragma push_macro("StencilIndex")
#  undef StencilIndex
	 , StencilIndex(_base())
#  pragma pop_macro("StencilIndex")
# else
	 , StencilIndex(_base())
# endif
#endif
#if defined GL_RED
# if defined Red
#  pragma push_macro("Red")
#  undef Red
	 , Red(_base())
#  pragma pop_macro("Red")
# else
	 , Red(_base())
# endif
#endif
#if defined GL_GREEN
# if defined Green
#  pragma push_macro("Green")
#  undef Green
	 , Green(_base())
#  pragma pop_macro("Green")
# else
	 , Green(_base())
# endif
#endif
#if defined GL_BLUE
# if defined Blue
#  pragma push_macro("Blue")
#  undef Blue
	 , Blue(_base())
#  pragma pop_macro("Blue")
# else
	 , Blue(_base())
# endif
#endif
#if defined GL_RG
# if defined RG
#  pragma push_macro("RG")
#  undef RG
	 , RG(_base())
#  pragma pop_macro("RG")
# else
	 , RG(_base())
# endif
#endif
#if defined GL_RGB
# if defined RGB
#  pragma push_macro("RGB")
#  undef RGB
	 , RGB(_base())
#  pragma pop_macro("RGB")
# else
	 , RGB(_base())
# endif
#endif
#if defined GL_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	 , RGBA(_base())
#  pragma pop_macro("RGBA")
# else
	 , RGBA(_base())
# endif
#endif
#if defined GL_BGR
# if defined BGR
#  pragma push_macro("BGR")
#  undef BGR
	 , BGR(_base())
#  pragma pop_macro("BGR")
# else
	 , BGR(_base())
# endif
#endif
#if defined GL_BGRA
# if defined BGRA
#  pragma push_macro("BGRA")
#  undef BGRA
	 , BGRA(_base())
#  pragma pop_macro("BGRA")
# else
	 , BGRA(_base())
# endif
#endif
#if defined GL_RED_INTEGER
# if defined RedInteger
#  pragma push_macro("RedInteger")
#  undef RedInteger
	 , RedInteger(_base())
#  pragma pop_macro("RedInteger")
# else
	 , RedInteger(_base())
# endif
#endif
#if defined GL_GREEN_INTEGER
# if defined GreenInteger
#  pragma push_macro("GreenInteger")
#  undef GreenInteger
	 , GreenInteger(_base())
#  pragma pop_macro("GreenInteger")
# else
	 , GreenInteger(_base())
# endif
#endif
#if defined GL_BLUE_INTEGER
# if defined BlueInteger
#  pragma push_macro("BlueInteger")
#  undef BlueInteger
	 , BlueInteger(_base())
#  pragma pop_macro("BlueInteger")
# else
	 , BlueInteger(_base())
# endif
#endif
#if defined GL_RG_INTEGER
# if defined RGInteger
#  pragma push_macro("RGInteger")
#  undef RGInteger
	 , RGInteger(_base())
#  pragma pop_macro("RGInteger")
# else
	 , RGInteger(_base())
# endif
#endif
#if defined GL_RGB_INTEGER
# if defined RGBInteger
#  pragma push_macro("RGBInteger")
#  undef RGBInteger
	 , RGBInteger(_base())
#  pragma pop_macro("RGBInteger")
# else
	 , RGBInteger(_base())
# endif
#endif
#if defined GL_RGBA_INTEGER
# if defined RGBAInteger
#  pragma push_macro("RGBAInteger")
#  undef RGBAInteger
	 , RGBAInteger(_base())
#  pragma pop_macro("RGBAInteger")
# else
	 , RGBAInteger(_base())
# endif
#endif
#if defined GL_BGR_INTEGER
# if defined BGRInteger
#  pragma push_macro("BGRInteger")
#  undef BGRInteger
	 , BGRInteger(_base())
#  pragma pop_macro("BGRInteger")
# else
	 , BGRInteger(_base())
# endif
#endif
#if defined GL_BGRA_INTEGER
# if defined BGRAInteger
#  pragma push_macro("BGRAInteger")
#  undef BGRAInteger
	 , BGRAInteger(_base())
#  pragma pop_macro("BGRAInteger")
# else
	 , BGRAInteger(_base())
# endif
#endif
	{ }
};

} // namespace enums

