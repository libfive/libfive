//  File implement/oglplus/enums/pixel_data_internal_format_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_internal_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PixelDataInternalFormat> class Transform>
class EnumToClass<Base, PixelDataInternalFormat, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEPTH_COMPONENT
# if defined DepthComponent
#  pragma push_macro("DepthComponent")
#  undef DepthComponent
	Transform<PixelDataInternalFormat::DepthComponent> DepthComponent;
#  pragma pop_macro("DepthComponent")
# else
	Transform<PixelDataInternalFormat::DepthComponent> DepthComponent;
# endif
#endif
#if defined GL_DEPTH_STENCIL
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
	Transform<PixelDataInternalFormat::DepthStencil> DepthStencil;
#  pragma pop_macro("DepthStencil")
# else
	Transform<PixelDataInternalFormat::DepthStencil> DepthStencil;
# endif
#endif
#if defined GL_STENCIL_INDEX8
# if defined StencilIndex8
#  pragma push_macro("StencilIndex8")
#  undef StencilIndex8
	Transform<PixelDataInternalFormat::StencilIndex8> StencilIndex8;
#  pragma pop_macro("StencilIndex8")
# else
	Transform<PixelDataInternalFormat::StencilIndex8> StencilIndex8;
# endif
#endif
#if defined GL_RED
# if defined Red
#  pragma push_macro("Red")
#  undef Red
	Transform<PixelDataInternalFormat::Red> Red;
#  pragma pop_macro("Red")
# else
	Transform<PixelDataInternalFormat::Red> Red;
# endif
#endif
#if defined GL_RG
# if defined RG
#  pragma push_macro("RG")
#  undef RG
	Transform<PixelDataInternalFormat::RG> RG;
#  pragma pop_macro("RG")
# else
	Transform<PixelDataInternalFormat::RG> RG;
# endif
#endif
#if defined GL_RGB
# if defined RGB
#  pragma push_macro("RGB")
#  undef RGB
	Transform<PixelDataInternalFormat::RGB> RGB;
#  pragma pop_macro("RGB")
# else
	Transform<PixelDataInternalFormat::RGB> RGB;
# endif
#endif
#if defined GL_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	Transform<PixelDataInternalFormat::RGBA> RGBA;
#  pragma pop_macro("RGBA")
# else
	Transform<PixelDataInternalFormat::RGBA> RGBA;
# endif
#endif
#if defined GL_R8
# if defined R8
#  pragma push_macro("R8")
#  undef R8
	Transform<PixelDataInternalFormat::R8> R8;
#  pragma pop_macro("R8")
# else
	Transform<PixelDataInternalFormat::R8> R8;
# endif
#endif
#if defined GL_R8_SNORM
# if defined R8SNorm
#  pragma push_macro("R8SNorm")
#  undef R8SNorm
	Transform<PixelDataInternalFormat::R8SNorm> R8SNorm;
#  pragma pop_macro("R8SNorm")
# else
	Transform<PixelDataInternalFormat::R8SNorm> R8SNorm;
# endif
#endif
#if defined GL_R16
# if defined R16
#  pragma push_macro("R16")
#  undef R16
	Transform<PixelDataInternalFormat::R16> R16;
#  pragma pop_macro("R16")
# else
	Transform<PixelDataInternalFormat::R16> R16;
# endif
#endif
#if defined GL_R16_SNORM
# if defined R16SNorm
#  pragma push_macro("R16SNorm")
#  undef R16SNorm
	Transform<PixelDataInternalFormat::R16SNorm> R16SNorm;
#  pragma pop_macro("R16SNorm")
# else
	Transform<PixelDataInternalFormat::R16SNorm> R16SNorm;
# endif
#endif
#if defined GL_RG8
# if defined RG8
#  pragma push_macro("RG8")
#  undef RG8
	Transform<PixelDataInternalFormat::RG8> RG8;
#  pragma pop_macro("RG8")
# else
	Transform<PixelDataInternalFormat::RG8> RG8;
# endif
#endif
#if defined GL_RG8_SNORM
# if defined RG8SNorm
#  pragma push_macro("RG8SNorm")
#  undef RG8SNorm
	Transform<PixelDataInternalFormat::RG8SNorm> RG8SNorm;
#  pragma pop_macro("RG8SNorm")
# else
	Transform<PixelDataInternalFormat::RG8SNorm> RG8SNorm;
# endif
#endif
#if defined GL_RG16
# if defined RG16
#  pragma push_macro("RG16")
#  undef RG16
	Transform<PixelDataInternalFormat::RG16> RG16;
#  pragma pop_macro("RG16")
# else
	Transform<PixelDataInternalFormat::RG16> RG16;
# endif
#endif
#if defined GL_RG16_SNORM
# if defined RG16SNorm
#  pragma push_macro("RG16SNorm")
#  undef RG16SNorm
	Transform<PixelDataInternalFormat::RG16SNorm> RG16SNorm;
#  pragma pop_macro("RG16SNorm")
# else
	Transform<PixelDataInternalFormat::RG16SNorm> RG16SNorm;
# endif
#endif
#if defined GL_R3_G3_B2
# if defined R3G3B2
#  pragma push_macro("R3G3B2")
#  undef R3G3B2
	Transform<PixelDataInternalFormat::R3G3B2> R3G3B2;
#  pragma pop_macro("R3G3B2")
# else
	Transform<PixelDataInternalFormat::R3G3B2> R3G3B2;
# endif
#endif
#if defined GL_RGB4
# if defined RGB4
#  pragma push_macro("RGB4")
#  undef RGB4
	Transform<PixelDataInternalFormat::RGB4> RGB4;
#  pragma pop_macro("RGB4")
# else
	Transform<PixelDataInternalFormat::RGB4> RGB4;
# endif
#endif
#if defined GL_RGB5
# if defined RGB5
#  pragma push_macro("RGB5")
#  undef RGB5
	Transform<PixelDataInternalFormat::RGB5> RGB5;
#  pragma pop_macro("RGB5")
# else
	Transform<PixelDataInternalFormat::RGB5> RGB5;
# endif
#endif
#if defined GL_RGB8
# if defined RGB8
#  pragma push_macro("RGB8")
#  undef RGB8
	Transform<PixelDataInternalFormat::RGB8> RGB8;
#  pragma pop_macro("RGB8")
# else
	Transform<PixelDataInternalFormat::RGB8> RGB8;
# endif
#endif
#if defined GL_RGB8_SNORM
# if defined RGB8SNorm
#  pragma push_macro("RGB8SNorm")
#  undef RGB8SNorm
	Transform<PixelDataInternalFormat::RGB8SNorm> RGB8SNorm;
#  pragma pop_macro("RGB8SNorm")
# else
	Transform<PixelDataInternalFormat::RGB8SNorm> RGB8SNorm;
# endif
#endif
#if defined GL_RGB10
# if defined RGB10
#  pragma push_macro("RGB10")
#  undef RGB10
	Transform<PixelDataInternalFormat::RGB10> RGB10;
#  pragma pop_macro("RGB10")
# else
	Transform<PixelDataInternalFormat::RGB10> RGB10;
# endif
#endif
#if defined GL_RGB12
# if defined RGB12
#  pragma push_macro("RGB12")
#  undef RGB12
	Transform<PixelDataInternalFormat::RGB12> RGB12;
#  pragma pop_macro("RGB12")
# else
	Transform<PixelDataInternalFormat::RGB12> RGB12;
# endif
#endif
#if defined GL_RGB16
# if defined RGB16
#  pragma push_macro("RGB16")
#  undef RGB16
	Transform<PixelDataInternalFormat::RGB16> RGB16;
#  pragma pop_macro("RGB16")
# else
	Transform<PixelDataInternalFormat::RGB16> RGB16;
# endif
#endif
#if defined GL_RGB16_SNORM
# if defined RGB16SNorm
#  pragma push_macro("RGB16SNorm")
#  undef RGB16SNorm
	Transform<PixelDataInternalFormat::RGB16SNorm> RGB16SNorm;
#  pragma pop_macro("RGB16SNorm")
# else
	Transform<PixelDataInternalFormat::RGB16SNorm> RGB16SNorm;
# endif
#endif
#if defined GL_RGBA2
# if defined RGBA2
#  pragma push_macro("RGBA2")
#  undef RGBA2
	Transform<PixelDataInternalFormat::RGBA2> RGBA2;
#  pragma pop_macro("RGBA2")
# else
	Transform<PixelDataInternalFormat::RGBA2> RGBA2;
# endif
#endif
#if defined GL_RGBA4
# if defined RGBA4
#  pragma push_macro("RGBA4")
#  undef RGBA4
	Transform<PixelDataInternalFormat::RGBA4> RGBA4;
#  pragma pop_macro("RGBA4")
# else
	Transform<PixelDataInternalFormat::RGBA4> RGBA4;
# endif
#endif
#if defined GL_RGB5_A1
# if defined RGB5A1
#  pragma push_macro("RGB5A1")
#  undef RGB5A1
	Transform<PixelDataInternalFormat::RGB5A1> RGB5A1;
#  pragma pop_macro("RGB5A1")
# else
	Transform<PixelDataInternalFormat::RGB5A1> RGB5A1;
# endif
#endif
#if defined GL_RGBA8
# if defined RGBA8
#  pragma push_macro("RGBA8")
#  undef RGBA8
	Transform<PixelDataInternalFormat::RGBA8> RGBA8;
#  pragma pop_macro("RGBA8")
# else
	Transform<PixelDataInternalFormat::RGBA8> RGBA8;
# endif
#endif
#if defined GL_RGBA8_SNORM
# if defined RGBA8SNorm
#  pragma push_macro("RGBA8SNorm")
#  undef RGBA8SNorm
	Transform<PixelDataInternalFormat::RGBA8SNorm> RGBA8SNorm;
#  pragma pop_macro("RGBA8SNorm")
# else
	Transform<PixelDataInternalFormat::RGBA8SNorm> RGBA8SNorm;
# endif
#endif
#if defined GL_RGB10_A2
# if defined RGB10A2
#  pragma push_macro("RGB10A2")
#  undef RGB10A2
	Transform<PixelDataInternalFormat::RGB10A2> RGB10A2;
#  pragma pop_macro("RGB10A2")
# else
	Transform<PixelDataInternalFormat::RGB10A2> RGB10A2;
# endif
#endif
#if defined GL_RGB10_A2UI
# if defined RGB10A2UI
#  pragma push_macro("RGB10A2UI")
#  undef RGB10A2UI
	Transform<PixelDataInternalFormat::RGB10A2UI> RGB10A2UI;
#  pragma pop_macro("RGB10A2UI")
# else
	Transform<PixelDataInternalFormat::RGB10A2UI> RGB10A2UI;
# endif
#endif
#if defined GL_RGBA12
# if defined RGBA12
#  pragma push_macro("RGBA12")
#  undef RGBA12
	Transform<PixelDataInternalFormat::RGBA12> RGBA12;
#  pragma pop_macro("RGBA12")
# else
	Transform<PixelDataInternalFormat::RGBA12> RGBA12;
# endif
#endif
#if defined GL_RGBA16
# if defined RGBA16
#  pragma push_macro("RGBA16")
#  undef RGBA16
	Transform<PixelDataInternalFormat::RGBA16> RGBA16;
#  pragma pop_macro("RGBA16")
# else
	Transform<PixelDataInternalFormat::RGBA16> RGBA16;
# endif
#endif
#if defined GL_RGBA16_SNORM
# if defined RGBA16SNorm
#  pragma push_macro("RGBA16SNorm")
#  undef RGBA16SNorm
	Transform<PixelDataInternalFormat::RGBA16SNorm> RGBA16SNorm;
#  pragma pop_macro("RGBA16SNorm")
# else
	Transform<PixelDataInternalFormat::RGBA16SNorm> RGBA16SNorm;
# endif
#endif
#if defined GL_SRGB8
# if defined SRGB8
#  pragma push_macro("SRGB8")
#  undef SRGB8
	Transform<PixelDataInternalFormat::SRGB8> SRGB8;
#  pragma pop_macro("SRGB8")
# else
	Transform<PixelDataInternalFormat::SRGB8> SRGB8;
# endif
#endif
#if defined GL_SRGB8_ALPHA8
# if defined SRGB8Alpha8
#  pragma push_macro("SRGB8Alpha8")
#  undef SRGB8Alpha8
	Transform<PixelDataInternalFormat::SRGB8Alpha8> SRGB8Alpha8;
#  pragma pop_macro("SRGB8Alpha8")
# else
	Transform<PixelDataInternalFormat::SRGB8Alpha8> SRGB8Alpha8;
# endif
#endif
#if defined GL_R16F
# if defined R16F
#  pragma push_macro("R16F")
#  undef R16F
	Transform<PixelDataInternalFormat::R16F> R16F;
#  pragma pop_macro("R16F")
# else
	Transform<PixelDataInternalFormat::R16F> R16F;
# endif
#endif
#if defined GL_RG16F
# if defined RG16F
#  pragma push_macro("RG16F")
#  undef RG16F
	Transform<PixelDataInternalFormat::RG16F> RG16F;
#  pragma pop_macro("RG16F")
# else
	Transform<PixelDataInternalFormat::RG16F> RG16F;
# endif
#endif
#if defined GL_RGB16F
# if defined RGB16F
#  pragma push_macro("RGB16F")
#  undef RGB16F
	Transform<PixelDataInternalFormat::RGB16F> RGB16F;
#  pragma pop_macro("RGB16F")
# else
	Transform<PixelDataInternalFormat::RGB16F> RGB16F;
# endif
#endif
#if defined GL_RGBA16F
# if defined RGBA16F
#  pragma push_macro("RGBA16F")
#  undef RGBA16F
	Transform<PixelDataInternalFormat::RGBA16F> RGBA16F;
#  pragma pop_macro("RGBA16F")
# else
	Transform<PixelDataInternalFormat::RGBA16F> RGBA16F;
# endif
#endif
#if defined GL_R32F
# if defined R32F
#  pragma push_macro("R32F")
#  undef R32F
	Transform<PixelDataInternalFormat::R32F> R32F;
#  pragma pop_macro("R32F")
# else
	Transform<PixelDataInternalFormat::R32F> R32F;
# endif
#endif
#if defined GL_RG32F
# if defined RG32F
#  pragma push_macro("RG32F")
#  undef RG32F
	Transform<PixelDataInternalFormat::RG32F> RG32F;
#  pragma pop_macro("RG32F")
# else
	Transform<PixelDataInternalFormat::RG32F> RG32F;
# endif
#endif
#if defined GL_RGB32F
# if defined RGB32F
#  pragma push_macro("RGB32F")
#  undef RGB32F
	Transform<PixelDataInternalFormat::RGB32F> RGB32F;
#  pragma pop_macro("RGB32F")
# else
	Transform<PixelDataInternalFormat::RGB32F> RGB32F;
# endif
#endif
#if defined GL_RGBA32F
# if defined RGBA32F
#  pragma push_macro("RGBA32F")
#  undef RGBA32F
	Transform<PixelDataInternalFormat::RGBA32F> RGBA32F;
#  pragma pop_macro("RGBA32F")
# else
	Transform<PixelDataInternalFormat::RGBA32F> RGBA32F;
# endif
#endif
#if defined GL_R11F_G11F_B10F
# if defined R11FG11FB10F
#  pragma push_macro("R11FG11FB10F")
#  undef R11FG11FB10F
	Transform<PixelDataInternalFormat::R11FG11FB10F> R11FG11FB10F;
#  pragma pop_macro("R11FG11FB10F")
# else
	Transform<PixelDataInternalFormat::R11FG11FB10F> R11FG11FB10F;
# endif
#endif
#if defined GL_RGB9_E5
# if defined RGB9E5
#  pragma push_macro("RGB9E5")
#  undef RGB9E5
	Transform<PixelDataInternalFormat::RGB9E5> RGB9E5;
#  pragma pop_macro("RGB9E5")
# else
	Transform<PixelDataInternalFormat::RGB9E5> RGB9E5;
# endif
#endif
#if defined GL_R8I
# if defined R8I
#  pragma push_macro("R8I")
#  undef R8I
	Transform<PixelDataInternalFormat::R8I> R8I;
#  pragma pop_macro("R8I")
# else
	Transform<PixelDataInternalFormat::R8I> R8I;
# endif
#endif
#if defined GL_R8UI
# if defined R8UI
#  pragma push_macro("R8UI")
#  undef R8UI
	Transform<PixelDataInternalFormat::R8UI> R8UI;
#  pragma pop_macro("R8UI")
# else
	Transform<PixelDataInternalFormat::R8UI> R8UI;
# endif
#endif
#if defined GL_R16I
# if defined R16I
#  pragma push_macro("R16I")
#  undef R16I
	Transform<PixelDataInternalFormat::R16I> R16I;
#  pragma pop_macro("R16I")
# else
	Transform<PixelDataInternalFormat::R16I> R16I;
# endif
#endif
#if defined GL_R16UI
# if defined R16UI
#  pragma push_macro("R16UI")
#  undef R16UI
	Transform<PixelDataInternalFormat::R16UI> R16UI;
#  pragma pop_macro("R16UI")
# else
	Transform<PixelDataInternalFormat::R16UI> R16UI;
# endif
#endif
#if defined GL_R32I
# if defined R32I
#  pragma push_macro("R32I")
#  undef R32I
	Transform<PixelDataInternalFormat::R32I> R32I;
#  pragma pop_macro("R32I")
# else
	Transform<PixelDataInternalFormat::R32I> R32I;
# endif
#endif
#if defined GL_R32UI
# if defined R32UI
#  pragma push_macro("R32UI")
#  undef R32UI
	Transform<PixelDataInternalFormat::R32UI> R32UI;
#  pragma pop_macro("R32UI")
# else
	Transform<PixelDataInternalFormat::R32UI> R32UI;
# endif
#endif
#if defined GL_RG8I
# if defined RG8I
#  pragma push_macro("RG8I")
#  undef RG8I
	Transform<PixelDataInternalFormat::RG8I> RG8I;
#  pragma pop_macro("RG8I")
# else
	Transform<PixelDataInternalFormat::RG8I> RG8I;
# endif
#endif
#if defined GL_RG8UI
# if defined RG8UI
#  pragma push_macro("RG8UI")
#  undef RG8UI
	Transform<PixelDataInternalFormat::RG8UI> RG8UI;
#  pragma pop_macro("RG8UI")
# else
	Transform<PixelDataInternalFormat::RG8UI> RG8UI;
# endif
#endif
#if defined GL_RG16I
# if defined RG16I
#  pragma push_macro("RG16I")
#  undef RG16I
	Transform<PixelDataInternalFormat::RG16I> RG16I;
#  pragma pop_macro("RG16I")
# else
	Transform<PixelDataInternalFormat::RG16I> RG16I;
# endif
#endif
#if defined GL_RG16UI
# if defined RG16UI
#  pragma push_macro("RG16UI")
#  undef RG16UI
	Transform<PixelDataInternalFormat::RG16UI> RG16UI;
#  pragma pop_macro("RG16UI")
# else
	Transform<PixelDataInternalFormat::RG16UI> RG16UI;
# endif
#endif
#if defined GL_RG32I
# if defined RG32I
#  pragma push_macro("RG32I")
#  undef RG32I
	Transform<PixelDataInternalFormat::RG32I> RG32I;
#  pragma pop_macro("RG32I")
# else
	Transform<PixelDataInternalFormat::RG32I> RG32I;
# endif
#endif
#if defined GL_RG32UI
# if defined RG32UI
#  pragma push_macro("RG32UI")
#  undef RG32UI
	Transform<PixelDataInternalFormat::RG32UI> RG32UI;
#  pragma pop_macro("RG32UI")
# else
	Transform<PixelDataInternalFormat::RG32UI> RG32UI;
# endif
#endif
#if defined GL_RGB8I
# if defined RGB8I
#  pragma push_macro("RGB8I")
#  undef RGB8I
	Transform<PixelDataInternalFormat::RGB8I> RGB8I;
#  pragma pop_macro("RGB8I")
# else
	Transform<PixelDataInternalFormat::RGB8I> RGB8I;
# endif
#endif
#if defined GL_RGB8UI
# if defined RGB8UI
#  pragma push_macro("RGB8UI")
#  undef RGB8UI
	Transform<PixelDataInternalFormat::RGB8UI> RGB8UI;
#  pragma pop_macro("RGB8UI")
# else
	Transform<PixelDataInternalFormat::RGB8UI> RGB8UI;
# endif
#endif
#if defined GL_RGB16I
# if defined RGB16I
#  pragma push_macro("RGB16I")
#  undef RGB16I
	Transform<PixelDataInternalFormat::RGB16I> RGB16I;
#  pragma pop_macro("RGB16I")
# else
	Transform<PixelDataInternalFormat::RGB16I> RGB16I;
# endif
#endif
#if defined GL_RGB16UI
# if defined RGB16UI
#  pragma push_macro("RGB16UI")
#  undef RGB16UI
	Transform<PixelDataInternalFormat::RGB16UI> RGB16UI;
#  pragma pop_macro("RGB16UI")
# else
	Transform<PixelDataInternalFormat::RGB16UI> RGB16UI;
# endif
#endif
#if defined GL_RGB32I
# if defined RGB32I
#  pragma push_macro("RGB32I")
#  undef RGB32I
	Transform<PixelDataInternalFormat::RGB32I> RGB32I;
#  pragma pop_macro("RGB32I")
# else
	Transform<PixelDataInternalFormat::RGB32I> RGB32I;
# endif
#endif
#if defined GL_RGB32UI
# if defined RGB32UI
#  pragma push_macro("RGB32UI")
#  undef RGB32UI
	Transform<PixelDataInternalFormat::RGB32UI> RGB32UI;
#  pragma pop_macro("RGB32UI")
# else
	Transform<PixelDataInternalFormat::RGB32UI> RGB32UI;
# endif
#endif
#if defined GL_RGBA8I
# if defined RGBA8I
#  pragma push_macro("RGBA8I")
#  undef RGBA8I
	Transform<PixelDataInternalFormat::RGBA8I> RGBA8I;
#  pragma pop_macro("RGBA8I")
# else
	Transform<PixelDataInternalFormat::RGBA8I> RGBA8I;
# endif
#endif
#if defined GL_RGBA8UI
# if defined RGBA8UI
#  pragma push_macro("RGBA8UI")
#  undef RGBA8UI
	Transform<PixelDataInternalFormat::RGBA8UI> RGBA8UI;
#  pragma pop_macro("RGBA8UI")
# else
	Transform<PixelDataInternalFormat::RGBA8UI> RGBA8UI;
# endif
#endif
#if defined GL_RGBA16I
# if defined RGBA16I
#  pragma push_macro("RGBA16I")
#  undef RGBA16I
	Transform<PixelDataInternalFormat::RGBA16I> RGBA16I;
#  pragma pop_macro("RGBA16I")
# else
	Transform<PixelDataInternalFormat::RGBA16I> RGBA16I;
# endif
#endif
#if defined GL_RGBA16UI
# if defined RGBA16UI
#  pragma push_macro("RGBA16UI")
#  undef RGBA16UI
	Transform<PixelDataInternalFormat::RGBA16UI> RGBA16UI;
#  pragma pop_macro("RGBA16UI")
# else
	Transform<PixelDataInternalFormat::RGBA16UI> RGBA16UI;
# endif
#endif
#if defined GL_RGBA32I
# if defined RGBA32I
#  pragma push_macro("RGBA32I")
#  undef RGBA32I
	Transform<PixelDataInternalFormat::RGBA32I> RGBA32I;
#  pragma pop_macro("RGBA32I")
# else
	Transform<PixelDataInternalFormat::RGBA32I> RGBA32I;
# endif
#endif
#if defined GL_RGBA32UI
# if defined RGBA32UI
#  pragma push_macro("RGBA32UI")
#  undef RGBA32UI
	Transform<PixelDataInternalFormat::RGBA32UI> RGBA32UI;
#  pragma pop_macro("RGBA32UI")
# else
	Transform<PixelDataInternalFormat::RGBA32UI> RGBA32UI;
# endif
#endif
#if defined GL_DEPTH_COMPONENT16
# if defined DepthComponent16
#  pragma push_macro("DepthComponent16")
#  undef DepthComponent16
	Transform<PixelDataInternalFormat::DepthComponent16> DepthComponent16;
#  pragma pop_macro("DepthComponent16")
# else
	Transform<PixelDataInternalFormat::DepthComponent16> DepthComponent16;
# endif
#endif
#if defined GL_DEPTH_COMPONENT24
# if defined DepthComponent24
#  pragma push_macro("DepthComponent24")
#  undef DepthComponent24
	Transform<PixelDataInternalFormat::DepthComponent24> DepthComponent24;
#  pragma pop_macro("DepthComponent24")
# else
	Transform<PixelDataInternalFormat::DepthComponent24> DepthComponent24;
# endif
#endif
#if defined GL_DEPTH_COMPONENT32
# if defined DepthComponent32
#  pragma push_macro("DepthComponent32")
#  undef DepthComponent32
	Transform<PixelDataInternalFormat::DepthComponent32> DepthComponent32;
#  pragma pop_macro("DepthComponent32")
# else
	Transform<PixelDataInternalFormat::DepthComponent32> DepthComponent32;
# endif
#endif
#if defined GL_DEPTH_COMPONENT32F
# if defined DepthComponent32F
#  pragma push_macro("DepthComponent32F")
#  undef DepthComponent32F
	Transform<PixelDataInternalFormat::DepthComponent32F> DepthComponent32F;
#  pragma pop_macro("DepthComponent32F")
# else
	Transform<PixelDataInternalFormat::DepthComponent32F> DepthComponent32F;
# endif
#endif
#if defined GL_DEPTH24_STENCIL8
# if defined Depth24Stencil8
#  pragma push_macro("Depth24Stencil8")
#  undef Depth24Stencil8
	Transform<PixelDataInternalFormat::Depth24Stencil8> Depth24Stencil8;
#  pragma pop_macro("Depth24Stencil8")
# else
	Transform<PixelDataInternalFormat::Depth24Stencil8> Depth24Stencil8;
# endif
#endif
#if defined GL_DEPTH32F_STENCIL8
# if defined Depth32fStencil8
#  pragma push_macro("Depth32fStencil8")
#  undef Depth32fStencil8
	Transform<PixelDataInternalFormat::Depth32fStencil8> Depth32fStencil8;
#  pragma pop_macro("Depth32fStencil8")
# else
	Transform<PixelDataInternalFormat::Depth32fStencil8> Depth32fStencil8;
# endif
#endif
#if defined GL_COMPRESSED_RED
# if defined CompressedRed
#  pragma push_macro("CompressedRed")
#  undef CompressedRed
	Transform<PixelDataInternalFormat::CompressedRed> CompressedRed;
#  pragma pop_macro("CompressedRed")
# else
	Transform<PixelDataInternalFormat::CompressedRed> CompressedRed;
# endif
#endif
#if defined GL_COMPRESSED_RG
# if defined CompressedRG
#  pragma push_macro("CompressedRG")
#  undef CompressedRG
	Transform<PixelDataInternalFormat::CompressedRG> CompressedRG;
#  pragma pop_macro("CompressedRG")
# else
	Transform<PixelDataInternalFormat::CompressedRG> CompressedRG;
# endif
#endif
#if defined GL_COMPRESSED_RGB
# if defined CompressedRGB
#  pragma push_macro("CompressedRGB")
#  undef CompressedRGB
	Transform<PixelDataInternalFormat::CompressedRGB> CompressedRGB;
#  pragma pop_macro("CompressedRGB")
# else
	Transform<PixelDataInternalFormat::CompressedRGB> CompressedRGB;
# endif
#endif
#if defined GL_COMPRESSED_RGBA
# if defined CompressedRGBA
#  pragma push_macro("CompressedRGBA")
#  undef CompressedRGBA
	Transform<PixelDataInternalFormat::CompressedRGBA> CompressedRGBA;
#  pragma pop_macro("CompressedRGBA")
# else
	Transform<PixelDataInternalFormat::CompressedRGBA> CompressedRGBA;
# endif
#endif
#if defined GL_COMPRESSED_SRGB
# if defined CompressedSRGB
#  pragma push_macro("CompressedSRGB")
#  undef CompressedSRGB
	Transform<PixelDataInternalFormat::CompressedSRGB> CompressedSRGB;
#  pragma pop_macro("CompressedSRGB")
# else
	Transform<PixelDataInternalFormat::CompressedSRGB> CompressedSRGB;
# endif
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA
# if defined CompressedSRGBAlpha
#  pragma push_macro("CompressedSRGBAlpha")
#  undef CompressedSRGBAlpha
	Transform<PixelDataInternalFormat::CompressedSRGBAlpha> CompressedSRGBAlpha;
#  pragma pop_macro("CompressedSRGBAlpha")
# else
	Transform<PixelDataInternalFormat::CompressedSRGBAlpha> CompressedSRGBAlpha;
# endif
#endif
#if defined GL_COMPRESSED_RED_RGTC1
# if defined CompressedRedRGTC1
#  pragma push_macro("CompressedRedRGTC1")
#  undef CompressedRedRGTC1
	Transform<PixelDataInternalFormat::CompressedRedRGTC1> CompressedRedRGTC1;
#  pragma pop_macro("CompressedRedRGTC1")
# else
	Transform<PixelDataInternalFormat::CompressedRedRGTC1> CompressedRedRGTC1;
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RED_RGTC1
# if defined CompressedSignedRedRGTC1
#  pragma push_macro("CompressedSignedRedRGTC1")
#  undef CompressedSignedRedRGTC1
	Transform<PixelDataInternalFormat::CompressedSignedRedRGTC1> CompressedSignedRedRGTC1;
#  pragma pop_macro("CompressedSignedRedRGTC1")
# else
	Transform<PixelDataInternalFormat::CompressedSignedRedRGTC1> CompressedSignedRedRGTC1;
# endif
#endif
#if defined GL_COMPRESSED_RG_RGTC2
# if defined CompressedRGRGTC2
#  pragma push_macro("CompressedRGRGTC2")
#  undef CompressedRGRGTC2
	Transform<PixelDataInternalFormat::CompressedRGRGTC2> CompressedRGRGTC2;
#  pragma pop_macro("CompressedRGRGTC2")
# else
	Transform<PixelDataInternalFormat::CompressedRGRGTC2> CompressedRGRGTC2;
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RG_RGTC2
# if defined CompressedSignedRGRGTC2
#  pragma push_macro("CompressedSignedRGRGTC2")
#  undef CompressedSignedRGRGTC2
	Transform<PixelDataInternalFormat::CompressedSignedRGRGTC2> CompressedSignedRGRGTC2;
#  pragma pop_macro("CompressedSignedRGRGTC2")
# else
	Transform<PixelDataInternalFormat::CompressedSignedRGRGTC2> CompressedSignedRGRGTC2;
# endif
#endif
#if defined GL_COMPRESSED_RGBA_BPTC_UNORM
# if defined CompressedRGBABPTCUNorm
#  pragma push_macro("CompressedRGBABPTCUNorm")
#  undef CompressedRGBABPTCUNorm
	Transform<PixelDataInternalFormat::CompressedRGBABPTCUNorm> CompressedRGBABPTCUNorm;
#  pragma pop_macro("CompressedRGBABPTCUNorm")
# else
	Transform<PixelDataInternalFormat::CompressedRGBABPTCUNorm> CompressedRGBABPTCUNorm;
# endif
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM
# if defined CompressedSRGBAlphaBPTCUNorm
#  pragma push_macro("CompressedSRGBAlphaBPTCUNorm")
#  undef CompressedSRGBAlphaBPTCUNorm
	Transform<PixelDataInternalFormat::CompressedSRGBAlphaBPTCUNorm> CompressedSRGBAlphaBPTCUNorm;
#  pragma pop_macro("CompressedSRGBAlphaBPTCUNorm")
# else
	Transform<PixelDataInternalFormat::CompressedSRGBAlphaBPTCUNorm> CompressedSRGBAlphaBPTCUNorm;
# endif
#endif
#if defined GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
# if defined CompressedRGBBPTCSignedFloat
#  pragma push_macro("CompressedRGBBPTCSignedFloat")
#  undef CompressedRGBBPTCSignedFloat
	Transform<PixelDataInternalFormat::CompressedRGBBPTCSignedFloat> CompressedRGBBPTCSignedFloat;
#  pragma pop_macro("CompressedRGBBPTCSignedFloat")
# else
	Transform<PixelDataInternalFormat::CompressedRGBBPTCSignedFloat> CompressedRGBBPTCSignedFloat;
# endif
#endif
#if defined GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
# if defined CompressedRGBBPTCUnsignedFloat
#  pragma push_macro("CompressedRGBBPTCUnsignedFloat")
#  undef CompressedRGBBPTCUnsignedFloat
	Transform<PixelDataInternalFormat::CompressedRGBBPTCUnsignedFloat> CompressedRGBBPTCUnsignedFloat;
#  pragma pop_macro("CompressedRGBBPTCUnsignedFloat")
# else
	Transform<PixelDataInternalFormat::CompressedRGBBPTCUnsignedFloat> CompressedRGBBPTCUnsignedFloat;
# endif
#endif
#if defined GL_COMPRESSED_RGB8_ETC2
# if defined CompressedRGB8ETC2
#  pragma push_macro("CompressedRGB8ETC2")
#  undef CompressedRGB8ETC2
	Transform<PixelDataInternalFormat::CompressedRGB8ETC2> CompressedRGB8ETC2;
#  pragma pop_macro("CompressedRGB8ETC2")
# else
	Transform<PixelDataInternalFormat::CompressedRGB8ETC2> CompressedRGB8ETC2;
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_ETC2
# if defined CompressedSRGB8ETC2
#  pragma push_macro("CompressedSRGB8ETC2")
#  undef CompressedSRGB8ETC2
	Transform<PixelDataInternalFormat::CompressedSRGB8ETC2> CompressedSRGB8ETC2;
#  pragma pop_macro("CompressedSRGB8ETC2")
# else
	Transform<PixelDataInternalFormat::CompressedSRGB8ETC2> CompressedSRGB8ETC2;
# endif
#endif
#if defined GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
# if defined CompressedRGB8PunchthroughAlpha1ETC2
#  pragma push_macro("CompressedRGB8PunchthroughAlpha1ETC2")
#  undef CompressedRGB8PunchthroughAlpha1ETC2
	Transform<PixelDataInternalFormat::CompressedRGB8PunchthroughAlpha1ETC2> CompressedRGB8PunchthroughAlpha1ETC2;
#  pragma pop_macro("CompressedRGB8PunchthroughAlpha1ETC2")
# else
	Transform<PixelDataInternalFormat::CompressedRGB8PunchthroughAlpha1ETC2> CompressedRGB8PunchthroughAlpha1ETC2;
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
# if defined CompressedSRGB8PunchthroughAlpha1ETC2
#  pragma push_macro("CompressedSRGB8PunchthroughAlpha1ETC2")
#  undef CompressedSRGB8PunchthroughAlpha1ETC2
	Transform<PixelDataInternalFormat::CompressedSRGB8PunchthroughAlpha1ETC2> CompressedSRGB8PunchthroughAlpha1ETC2;
#  pragma pop_macro("CompressedSRGB8PunchthroughAlpha1ETC2")
# else
	Transform<PixelDataInternalFormat::CompressedSRGB8PunchthroughAlpha1ETC2> CompressedSRGB8PunchthroughAlpha1ETC2;
# endif
#endif
#if defined GL_COMPRESSED_RGBA8_ETC2_EAC
# if defined CompressedETC2EAC
#  pragma push_macro("CompressedETC2EAC")
#  undef CompressedETC2EAC
	Transform<PixelDataInternalFormat::CompressedETC2EAC> CompressedETC2EAC;
#  pragma pop_macro("CompressedETC2EAC")
# else
	Transform<PixelDataInternalFormat::CompressedETC2EAC> CompressedETC2EAC;
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
# if defined CompressedSRGB8Alpha8ETC2EAC
#  pragma push_macro("CompressedSRGB8Alpha8ETC2EAC")
#  undef CompressedSRGB8Alpha8ETC2EAC
	Transform<PixelDataInternalFormat::CompressedSRGB8Alpha8ETC2EAC> CompressedSRGB8Alpha8ETC2EAC;
#  pragma pop_macro("CompressedSRGB8Alpha8ETC2EAC")
# else
	Transform<PixelDataInternalFormat::CompressedSRGB8Alpha8ETC2EAC> CompressedSRGB8Alpha8ETC2EAC;
# endif
#endif
#if defined GL_COMPRESSED_R11_EAC
# if defined CompressedR11EAC
#  pragma push_macro("CompressedR11EAC")
#  undef CompressedR11EAC
	Transform<PixelDataInternalFormat::CompressedR11EAC> CompressedR11EAC;
#  pragma pop_macro("CompressedR11EAC")
# else
	Transform<PixelDataInternalFormat::CompressedR11EAC> CompressedR11EAC;
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_R11_EAC
# if defined CompressedSignedR11EAC
#  pragma push_macro("CompressedSignedR11EAC")
#  undef CompressedSignedR11EAC
	Transform<PixelDataInternalFormat::CompressedSignedR11EAC> CompressedSignedR11EAC;
#  pragma pop_macro("CompressedSignedR11EAC")
# else
	Transform<PixelDataInternalFormat::CompressedSignedR11EAC> CompressedSignedR11EAC;
# endif
#endif
#if defined GL_COMPRESSED_RG11_EAC
# if defined CompressedRG11EAC
#  pragma push_macro("CompressedRG11EAC")
#  undef CompressedRG11EAC
	Transform<PixelDataInternalFormat::CompressedRG11EAC> CompressedRG11EAC;
#  pragma pop_macro("CompressedRG11EAC")
# else
	Transform<PixelDataInternalFormat::CompressedRG11EAC> CompressedRG11EAC;
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RG11_EAC
# if defined CompressedSignedRG11EAC
#  pragma push_macro("CompressedSignedRG11EAC")
#  undef CompressedSignedRG11EAC
	Transform<PixelDataInternalFormat::CompressedSignedRG11EAC> CompressedSignedRG11EAC;
#  pragma pop_macro("CompressedSignedRG11EAC")
# else
	Transform<PixelDataInternalFormat::CompressedSignedRG11EAC> CompressedSignedRG11EAC;
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
#if defined GL_STENCIL_INDEX8
# if defined StencilIndex8
#  pragma push_macro("StencilIndex8")
#  undef StencilIndex8
	 , StencilIndex8(_base())
#  pragma pop_macro("StencilIndex8")
# else
	 , StencilIndex8(_base())
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
#if defined GL_R8
# if defined R8
#  pragma push_macro("R8")
#  undef R8
	 , R8(_base())
#  pragma pop_macro("R8")
# else
	 , R8(_base())
# endif
#endif
#if defined GL_R8_SNORM
# if defined R8SNorm
#  pragma push_macro("R8SNorm")
#  undef R8SNorm
	 , R8SNorm(_base())
#  pragma pop_macro("R8SNorm")
# else
	 , R8SNorm(_base())
# endif
#endif
#if defined GL_R16
# if defined R16
#  pragma push_macro("R16")
#  undef R16
	 , R16(_base())
#  pragma pop_macro("R16")
# else
	 , R16(_base())
# endif
#endif
#if defined GL_R16_SNORM
# if defined R16SNorm
#  pragma push_macro("R16SNorm")
#  undef R16SNorm
	 , R16SNorm(_base())
#  pragma pop_macro("R16SNorm")
# else
	 , R16SNorm(_base())
# endif
#endif
#if defined GL_RG8
# if defined RG8
#  pragma push_macro("RG8")
#  undef RG8
	 , RG8(_base())
#  pragma pop_macro("RG8")
# else
	 , RG8(_base())
# endif
#endif
#if defined GL_RG8_SNORM
# if defined RG8SNorm
#  pragma push_macro("RG8SNorm")
#  undef RG8SNorm
	 , RG8SNorm(_base())
#  pragma pop_macro("RG8SNorm")
# else
	 , RG8SNorm(_base())
# endif
#endif
#if defined GL_RG16
# if defined RG16
#  pragma push_macro("RG16")
#  undef RG16
	 , RG16(_base())
#  pragma pop_macro("RG16")
# else
	 , RG16(_base())
# endif
#endif
#if defined GL_RG16_SNORM
# if defined RG16SNorm
#  pragma push_macro("RG16SNorm")
#  undef RG16SNorm
	 , RG16SNorm(_base())
#  pragma pop_macro("RG16SNorm")
# else
	 , RG16SNorm(_base())
# endif
#endif
#if defined GL_R3_G3_B2
# if defined R3G3B2
#  pragma push_macro("R3G3B2")
#  undef R3G3B2
	 , R3G3B2(_base())
#  pragma pop_macro("R3G3B2")
# else
	 , R3G3B2(_base())
# endif
#endif
#if defined GL_RGB4
# if defined RGB4
#  pragma push_macro("RGB4")
#  undef RGB4
	 , RGB4(_base())
#  pragma pop_macro("RGB4")
# else
	 , RGB4(_base())
# endif
#endif
#if defined GL_RGB5
# if defined RGB5
#  pragma push_macro("RGB5")
#  undef RGB5
	 , RGB5(_base())
#  pragma pop_macro("RGB5")
# else
	 , RGB5(_base())
# endif
#endif
#if defined GL_RGB8
# if defined RGB8
#  pragma push_macro("RGB8")
#  undef RGB8
	 , RGB8(_base())
#  pragma pop_macro("RGB8")
# else
	 , RGB8(_base())
# endif
#endif
#if defined GL_RGB8_SNORM
# if defined RGB8SNorm
#  pragma push_macro("RGB8SNorm")
#  undef RGB8SNorm
	 , RGB8SNorm(_base())
#  pragma pop_macro("RGB8SNorm")
# else
	 , RGB8SNorm(_base())
# endif
#endif
#if defined GL_RGB10
# if defined RGB10
#  pragma push_macro("RGB10")
#  undef RGB10
	 , RGB10(_base())
#  pragma pop_macro("RGB10")
# else
	 , RGB10(_base())
# endif
#endif
#if defined GL_RGB12
# if defined RGB12
#  pragma push_macro("RGB12")
#  undef RGB12
	 , RGB12(_base())
#  pragma pop_macro("RGB12")
# else
	 , RGB12(_base())
# endif
#endif
#if defined GL_RGB16
# if defined RGB16
#  pragma push_macro("RGB16")
#  undef RGB16
	 , RGB16(_base())
#  pragma pop_macro("RGB16")
# else
	 , RGB16(_base())
# endif
#endif
#if defined GL_RGB16_SNORM
# if defined RGB16SNorm
#  pragma push_macro("RGB16SNorm")
#  undef RGB16SNorm
	 , RGB16SNorm(_base())
#  pragma pop_macro("RGB16SNorm")
# else
	 , RGB16SNorm(_base())
# endif
#endif
#if defined GL_RGBA2
# if defined RGBA2
#  pragma push_macro("RGBA2")
#  undef RGBA2
	 , RGBA2(_base())
#  pragma pop_macro("RGBA2")
# else
	 , RGBA2(_base())
# endif
#endif
#if defined GL_RGBA4
# if defined RGBA4
#  pragma push_macro("RGBA4")
#  undef RGBA4
	 , RGBA4(_base())
#  pragma pop_macro("RGBA4")
# else
	 , RGBA4(_base())
# endif
#endif
#if defined GL_RGB5_A1
# if defined RGB5A1
#  pragma push_macro("RGB5A1")
#  undef RGB5A1
	 , RGB5A1(_base())
#  pragma pop_macro("RGB5A1")
# else
	 , RGB5A1(_base())
# endif
#endif
#if defined GL_RGBA8
# if defined RGBA8
#  pragma push_macro("RGBA8")
#  undef RGBA8
	 , RGBA8(_base())
#  pragma pop_macro("RGBA8")
# else
	 , RGBA8(_base())
# endif
#endif
#if defined GL_RGBA8_SNORM
# if defined RGBA8SNorm
#  pragma push_macro("RGBA8SNorm")
#  undef RGBA8SNorm
	 , RGBA8SNorm(_base())
#  pragma pop_macro("RGBA8SNorm")
# else
	 , RGBA8SNorm(_base())
# endif
#endif
#if defined GL_RGB10_A2
# if defined RGB10A2
#  pragma push_macro("RGB10A2")
#  undef RGB10A2
	 , RGB10A2(_base())
#  pragma pop_macro("RGB10A2")
# else
	 , RGB10A2(_base())
# endif
#endif
#if defined GL_RGB10_A2UI
# if defined RGB10A2UI
#  pragma push_macro("RGB10A2UI")
#  undef RGB10A2UI
	 , RGB10A2UI(_base())
#  pragma pop_macro("RGB10A2UI")
# else
	 , RGB10A2UI(_base())
# endif
#endif
#if defined GL_RGBA12
# if defined RGBA12
#  pragma push_macro("RGBA12")
#  undef RGBA12
	 , RGBA12(_base())
#  pragma pop_macro("RGBA12")
# else
	 , RGBA12(_base())
# endif
#endif
#if defined GL_RGBA16
# if defined RGBA16
#  pragma push_macro("RGBA16")
#  undef RGBA16
	 , RGBA16(_base())
#  pragma pop_macro("RGBA16")
# else
	 , RGBA16(_base())
# endif
#endif
#if defined GL_RGBA16_SNORM
# if defined RGBA16SNorm
#  pragma push_macro("RGBA16SNorm")
#  undef RGBA16SNorm
	 , RGBA16SNorm(_base())
#  pragma pop_macro("RGBA16SNorm")
# else
	 , RGBA16SNorm(_base())
# endif
#endif
#if defined GL_SRGB8
# if defined SRGB8
#  pragma push_macro("SRGB8")
#  undef SRGB8
	 , SRGB8(_base())
#  pragma pop_macro("SRGB8")
# else
	 , SRGB8(_base())
# endif
#endif
#if defined GL_SRGB8_ALPHA8
# if defined SRGB8Alpha8
#  pragma push_macro("SRGB8Alpha8")
#  undef SRGB8Alpha8
	 , SRGB8Alpha8(_base())
#  pragma pop_macro("SRGB8Alpha8")
# else
	 , SRGB8Alpha8(_base())
# endif
#endif
#if defined GL_R16F
# if defined R16F
#  pragma push_macro("R16F")
#  undef R16F
	 , R16F(_base())
#  pragma pop_macro("R16F")
# else
	 , R16F(_base())
# endif
#endif
#if defined GL_RG16F
# if defined RG16F
#  pragma push_macro("RG16F")
#  undef RG16F
	 , RG16F(_base())
#  pragma pop_macro("RG16F")
# else
	 , RG16F(_base())
# endif
#endif
#if defined GL_RGB16F
# if defined RGB16F
#  pragma push_macro("RGB16F")
#  undef RGB16F
	 , RGB16F(_base())
#  pragma pop_macro("RGB16F")
# else
	 , RGB16F(_base())
# endif
#endif
#if defined GL_RGBA16F
# if defined RGBA16F
#  pragma push_macro("RGBA16F")
#  undef RGBA16F
	 , RGBA16F(_base())
#  pragma pop_macro("RGBA16F")
# else
	 , RGBA16F(_base())
# endif
#endif
#if defined GL_R32F
# if defined R32F
#  pragma push_macro("R32F")
#  undef R32F
	 , R32F(_base())
#  pragma pop_macro("R32F")
# else
	 , R32F(_base())
# endif
#endif
#if defined GL_RG32F
# if defined RG32F
#  pragma push_macro("RG32F")
#  undef RG32F
	 , RG32F(_base())
#  pragma pop_macro("RG32F")
# else
	 , RG32F(_base())
# endif
#endif
#if defined GL_RGB32F
# if defined RGB32F
#  pragma push_macro("RGB32F")
#  undef RGB32F
	 , RGB32F(_base())
#  pragma pop_macro("RGB32F")
# else
	 , RGB32F(_base())
# endif
#endif
#if defined GL_RGBA32F
# if defined RGBA32F
#  pragma push_macro("RGBA32F")
#  undef RGBA32F
	 , RGBA32F(_base())
#  pragma pop_macro("RGBA32F")
# else
	 , RGBA32F(_base())
# endif
#endif
#if defined GL_R11F_G11F_B10F
# if defined R11FG11FB10F
#  pragma push_macro("R11FG11FB10F")
#  undef R11FG11FB10F
	 , R11FG11FB10F(_base())
#  pragma pop_macro("R11FG11FB10F")
# else
	 , R11FG11FB10F(_base())
# endif
#endif
#if defined GL_RGB9_E5
# if defined RGB9E5
#  pragma push_macro("RGB9E5")
#  undef RGB9E5
	 , RGB9E5(_base())
#  pragma pop_macro("RGB9E5")
# else
	 , RGB9E5(_base())
# endif
#endif
#if defined GL_R8I
# if defined R8I
#  pragma push_macro("R8I")
#  undef R8I
	 , R8I(_base())
#  pragma pop_macro("R8I")
# else
	 , R8I(_base())
# endif
#endif
#if defined GL_R8UI
# if defined R8UI
#  pragma push_macro("R8UI")
#  undef R8UI
	 , R8UI(_base())
#  pragma pop_macro("R8UI")
# else
	 , R8UI(_base())
# endif
#endif
#if defined GL_R16I
# if defined R16I
#  pragma push_macro("R16I")
#  undef R16I
	 , R16I(_base())
#  pragma pop_macro("R16I")
# else
	 , R16I(_base())
# endif
#endif
#if defined GL_R16UI
# if defined R16UI
#  pragma push_macro("R16UI")
#  undef R16UI
	 , R16UI(_base())
#  pragma pop_macro("R16UI")
# else
	 , R16UI(_base())
# endif
#endif
#if defined GL_R32I
# if defined R32I
#  pragma push_macro("R32I")
#  undef R32I
	 , R32I(_base())
#  pragma pop_macro("R32I")
# else
	 , R32I(_base())
# endif
#endif
#if defined GL_R32UI
# if defined R32UI
#  pragma push_macro("R32UI")
#  undef R32UI
	 , R32UI(_base())
#  pragma pop_macro("R32UI")
# else
	 , R32UI(_base())
# endif
#endif
#if defined GL_RG8I
# if defined RG8I
#  pragma push_macro("RG8I")
#  undef RG8I
	 , RG8I(_base())
#  pragma pop_macro("RG8I")
# else
	 , RG8I(_base())
# endif
#endif
#if defined GL_RG8UI
# if defined RG8UI
#  pragma push_macro("RG8UI")
#  undef RG8UI
	 , RG8UI(_base())
#  pragma pop_macro("RG8UI")
# else
	 , RG8UI(_base())
# endif
#endif
#if defined GL_RG16I
# if defined RG16I
#  pragma push_macro("RG16I")
#  undef RG16I
	 , RG16I(_base())
#  pragma pop_macro("RG16I")
# else
	 , RG16I(_base())
# endif
#endif
#if defined GL_RG16UI
# if defined RG16UI
#  pragma push_macro("RG16UI")
#  undef RG16UI
	 , RG16UI(_base())
#  pragma pop_macro("RG16UI")
# else
	 , RG16UI(_base())
# endif
#endif
#if defined GL_RG32I
# if defined RG32I
#  pragma push_macro("RG32I")
#  undef RG32I
	 , RG32I(_base())
#  pragma pop_macro("RG32I")
# else
	 , RG32I(_base())
# endif
#endif
#if defined GL_RG32UI
# if defined RG32UI
#  pragma push_macro("RG32UI")
#  undef RG32UI
	 , RG32UI(_base())
#  pragma pop_macro("RG32UI")
# else
	 , RG32UI(_base())
# endif
#endif
#if defined GL_RGB8I
# if defined RGB8I
#  pragma push_macro("RGB8I")
#  undef RGB8I
	 , RGB8I(_base())
#  pragma pop_macro("RGB8I")
# else
	 , RGB8I(_base())
# endif
#endif
#if defined GL_RGB8UI
# if defined RGB8UI
#  pragma push_macro("RGB8UI")
#  undef RGB8UI
	 , RGB8UI(_base())
#  pragma pop_macro("RGB8UI")
# else
	 , RGB8UI(_base())
# endif
#endif
#if defined GL_RGB16I
# if defined RGB16I
#  pragma push_macro("RGB16I")
#  undef RGB16I
	 , RGB16I(_base())
#  pragma pop_macro("RGB16I")
# else
	 , RGB16I(_base())
# endif
#endif
#if defined GL_RGB16UI
# if defined RGB16UI
#  pragma push_macro("RGB16UI")
#  undef RGB16UI
	 , RGB16UI(_base())
#  pragma pop_macro("RGB16UI")
# else
	 , RGB16UI(_base())
# endif
#endif
#if defined GL_RGB32I
# if defined RGB32I
#  pragma push_macro("RGB32I")
#  undef RGB32I
	 , RGB32I(_base())
#  pragma pop_macro("RGB32I")
# else
	 , RGB32I(_base())
# endif
#endif
#if defined GL_RGB32UI
# if defined RGB32UI
#  pragma push_macro("RGB32UI")
#  undef RGB32UI
	 , RGB32UI(_base())
#  pragma pop_macro("RGB32UI")
# else
	 , RGB32UI(_base())
# endif
#endif
#if defined GL_RGBA8I
# if defined RGBA8I
#  pragma push_macro("RGBA8I")
#  undef RGBA8I
	 , RGBA8I(_base())
#  pragma pop_macro("RGBA8I")
# else
	 , RGBA8I(_base())
# endif
#endif
#if defined GL_RGBA8UI
# if defined RGBA8UI
#  pragma push_macro("RGBA8UI")
#  undef RGBA8UI
	 , RGBA8UI(_base())
#  pragma pop_macro("RGBA8UI")
# else
	 , RGBA8UI(_base())
# endif
#endif
#if defined GL_RGBA16I
# if defined RGBA16I
#  pragma push_macro("RGBA16I")
#  undef RGBA16I
	 , RGBA16I(_base())
#  pragma pop_macro("RGBA16I")
# else
	 , RGBA16I(_base())
# endif
#endif
#if defined GL_RGBA16UI
# if defined RGBA16UI
#  pragma push_macro("RGBA16UI")
#  undef RGBA16UI
	 , RGBA16UI(_base())
#  pragma pop_macro("RGBA16UI")
# else
	 , RGBA16UI(_base())
# endif
#endif
#if defined GL_RGBA32I
# if defined RGBA32I
#  pragma push_macro("RGBA32I")
#  undef RGBA32I
	 , RGBA32I(_base())
#  pragma pop_macro("RGBA32I")
# else
	 , RGBA32I(_base())
# endif
#endif
#if defined GL_RGBA32UI
# if defined RGBA32UI
#  pragma push_macro("RGBA32UI")
#  undef RGBA32UI
	 , RGBA32UI(_base())
#  pragma pop_macro("RGBA32UI")
# else
	 , RGBA32UI(_base())
# endif
#endif
#if defined GL_DEPTH_COMPONENT16
# if defined DepthComponent16
#  pragma push_macro("DepthComponent16")
#  undef DepthComponent16
	 , DepthComponent16(_base())
#  pragma pop_macro("DepthComponent16")
# else
	 , DepthComponent16(_base())
# endif
#endif
#if defined GL_DEPTH_COMPONENT24
# if defined DepthComponent24
#  pragma push_macro("DepthComponent24")
#  undef DepthComponent24
	 , DepthComponent24(_base())
#  pragma pop_macro("DepthComponent24")
# else
	 , DepthComponent24(_base())
# endif
#endif
#if defined GL_DEPTH_COMPONENT32
# if defined DepthComponent32
#  pragma push_macro("DepthComponent32")
#  undef DepthComponent32
	 , DepthComponent32(_base())
#  pragma pop_macro("DepthComponent32")
# else
	 , DepthComponent32(_base())
# endif
#endif
#if defined GL_DEPTH_COMPONENT32F
# if defined DepthComponent32F
#  pragma push_macro("DepthComponent32F")
#  undef DepthComponent32F
	 , DepthComponent32F(_base())
#  pragma pop_macro("DepthComponent32F")
# else
	 , DepthComponent32F(_base())
# endif
#endif
#if defined GL_DEPTH24_STENCIL8
# if defined Depth24Stencil8
#  pragma push_macro("Depth24Stencil8")
#  undef Depth24Stencil8
	 , Depth24Stencil8(_base())
#  pragma pop_macro("Depth24Stencil8")
# else
	 , Depth24Stencil8(_base())
# endif
#endif
#if defined GL_DEPTH32F_STENCIL8
# if defined Depth32fStencil8
#  pragma push_macro("Depth32fStencil8")
#  undef Depth32fStencil8
	 , Depth32fStencil8(_base())
#  pragma pop_macro("Depth32fStencil8")
# else
	 , Depth32fStencil8(_base())
# endif
#endif
#if defined GL_COMPRESSED_RED
# if defined CompressedRed
#  pragma push_macro("CompressedRed")
#  undef CompressedRed
	 , CompressedRed(_base())
#  pragma pop_macro("CompressedRed")
# else
	 , CompressedRed(_base())
# endif
#endif
#if defined GL_COMPRESSED_RG
# if defined CompressedRG
#  pragma push_macro("CompressedRG")
#  undef CompressedRG
	 , CompressedRG(_base())
#  pragma pop_macro("CompressedRG")
# else
	 , CompressedRG(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGB
# if defined CompressedRGB
#  pragma push_macro("CompressedRGB")
#  undef CompressedRGB
	 , CompressedRGB(_base())
#  pragma pop_macro("CompressedRGB")
# else
	 , CompressedRGB(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGBA
# if defined CompressedRGBA
#  pragma push_macro("CompressedRGBA")
#  undef CompressedRGBA
	 , CompressedRGBA(_base())
#  pragma pop_macro("CompressedRGBA")
# else
	 , CompressedRGBA(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB
# if defined CompressedSRGB
#  pragma push_macro("CompressedSRGB")
#  undef CompressedSRGB
	 , CompressedSRGB(_base())
#  pragma pop_macro("CompressedSRGB")
# else
	 , CompressedSRGB(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA
# if defined CompressedSRGBAlpha
#  pragma push_macro("CompressedSRGBAlpha")
#  undef CompressedSRGBAlpha
	 , CompressedSRGBAlpha(_base())
#  pragma pop_macro("CompressedSRGBAlpha")
# else
	 , CompressedSRGBAlpha(_base())
# endif
#endif
#if defined GL_COMPRESSED_RED_RGTC1
# if defined CompressedRedRGTC1
#  pragma push_macro("CompressedRedRGTC1")
#  undef CompressedRedRGTC1
	 , CompressedRedRGTC1(_base())
#  pragma pop_macro("CompressedRedRGTC1")
# else
	 , CompressedRedRGTC1(_base())
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RED_RGTC1
# if defined CompressedSignedRedRGTC1
#  pragma push_macro("CompressedSignedRedRGTC1")
#  undef CompressedSignedRedRGTC1
	 , CompressedSignedRedRGTC1(_base())
#  pragma pop_macro("CompressedSignedRedRGTC1")
# else
	 , CompressedSignedRedRGTC1(_base())
# endif
#endif
#if defined GL_COMPRESSED_RG_RGTC2
# if defined CompressedRGRGTC2
#  pragma push_macro("CompressedRGRGTC2")
#  undef CompressedRGRGTC2
	 , CompressedRGRGTC2(_base())
#  pragma pop_macro("CompressedRGRGTC2")
# else
	 , CompressedRGRGTC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RG_RGTC2
# if defined CompressedSignedRGRGTC2
#  pragma push_macro("CompressedSignedRGRGTC2")
#  undef CompressedSignedRGRGTC2
	 , CompressedSignedRGRGTC2(_base())
#  pragma pop_macro("CompressedSignedRGRGTC2")
# else
	 , CompressedSignedRGRGTC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGBA_BPTC_UNORM
# if defined CompressedRGBABPTCUNorm
#  pragma push_macro("CompressedRGBABPTCUNorm")
#  undef CompressedRGBABPTCUNorm
	 , CompressedRGBABPTCUNorm(_base())
#  pragma pop_macro("CompressedRGBABPTCUNorm")
# else
	 , CompressedRGBABPTCUNorm(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM
# if defined CompressedSRGBAlphaBPTCUNorm
#  pragma push_macro("CompressedSRGBAlphaBPTCUNorm")
#  undef CompressedSRGBAlphaBPTCUNorm
	 , CompressedSRGBAlphaBPTCUNorm(_base())
#  pragma pop_macro("CompressedSRGBAlphaBPTCUNorm")
# else
	 , CompressedSRGBAlphaBPTCUNorm(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
# if defined CompressedRGBBPTCSignedFloat
#  pragma push_macro("CompressedRGBBPTCSignedFloat")
#  undef CompressedRGBBPTCSignedFloat
	 , CompressedRGBBPTCSignedFloat(_base())
#  pragma pop_macro("CompressedRGBBPTCSignedFloat")
# else
	 , CompressedRGBBPTCSignedFloat(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
# if defined CompressedRGBBPTCUnsignedFloat
#  pragma push_macro("CompressedRGBBPTCUnsignedFloat")
#  undef CompressedRGBBPTCUnsignedFloat
	 , CompressedRGBBPTCUnsignedFloat(_base())
#  pragma pop_macro("CompressedRGBBPTCUnsignedFloat")
# else
	 , CompressedRGBBPTCUnsignedFloat(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGB8_ETC2
# if defined CompressedRGB8ETC2
#  pragma push_macro("CompressedRGB8ETC2")
#  undef CompressedRGB8ETC2
	 , CompressedRGB8ETC2(_base())
#  pragma pop_macro("CompressedRGB8ETC2")
# else
	 , CompressedRGB8ETC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_ETC2
# if defined CompressedSRGB8ETC2
#  pragma push_macro("CompressedSRGB8ETC2")
#  undef CompressedSRGB8ETC2
	 , CompressedSRGB8ETC2(_base())
#  pragma pop_macro("CompressedSRGB8ETC2")
# else
	 , CompressedSRGB8ETC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
# if defined CompressedRGB8PunchthroughAlpha1ETC2
#  pragma push_macro("CompressedRGB8PunchthroughAlpha1ETC2")
#  undef CompressedRGB8PunchthroughAlpha1ETC2
	 , CompressedRGB8PunchthroughAlpha1ETC2(_base())
#  pragma pop_macro("CompressedRGB8PunchthroughAlpha1ETC2")
# else
	 , CompressedRGB8PunchthroughAlpha1ETC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
# if defined CompressedSRGB8PunchthroughAlpha1ETC2
#  pragma push_macro("CompressedSRGB8PunchthroughAlpha1ETC2")
#  undef CompressedSRGB8PunchthroughAlpha1ETC2
	 , CompressedSRGB8PunchthroughAlpha1ETC2(_base())
#  pragma pop_macro("CompressedSRGB8PunchthroughAlpha1ETC2")
# else
	 , CompressedSRGB8PunchthroughAlpha1ETC2(_base())
# endif
#endif
#if defined GL_COMPRESSED_RGBA8_ETC2_EAC
# if defined CompressedETC2EAC
#  pragma push_macro("CompressedETC2EAC")
#  undef CompressedETC2EAC
	 , CompressedETC2EAC(_base())
#  pragma pop_macro("CompressedETC2EAC")
# else
	 , CompressedETC2EAC(_base())
# endif
#endif
#if defined GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
# if defined CompressedSRGB8Alpha8ETC2EAC
#  pragma push_macro("CompressedSRGB8Alpha8ETC2EAC")
#  undef CompressedSRGB8Alpha8ETC2EAC
	 , CompressedSRGB8Alpha8ETC2EAC(_base())
#  pragma pop_macro("CompressedSRGB8Alpha8ETC2EAC")
# else
	 , CompressedSRGB8Alpha8ETC2EAC(_base())
# endif
#endif
#if defined GL_COMPRESSED_R11_EAC
# if defined CompressedR11EAC
#  pragma push_macro("CompressedR11EAC")
#  undef CompressedR11EAC
	 , CompressedR11EAC(_base())
#  pragma pop_macro("CompressedR11EAC")
# else
	 , CompressedR11EAC(_base())
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_R11_EAC
# if defined CompressedSignedR11EAC
#  pragma push_macro("CompressedSignedR11EAC")
#  undef CompressedSignedR11EAC
	 , CompressedSignedR11EAC(_base())
#  pragma pop_macro("CompressedSignedR11EAC")
# else
	 , CompressedSignedR11EAC(_base())
# endif
#endif
#if defined GL_COMPRESSED_RG11_EAC
# if defined CompressedRG11EAC
#  pragma push_macro("CompressedRG11EAC")
#  undef CompressedRG11EAC
	 , CompressedRG11EAC(_base())
#  pragma pop_macro("CompressedRG11EAC")
# else
	 , CompressedRG11EAC(_base())
# endif
#endif
#if defined GL_COMPRESSED_SIGNED_RG11_EAC
# if defined CompressedSignedRG11EAC
#  pragma push_macro("CompressedSignedRG11EAC")
#  undef CompressedSignedRG11EAC
	 , CompressedSignedRG11EAC(_base())
#  pragma pop_macro("CompressedSignedRG11EAC")
# else
	 , CompressedSignedRG11EAC(_base())
# endif
#endif
	{ }
};

} // namespace enums

