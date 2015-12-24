//  File implement/oglplus/enums/blend_function_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blend_function.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BlendFunction> class Transform>
class EnumToClass<Base, BlendFunction, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	Transform<BlendFunction::Zero> Zero;
#  pragma pop_macro("Zero")
# else
	Transform<BlendFunction::Zero> Zero;
# endif
#endif
#if defined GL_ONE
# if defined One
#  pragma push_macro("One")
#  undef One
	Transform<BlendFunction::One> One;
#  pragma pop_macro("One")
# else
	Transform<BlendFunction::One> One;
# endif
#endif
#if defined GL_SRC_COLOR
# if defined SrcColor
#  pragma push_macro("SrcColor")
#  undef SrcColor
	Transform<BlendFunction::SrcColor> SrcColor;
#  pragma pop_macro("SrcColor")
# else
	Transform<BlendFunction::SrcColor> SrcColor;
# endif
#endif
#if defined GL_ONE_MINUS_SRC_COLOR
# if defined OneMinusSrcColor
#  pragma push_macro("OneMinusSrcColor")
#  undef OneMinusSrcColor
	Transform<BlendFunction::OneMinusSrcColor> OneMinusSrcColor;
#  pragma pop_macro("OneMinusSrcColor")
# else
	Transform<BlendFunction::OneMinusSrcColor> OneMinusSrcColor;
# endif
#endif
#if defined GL_DST_COLOR
# if defined DstColor
#  pragma push_macro("DstColor")
#  undef DstColor
	Transform<BlendFunction::DstColor> DstColor;
#  pragma pop_macro("DstColor")
# else
	Transform<BlendFunction::DstColor> DstColor;
# endif
#endif
#if defined GL_ONE_MINUS_DST_COLOR
# if defined OneMinusDstColor
#  pragma push_macro("OneMinusDstColor")
#  undef OneMinusDstColor
	Transform<BlendFunction::OneMinusDstColor> OneMinusDstColor;
#  pragma pop_macro("OneMinusDstColor")
# else
	Transform<BlendFunction::OneMinusDstColor> OneMinusDstColor;
# endif
#endif
#if defined GL_SRC_ALPHA
# if defined SrcAlpha
#  pragma push_macro("SrcAlpha")
#  undef SrcAlpha
	Transform<BlendFunction::SrcAlpha> SrcAlpha;
#  pragma pop_macro("SrcAlpha")
# else
	Transform<BlendFunction::SrcAlpha> SrcAlpha;
# endif
#endif
#if defined GL_ONE_MINUS_SRC_ALPHA
# if defined OneMinusSrcAlpha
#  pragma push_macro("OneMinusSrcAlpha")
#  undef OneMinusSrcAlpha
	Transform<BlendFunction::OneMinusSrcAlpha> OneMinusSrcAlpha;
#  pragma pop_macro("OneMinusSrcAlpha")
# else
	Transform<BlendFunction::OneMinusSrcAlpha> OneMinusSrcAlpha;
# endif
#endif
#if defined GL_DST_ALPHA
# if defined DstAlpha
#  pragma push_macro("DstAlpha")
#  undef DstAlpha
	Transform<BlendFunction::DstAlpha> DstAlpha;
#  pragma pop_macro("DstAlpha")
# else
	Transform<BlendFunction::DstAlpha> DstAlpha;
# endif
#endif
#if defined GL_ONE_MINUS_DST_ALPHA
# if defined OneMinusDstAlpha
#  pragma push_macro("OneMinusDstAlpha")
#  undef OneMinusDstAlpha
	Transform<BlendFunction::OneMinusDstAlpha> OneMinusDstAlpha;
#  pragma pop_macro("OneMinusDstAlpha")
# else
	Transform<BlendFunction::OneMinusDstAlpha> OneMinusDstAlpha;
# endif
#endif
#if defined GL_CONSTANT_COLOR
# if defined ConstantColor
#  pragma push_macro("ConstantColor")
#  undef ConstantColor
	Transform<BlendFunction::ConstantColor> ConstantColor;
#  pragma pop_macro("ConstantColor")
# else
	Transform<BlendFunction::ConstantColor> ConstantColor;
# endif
#endif
#if defined GL_ONE_MINUS_CONSTANT_COLOR
# if defined OneMinusConstantColor
#  pragma push_macro("OneMinusConstantColor")
#  undef OneMinusConstantColor
	Transform<BlendFunction::OneMinusConstantColor> OneMinusConstantColor;
#  pragma pop_macro("OneMinusConstantColor")
# else
	Transform<BlendFunction::OneMinusConstantColor> OneMinusConstantColor;
# endif
#endif
#if defined GL_CONSTANT_ALPHA
# if defined ConstantAlpha
#  pragma push_macro("ConstantAlpha")
#  undef ConstantAlpha
	Transform<BlendFunction::ConstantAlpha> ConstantAlpha;
#  pragma pop_macro("ConstantAlpha")
# else
	Transform<BlendFunction::ConstantAlpha> ConstantAlpha;
# endif
#endif
#if defined GL_ONE_MINUS_CONSTANT_ALPHA
# if defined OneMinusConstantAlpha
#  pragma push_macro("OneMinusConstantAlpha")
#  undef OneMinusConstantAlpha
	Transform<BlendFunction::OneMinusConstantAlpha> OneMinusConstantAlpha;
#  pragma pop_macro("OneMinusConstantAlpha")
# else
	Transform<BlendFunction::OneMinusConstantAlpha> OneMinusConstantAlpha;
# endif
#endif
#if defined GL_SRC_ALPHA_SATURATE
# if defined SrcAlphaSaturate
#  pragma push_macro("SrcAlphaSaturate")
#  undef SrcAlphaSaturate
	Transform<BlendFunction::SrcAlphaSaturate> SrcAlphaSaturate;
#  pragma pop_macro("SrcAlphaSaturate")
# else
	Transform<BlendFunction::SrcAlphaSaturate> SrcAlphaSaturate;
# endif
#endif
#if defined GL_SRC1_COLOR
# if defined Src1Color
#  pragma push_macro("Src1Color")
#  undef Src1Color
	Transform<BlendFunction::Src1Color> Src1Color;
#  pragma pop_macro("Src1Color")
# else
	Transform<BlendFunction::Src1Color> Src1Color;
# endif
#endif
#if defined GL_ONE_MINUS_SRC1_COLOR
# if defined OneMinusSrc1Color
#  pragma push_macro("OneMinusSrc1Color")
#  undef OneMinusSrc1Color
	Transform<BlendFunction::OneMinusSrc1Color> OneMinusSrc1Color;
#  pragma pop_macro("OneMinusSrc1Color")
# else
	Transform<BlendFunction::OneMinusSrc1Color> OneMinusSrc1Color;
# endif
#endif
#if defined GL_SRC1_ALPHA
# if defined Src1Alpha
#  pragma push_macro("Src1Alpha")
#  undef Src1Alpha
	Transform<BlendFunction::Src1Alpha> Src1Alpha;
#  pragma pop_macro("Src1Alpha")
# else
	Transform<BlendFunction::Src1Alpha> Src1Alpha;
# endif
#endif
#if defined GL_ONE_MINUS_SRC1_ALPHA
# if defined OneMinusSrc1Alpha
#  pragma push_macro("OneMinusSrc1Alpha")
#  undef OneMinusSrc1Alpha
	Transform<BlendFunction::OneMinusSrc1Alpha> OneMinusSrc1Alpha;
#  pragma pop_macro("OneMinusSrc1Alpha")
# else
	Transform<BlendFunction::OneMinusSrc1Alpha> OneMinusSrc1Alpha;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	 , Zero(_base())
#  pragma pop_macro("Zero")
# else
	 , Zero(_base())
# endif
#endif
#if defined GL_ONE
# if defined One
#  pragma push_macro("One")
#  undef One
	 , One(_base())
#  pragma pop_macro("One")
# else
	 , One(_base())
# endif
#endif
#if defined GL_SRC_COLOR
# if defined SrcColor
#  pragma push_macro("SrcColor")
#  undef SrcColor
	 , SrcColor(_base())
#  pragma pop_macro("SrcColor")
# else
	 , SrcColor(_base())
# endif
#endif
#if defined GL_ONE_MINUS_SRC_COLOR
# if defined OneMinusSrcColor
#  pragma push_macro("OneMinusSrcColor")
#  undef OneMinusSrcColor
	 , OneMinusSrcColor(_base())
#  pragma pop_macro("OneMinusSrcColor")
# else
	 , OneMinusSrcColor(_base())
# endif
#endif
#if defined GL_DST_COLOR
# if defined DstColor
#  pragma push_macro("DstColor")
#  undef DstColor
	 , DstColor(_base())
#  pragma pop_macro("DstColor")
# else
	 , DstColor(_base())
# endif
#endif
#if defined GL_ONE_MINUS_DST_COLOR
# if defined OneMinusDstColor
#  pragma push_macro("OneMinusDstColor")
#  undef OneMinusDstColor
	 , OneMinusDstColor(_base())
#  pragma pop_macro("OneMinusDstColor")
# else
	 , OneMinusDstColor(_base())
# endif
#endif
#if defined GL_SRC_ALPHA
# if defined SrcAlpha
#  pragma push_macro("SrcAlpha")
#  undef SrcAlpha
	 , SrcAlpha(_base())
#  pragma pop_macro("SrcAlpha")
# else
	 , SrcAlpha(_base())
# endif
#endif
#if defined GL_ONE_MINUS_SRC_ALPHA
# if defined OneMinusSrcAlpha
#  pragma push_macro("OneMinusSrcAlpha")
#  undef OneMinusSrcAlpha
	 , OneMinusSrcAlpha(_base())
#  pragma pop_macro("OneMinusSrcAlpha")
# else
	 , OneMinusSrcAlpha(_base())
# endif
#endif
#if defined GL_DST_ALPHA
# if defined DstAlpha
#  pragma push_macro("DstAlpha")
#  undef DstAlpha
	 , DstAlpha(_base())
#  pragma pop_macro("DstAlpha")
# else
	 , DstAlpha(_base())
# endif
#endif
#if defined GL_ONE_MINUS_DST_ALPHA
# if defined OneMinusDstAlpha
#  pragma push_macro("OneMinusDstAlpha")
#  undef OneMinusDstAlpha
	 , OneMinusDstAlpha(_base())
#  pragma pop_macro("OneMinusDstAlpha")
# else
	 , OneMinusDstAlpha(_base())
# endif
#endif
#if defined GL_CONSTANT_COLOR
# if defined ConstantColor
#  pragma push_macro("ConstantColor")
#  undef ConstantColor
	 , ConstantColor(_base())
#  pragma pop_macro("ConstantColor")
# else
	 , ConstantColor(_base())
# endif
#endif
#if defined GL_ONE_MINUS_CONSTANT_COLOR
# if defined OneMinusConstantColor
#  pragma push_macro("OneMinusConstantColor")
#  undef OneMinusConstantColor
	 , OneMinusConstantColor(_base())
#  pragma pop_macro("OneMinusConstantColor")
# else
	 , OneMinusConstantColor(_base())
# endif
#endif
#if defined GL_CONSTANT_ALPHA
# if defined ConstantAlpha
#  pragma push_macro("ConstantAlpha")
#  undef ConstantAlpha
	 , ConstantAlpha(_base())
#  pragma pop_macro("ConstantAlpha")
# else
	 , ConstantAlpha(_base())
# endif
#endif
#if defined GL_ONE_MINUS_CONSTANT_ALPHA
# if defined OneMinusConstantAlpha
#  pragma push_macro("OneMinusConstantAlpha")
#  undef OneMinusConstantAlpha
	 , OneMinusConstantAlpha(_base())
#  pragma pop_macro("OneMinusConstantAlpha")
# else
	 , OneMinusConstantAlpha(_base())
# endif
#endif
#if defined GL_SRC_ALPHA_SATURATE
# if defined SrcAlphaSaturate
#  pragma push_macro("SrcAlphaSaturate")
#  undef SrcAlphaSaturate
	 , SrcAlphaSaturate(_base())
#  pragma pop_macro("SrcAlphaSaturate")
# else
	 , SrcAlphaSaturate(_base())
# endif
#endif
#if defined GL_SRC1_COLOR
# if defined Src1Color
#  pragma push_macro("Src1Color")
#  undef Src1Color
	 , Src1Color(_base())
#  pragma pop_macro("Src1Color")
# else
	 , Src1Color(_base())
# endif
#endif
#if defined GL_ONE_MINUS_SRC1_COLOR
# if defined OneMinusSrc1Color
#  pragma push_macro("OneMinusSrc1Color")
#  undef OneMinusSrc1Color
	 , OneMinusSrc1Color(_base())
#  pragma pop_macro("OneMinusSrc1Color")
# else
	 , OneMinusSrc1Color(_base())
# endif
#endif
#if defined GL_SRC1_ALPHA
# if defined Src1Alpha
#  pragma push_macro("Src1Alpha")
#  undef Src1Alpha
	 , Src1Alpha(_base())
#  pragma pop_macro("Src1Alpha")
# else
	 , Src1Alpha(_base())
# endif
#endif
#if defined GL_ONE_MINUS_SRC1_ALPHA
# if defined OneMinusSrc1Alpha
#  pragma push_macro("OneMinusSrc1Alpha")
#  undef OneMinusSrc1Alpha
	 , OneMinusSrc1Alpha(_base())
#  pragma pop_macro("OneMinusSrc1Alpha")
# else
	 , OneMinusSrc1Alpha(_base())
# endif
#endif
	{ }
};

} // namespace enums

