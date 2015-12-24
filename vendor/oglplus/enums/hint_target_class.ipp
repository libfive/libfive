//  File implement/oglplus/enums/hint_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/hint_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<HintTarget> class Transform>
class EnumToClass<Base, HintTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_LINE_SMOOTH_HINT
# if defined LineSmooth
#  pragma push_macro("LineSmooth")
#  undef LineSmooth
	Transform<HintTarget::LineSmooth> LineSmooth;
#  pragma pop_macro("LineSmooth")
# else
	Transform<HintTarget::LineSmooth> LineSmooth;
# endif
#endif
#if defined GL_POLYGON_SMOOTH_HINT
# if defined PolygonSmooth
#  pragma push_macro("PolygonSmooth")
#  undef PolygonSmooth
	Transform<HintTarget::PolygonSmooth> PolygonSmooth;
#  pragma pop_macro("PolygonSmooth")
# else
	Transform<HintTarget::PolygonSmooth> PolygonSmooth;
# endif
#endif
#if defined GL_TEXTURE_COMPRESSION_HINT
# if defined TextureCompression
#  pragma push_macro("TextureCompression")
#  undef TextureCompression
	Transform<HintTarget::TextureCompression> TextureCompression;
#  pragma pop_macro("TextureCompression")
# else
	Transform<HintTarget::TextureCompression> TextureCompression;
# endif
#endif
#if defined GL_FRAGMENT_SHADER_DERIVATIVE_HINT
# if defined FragmentShaderDerivative
#  pragma push_macro("FragmentShaderDerivative")
#  undef FragmentShaderDerivative
	Transform<HintTarget::FragmentShaderDerivative> FragmentShaderDerivative;
#  pragma pop_macro("FragmentShaderDerivative")
# else
	Transform<HintTarget::FragmentShaderDerivative> FragmentShaderDerivative;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_LINE_SMOOTH_HINT
# if defined LineSmooth
#  pragma push_macro("LineSmooth")
#  undef LineSmooth
	 , LineSmooth(_base())
#  pragma pop_macro("LineSmooth")
# else
	 , LineSmooth(_base())
# endif
#endif
#if defined GL_POLYGON_SMOOTH_HINT
# if defined PolygonSmooth
#  pragma push_macro("PolygonSmooth")
#  undef PolygonSmooth
	 , PolygonSmooth(_base())
#  pragma pop_macro("PolygonSmooth")
# else
	 , PolygonSmooth(_base())
# endif
#endif
#if defined GL_TEXTURE_COMPRESSION_HINT
# if defined TextureCompression
#  pragma push_macro("TextureCompression")
#  undef TextureCompression
	 , TextureCompression(_base())
#  pragma pop_macro("TextureCompression")
# else
	 , TextureCompression(_base())
# endif
#endif
#if defined GL_FRAGMENT_SHADER_DERIVATIVE_HINT
# if defined FragmentShaderDerivative
#  pragma push_macro("FragmentShaderDerivative")
#  undef FragmentShaderDerivative
	 , FragmentShaderDerivative(_base())
#  pragma pop_macro("FragmentShaderDerivative")
# else
	 , FragmentShaderDerivative(_base())
# endif
#endif
	{ }
};

} // namespace enums

