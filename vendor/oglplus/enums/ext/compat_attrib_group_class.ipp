//  File implement/oglplus/enums/ext/compat_attrib_group_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_attrib_group.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<CompatibilityAttributeGroup> class Transform>
class EnumToClass<Base, CompatibilityAttributeGroup, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ACCUM_BUFFER_BIT
# if defined AccumBuffer
#  pragma push_macro("AccumBuffer")
#  undef AccumBuffer
	Transform<CompatibilityAttributeGroup::AccumBuffer> AccumBuffer;
#  pragma pop_macro("AccumBuffer")
# else
	Transform<CompatibilityAttributeGroup::AccumBuffer> AccumBuffer;
# endif
#endif
#if defined GL_COLOR_BUFFER_BIT
# if defined ColorBuffer
#  pragma push_macro("ColorBuffer")
#  undef ColorBuffer
	Transform<CompatibilityAttributeGroup::ColorBuffer> ColorBuffer;
#  pragma pop_macro("ColorBuffer")
# else
	Transform<CompatibilityAttributeGroup::ColorBuffer> ColorBuffer;
# endif
#endif
#if defined GL_CURRENT_BIT
# if defined Current
#  pragma push_macro("Current")
#  undef Current
	Transform<CompatibilityAttributeGroup::Current> Current;
#  pragma pop_macro("Current")
# else
	Transform<CompatibilityAttributeGroup::Current> Current;
# endif
#endif
#if defined GL_DEPTH_BUFFER_BIT
# if defined DepthBuffer
#  pragma push_macro("DepthBuffer")
#  undef DepthBuffer
	Transform<CompatibilityAttributeGroup::DepthBuffer> DepthBuffer;
#  pragma pop_macro("DepthBuffer")
# else
	Transform<CompatibilityAttributeGroup::DepthBuffer> DepthBuffer;
# endif
#endif
#if defined GL_ENABLE_BIT
# if defined Enable
#  pragma push_macro("Enable")
#  undef Enable
	Transform<CompatibilityAttributeGroup::Enable> Enable;
#  pragma pop_macro("Enable")
# else
	Transform<CompatibilityAttributeGroup::Enable> Enable;
# endif
#endif
#if defined GL_EVAL_BIT
# if defined Eval
#  pragma push_macro("Eval")
#  undef Eval
	Transform<CompatibilityAttributeGroup::Eval> Eval;
#  pragma pop_macro("Eval")
# else
	Transform<CompatibilityAttributeGroup::Eval> Eval;
# endif
#endif
#if defined GL_FOG_BIT
# if defined Fog
#  pragma push_macro("Fog")
#  undef Fog
	Transform<CompatibilityAttributeGroup::Fog> Fog;
#  pragma pop_macro("Fog")
# else
	Transform<CompatibilityAttributeGroup::Fog> Fog;
# endif
#endif
#if defined GL_HINT_BIT
# if defined Hint
#  pragma push_macro("Hint")
#  undef Hint
	Transform<CompatibilityAttributeGroup::Hint> Hint;
#  pragma pop_macro("Hint")
# else
	Transform<CompatibilityAttributeGroup::Hint> Hint;
# endif
#endif
#if defined GL_LIGHTING_BIT
# if defined Lighting
#  pragma push_macro("Lighting")
#  undef Lighting
	Transform<CompatibilityAttributeGroup::Lighting> Lighting;
#  pragma pop_macro("Lighting")
# else
	Transform<CompatibilityAttributeGroup::Lighting> Lighting;
# endif
#endif
#if defined GL_LINE_BIT
# if defined Line
#  pragma push_macro("Line")
#  undef Line
	Transform<CompatibilityAttributeGroup::Line> Line;
#  pragma pop_macro("Line")
# else
	Transform<CompatibilityAttributeGroup::Line> Line;
# endif
#endif
#if defined GL_LIST_BIT
# if defined List
#  pragma push_macro("List")
#  undef List
	Transform<CompatibilityAttributeGroup::List> List;
#  pragma pop_macro("List")
# else
	Transform<CompatibilityAttributeGroup::List> List;
# endif
#endif
#if defined GL_MULTISAMPLE_BIT
# if defined Multisample
#  pragma push_macro("Multisample")
#  undef Multisample
	Transform<CompatibilityAttributeGroup::Multisample> Multisample;
#  pragma pop_macro("Multisample")
# else
	Transform<CompatibilityAttributeGroup::Multisample> Multisample;
# endif
#endif
#if defined GL_PIXEL_MODE_BIT
# if defined PixelMode
#  pragma push_macro("PixelMode")
#  undef PixelMode
	Transform<CompatibilityAttributeGroup::PixelMode> PixelMode;
#  pragma pop_macro("PixelMode")
# else
	Transform<CompatibilityAttributeGroup::PixelMode> PixelMode;
# endif
#endif
#if defined GL_POINT_BIT
# if defined Point
#  pragma push_macro("Point")
#  undef Point
	Transform<CompatibilityAttributeGroup::Point> Point;
#  pragma pop_macro("Point")
# else
	Transform<CompatibilityAttributeGroup::Point> Point;
# endif
#endif
#if defined GL_POLYGON_BIT
# if defined Polygon
#  pragma push_macro("Polygon")
#  undef Polygon
	Transform<CompatibilityAttributeGroup::Polygon> Polygon;
#  pragma pop_macro("Polygon")
# else
	Transform<CompatibilityAttributeGroup::Polygon> Polygon;
# endif
#endif
#if defined GL_POLYGON_STIPPLE_BIT
# if defined PolygonStipple
#  pragma push_macro("PolygonStipple")
#  undef PolygonStipple
	Transform<CompatibilityAttributeGroup::PolygonStipple> PolygonStipple;
#  pragma pop_macro("PolygonStipple")
# else
	Transform<CompatibilityAttributeGroup::PolygonStipple> PolygonStipple;
# endif
#endif
#if defined GL_SCISSOR_BIT
# if defined Scissor
#  pragma push_macro("Scissor")
#  undef Scissor
	Transform<CompatibilityAttributeGroup::Scissor> Scissor;
#  pragma pop_macro("Scissor")
# else
	Transform<CompatibilityAttributeGroup::Scissor> Scissor;
# endif
#endif
#if defined GL_STENCIL_BUFFER_BIT
# if defined StencilBuffer
#  pragma push_macro("StencilBuffer")
#  undef StencilBuffer
	Transform<CompatibilityAttributeGroup::StencilBuffer> StencilBuffer;
#  pragma pop_macro("StencilBuffer")
# else
	Transform<CompatibilityAttributeGroup::StencilBuffer> StencilBuffer;
# endif
#endif
#if defined GL_TEXTURE_BIT
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	Transform<CompatibilityAttributeGroup::Texture> Texture;
#  pragma pop_macro("Texture")
# else
	Transform<CompatibilityAttributeGroup::Texture> Texture;
# endif
#endif
#if defined GL_TRANSFORM_BIT
# if defined Transform
#  pragma push_macro("Transform")
#  undef Transform
	Transform<CompatibilityAttributeGroup::Transform> Transform;
#  pragma pop_macro("Transform")
# else
	Transform<CompatibilityAttributeGroup::Transform> Transform;
# endif
#endif
#if defined GL_VIEWPORT_BIT
# if defined Viewport
#  pragma push_macro("Viewport")
#  undef Viewport
	Transform<CompatibilityAttributeGroup::Viewport> Viewport;
#  pragma pop_macro("Viewport")
# else
	Transform<CompatibilityAttributeGroup::Viewport> Viewport;
# endif
#endif
#if defined GL_ALL_ATTRIB_BITS
# if defined AllAttribs
#  pragma push_macro("AllAttribs")
#  undef AllAttribs
	Transform<CompatibilityAttributeGroup::AllAttribs> AllAttribs;
#  pragma pop_macro("AllAttribs")
# else
	Transform<CompatibilityAttributeGroup::AllAttribs> AllAttribs;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_ACCUM_BUFFER_BIT
# if defined AccumBuffer
#  pragma push_macro("AccumBuffer")
#  undef AccumBuffer
	 , AccumBuffer(_base())
#  pragma pop_macro("AccumBuffer")
# else
	 , AccumBuffer(_base())
# endif
#endif
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
#if defined GL_CURRENT_BIT
# if defined Current
#  pragma push_macro("Current")
#  undef Current
	 , Current(_base())
#  pragma pop_macro("Current")
# else
	 , Current(_base())
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
#if defined GL_ENABLE_BIT
# if defined Enable
#  pragma push_macro("Enable")
#  undef Enable
	 , Enable(_base())
#  pragma pop_macro("Enable")
# else
	 , Enable(_base())
# endif
#endif
#if defined GL_EVAL_BIT
# if defined Eval
#  pragma push_macro("Eval")
#  undef Eval
	 , Eval(_base())
#  pragma pop_macro("Eval")
# else
	 , Eval(_base())
# endif
#endif
#if defined GL_FOG_BIT
# if defined Fog
#  pragma push_macro("Fog")
#  undef Fog
	 , Fog(_base())
#  pragma pop_macro("Fog")
# else
	 , Fog(_base())
# endif
#endif
#if defined GL_HINT_BIT
# if defined Hint
#  pragma push_macro("Hint")
#  undef Hint
	 , Hint(_base())
#  pragma pop_macro("Hint")
# else
	 , Hint(_base())
# endif
#endif
#if defined GL_LIGHTING_BIT
# if defined Lighting
#  pragma push_macro("Lighting")
#  undef Lighting
	 , Lighting(_base())
#  pragma pop_macro("Lighting")
# else
	 , Lighting(_base())
# endif
#endif
#if defined GL_LINE_BIT
# if defined Line
#  pragma push_macro("Line")
#  undef Line
	 , Line(_base())
#  pragma pop_macro("Line")
# else
	 , Line(_base())
# endif
#endif
#if defined GL_LIST_BIT
# if defined List
#  pragma push_macro("List")
#  undef List
	 , List(_base())
#  pragma pop_macro("List")
# else
	 , List(_base())
# endif
#endif
#if defined GL_MULTISAMPLE_BIT
# if defined Multisample
#  pragma push_macro("Multisample")
#  undef Multisample
	 , Multisample(_base())
#  pragma pop_macro("Multisample")
# else
	 , Multisample(_base())
# endif
#endif
#if defined GL_PIXEL_MODE_BIT
# if defined PixelMode
#  pragma push_macro("PixelMode")
#  undef PixelMode
	 , PixelMode(_base())
#  pragma pop_macro("PixelMode")
# else
	 , PixelMode(_base())
# endif
#endif
#if defined GL_POINT_BIT
# if defined Point
#  pragma push_macro("Point")
#  undef Point
	 , Point(_base())
#  pragma pop_macro("Point")
# else
	 , Point(_base())
# endif
#endif
#if defined GL_POLYGON_BIT
# if defined Polygon
#  pragma push_macro("Polygon")
#  undef Polygon
	 , Polygon(_base())
#  pragma pop_macro("Polygon")
# else
	 , Polygon(_base())
# endif
#endif
#if defined GL_POLYGON_STIPPLE_BIT
# if defined PolygonStipple
#  pragma push_macro("PolygonStipple")
#  undef PolygonStipple
	 , PolygonStipple(_base())
#  pragma pop_macro("PolygonStipple")
# else
	 , PolygonStipple(_base())
# endif
#endif
#if defined GL_SCISSOR_BIT
# if defined Scissor
#  pragma push_macro("Scissor")
#  undef Scissor
	 , Scissor(_base())
#  pragma pop_macro("Scissor")
# else
	 , Scissor(_base())
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
#if defined GL_TEXTURE_BIT
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	 , Texture(_base())
#  pragma pop_macro("Texture")
# else
	 , Texture(_base())
# endif
#endif
#if defined GL_TRANSFORM_BIT
# if defined Transform
#  pragma push_macro("Transform")
#  undef Transform
	 , Transform(_base())
#  pragma pop_macro("Transform")
# else
	 , Transform(_base())
# endif
#endif
#if defined GL_VIEWPORT_BIT
# if defined Viewport
#  pragma push_macro("Viewport")
#  undef Viewport
	 , Viewport(_base())
#  pragma pop_macro("Viewport")
# else
	 , Viewport(_base())
# endif
#endif
#if defined GL_ALL_ATTRIB_BITS
# if defined AllAttribs
#  pragma push_macro("AllAttribs")
#  undef AllAttribs
	 , AllAttribs(_base())
#  pragma pop_macro("AllAttribs")
# else
	 , AllAttribs(_base())
# endif
#endif
	{ }
};

} // namespace enums

