//  File implement/oglplus/enums/capability_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/capability.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<Capability> class Transform>
class EnumToClass<Base, Capability, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PRIMITIVE_RESTART
# if defined PrimitiveRestart
#  pragma push_macro("PrimitiveRestart")
#  undef PrimitiveRestart
	Transform<Capability::PrimitiveRestart> PrimitiveRestart;
#  pragma pop_macro("PrimitiveRestart")
# else
	Transform<Capability::PrimitiveRestart> PrimitiveRestart;
# endif
#endif
#if defined GL_DEPTH_TEST
# if defined DepthTest
#  pragma push_macro("DepthTest")
#  undef DepthTest
	Transform<Capability::DepthTest> DepthTest;
#  pragma pop_macro("DepthTest")
# else
	Transform<Capability::DepthTest> DepthTest;
# endif
#endif
#if defined GL_STENCIL_TEST
# if defined StencilTest
#  pragma push_macro("StencilTest")
#  undef StencilTest
	Transform<Capability::StencilTest> StencilTest;
#  pragma pop_macro("StencilTest")
# else
	Transform<Capability::StencilTest> StencilTest;
# endif
#endif
#if defined GL_SCISSOR_TEST
# if defined ScissorTest
#  pragma push_macro("ScissorTest")
#  undef ScissorTest
	Transform<Capability::ScissorTest> ScissorTest;
#  pragma pop_macro("ScissorTest")
# else
	Transform<Capability::ScissorTest> ScissorTest;
# endif
#endif
#if defined GL_CULL_FACE
# if defined CullFace
#  pragma push_macro("CullFace")
#  undef CullFace
	Transform<Capability::CullFace> CullFace;
#  pragma pop_macro("CullFace")
# else
	Transform<Capability::CullFace> CullFace;
# endif
#endif
#if defined GL_RASTERIZER_DISCARD
# if defined RasterizerDiscard
#  pragma push_macro("RasterizerDiscard")
#  undef RasterizerDiscard
	Transform<Capability::RasterizerDiscard> RasterizerDiscard;
#  pragma pop_macro("RasterizerDiscard")
# else
	Transform<Capability::RasterizerDiscard> RasterizerDiscard;
# endif
#endif
#if defined GL_POLYGON_OFFSET_POINT
# if defined PolygonOffsetPoint
#  pragma push_macro("PolygonOffsetPoint")
#  undef PolygonOffsetPoint
	Transform<Capability::PolygonOffsetPoint> PolygonOffsetPoint;
#  pragma pop_macro("PolygonOffsetPoint")
# else
	Transform<Capability::PolygonOffsetPoint> PolygonOffsetPoint;
# endif
#endif
#if defined GL_POLYGON_OFFSET_LINE
# if defined PolygonOffsetLine
#  pragma push_macro("PolygonOffsetLine")
#  undef PolygonOffsetLine
	Transform<Capability::PolygonOffsetLine> PolygonOffsetLine;
#  pragma pop_macro("PolygonOffsetLine")
# else
	Transform<Capability::PolygonOffsetLine> PolygonOffsetLine;
# endif
#endif
#if defined GL_POLYGON_OFFSET_FILL
# if defined PolygonOffsetFill
#  pragma push_macro("PolygonOffsetFill")
#  undef PolygonOffsetFill
	Transform<Capability::PolygonOffsetFill> PolygonOffsetFill;
#  pragma pop_macro("PolygonOffsetFill")
# else
	Transform<Capability::PolygonOffsetFill> PolygonOffsetFill;
# endif
#endif
#if defined GL_BLEND
# if defined Blend
#  pragma push_macro("Blend")
#  undef Blend
	Transform<Capability::Blend> Blend;
#  pragma pop_macro("Blend")
# else
	Transform<Capability::Blend> Blend;
# endif
#endif
#if defined GL_COLOR_LOGIC_OP
# if defined ColorLogicOp
#  pragma push_macro("ColorLogicOp")
#  undef ColorLogicOp
	Transform<Capability::ColorLogicOp> ColorLogicOp;
#  pragma pop_macro("ColorLogicOp")
# else
	Transform<Capability::ColorLogicOp> ColorLogicOp;
# endif
#endif
#if defined GL_DITHER
# if defined Dither
#  pragma push_macro("Dither")
#  undef Dither
	Transform<Capability::Dither> Dither;
#  pragma pop_macro("Dither")
# else
	Transform<Capability::Dither> Dither;
# endif
#endif
#if defined GL_MULTISAMPLE
# if defined Multisample
#  pragma push_macro("Multisample")
#  undef Multisample
	Transform<Capability::Multisample> Multisample;
#  pragma pop_macro("Multisample")
# else
	Transform<Capability::Multisample> Multisample;
# endif
#endif
#if defined GL_SAMPLE_SHADING
# if defined SampleShading
#  pragma push_macro("SampleShading")
#  undef SampleShading
	Transform<Capability::SampleShading> SampleShading;
#  pragma pop_macro("SampleShading")
# else
	Transform<Capability::SampleShading> SampleShading;
# endif
#endif
#if defined GL_LINE_SMOOTH
# if defined LineSmooth
#  pragma push_macro("LineSmooth")
#  undef LineSmooth
	Transform<Capability::LineSmooth> LineSmooth;
#  pragma pop_macro("LineSmooth")
# else
	Transform<Capability::LineSmooth> LineSmooth;
# endif
#endif
#if defined GL_POLYGON_SMOOTH
# if defined PolygonSmooth
#  pragma push_macro("PolygonSmooth")
#  undef PolygonSmooth
	Transform<Capability::PolygonSmooth> PolygonSmooth;
#  pragma pop_macro("PolygonSmooth")
# else
	Transform<Capability::PolygonSmooth> PolygonSmooth;
# endif
#endif
#if defined GL_PROGRAM_POINT_SIZE
# if defined ProgramPointSize
#  pragma push_macro("ProgramPointSize")
#  undef ProgramPointSize
	Transform<Capability::ProgramPointSize> ProgramPointSize;
#  pragma pop_macro("ProgramPointSize")
# else
	Transform<Capability::ProgramPointSize> ProgramPointSize;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_SEAMLESS
# if defined TextureCubeMapSeamless
#  pragma push_macro("TextureCubeMapSeamless")
#  undef TextureCubeMapSeamless
	Transform<Capability::TextureCubeMapSeamless> TextureCubeMapSeamless;
#  pragma pop_macro("TextureCubeMapSeamless")
# else
	Transform<Capability::TextureCubeMapSeamless> TextureCubeMapSeamless;
# endif
#endif
#if defined GL_SAMPLE_ALPHA_TO_COVERAGE
# if defined SampleAlphaToCoverage
#  pragma push_macro("SampleAlphaToCoverage")
#  undef SampleAlphaToCoverage
	Transform<Capability::SampleAlphaToCoverage> SampleAlphaToCoverage;
#  pragma pop_macro("SampleAlphaToCoverage")
# else
	Transform<Capability::SampleAlphaToCoverage> SampleAlphaToCoverage;
# endif
#endif
#if defined GL_SAMPLE_ALPHA_TO_ONE
# if defined SampleAlphaToOne
#  pragma push_macro("SampleAlphaToOne")
#  undef SampleAlphaToOne
	Transform<Capability::SampleAlphaToOne> SampleAlphaToOne;
#  pragma pop_macro("SampleAlphaToOne")
# else
	Transform<Capability::SampleAlphaToOne> SampleAlphaToOne;
# endif
#endif
#if defined GL_SAMPLE_COVERAGE
# if defined SampleCoverage
#  pragma push_macro("SampleCoverage")
#  undef SampleCoverage
	Transform<Capability::SampleCoverage> SampleCoverage;
#  pragma pop_macro("SampleCoverage")
# else
	Transform<Capability::SampleCoverage> SampleCoverage;
# endif
#endif
#if defined GL_SAMPLE_MASK
# if defined SampleMask
#  pragma push_macro("SampleMask")
#  undef SampleMask
	Transform<Capability::SampleMask> SampleMask;
#  pragma pop_macro("SampleMask")
# else
	Transform<Capability::SampleMask> SampleMask;
# endif
#endif
#if defined GL_FRAMEBUFFER_SRGB
# if defined FramebufferSRGB
#  pragma push_macro("FramebufferSRGB")
#  undef FramebufferSRGB
	Transform<Capability::FramebufferSRGB> FramebufferSRGB;
#  pragma pop_macro("FramebufferSRGB")
# else
	Transform<Capability::FramebufferSRGB> FramebufferSRGB;
# endif
#endif
#if defined GL_DEBUG_OUTPUT_SYNCHRONOUS
# if defined DebugOutputSynchronous
#  pragma push_macro("DebugOutputSynchronous")
#  undef DebugOutputSynchronous
	Transform<Capability::DebugOutputSynchronous> DebugOutputSynchronous;
#  pragma pop_macro("DebugOutputSynchronous")
# else
	Transform<Capability::DebugOutputSynchronous> DebugOutputSynchronous;
# endif
#endif
#if defined GL_STREAM_RASTERIZATION_AMD
# if defined StreamRasterization
#  pragma push_macro("StreamRasterization")
#  undef StreamRasterization
	Transform<Capability::StreamRasterization> StreamRasterization;
#  pragma pop_macro("StreamRasterization")
# else
	Transform<Capability::StreamRasterization> StreamRasterization;
# endif
#endif
#if defined GL_BLEND_ADVANCED_COHERENT_KHR
# if defined BlendAdvancedCoherent
#  pragma push_macro("BlendAdvancedCoherent")
#  undef BlendAdvancedCoherent
	Transform<Capability::BlendAdvancedCoherent> BlendAdvancedCoherent;
#  pragma pop_macro("BlendAdvancedCoherent")
# else
	Transform<Capability::BlendAdvancedCoherent> BlendAdvancedCoherent;
# endif
#endif
#if defined GL_FRAGMENT_COVERAGE_TO_COLOR_NV
# if defined FragmentCoverageToColor
#  pragma push_macro("FragmentCoverageToColor")
#  undef FragmentCoverageToColor
	Transform<Capability::FragmentCoverageToColor> FragmentCoverageToColor;
#  pragma pop_macro("FragmentCoverageToColor")
# else
	Transform<Capability::FragmentCoverageToColor> FragmentCoverageToColor;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PRIMITIVE_RESTART
# if defined PrimitiveRestart
#  pragma push_macro("PrimitiveRestart")
#  undef PrimitiveRestart
	 , PrimitiveRestart(_base())
#  pragma pop_macro("PrimitiveRestart")
# else
	 , PrimitiveRestart(_base())
# endif
#endif
#if defined GL_DEPTH_TEST
# if defined DepthTest
#  pragma push_macro("DepthTest")
#  undef DepthTest
	 , DepthTest(_base())
#  pragma pop_macro("DepthTest")
# else
	 , DepthTest(_base())
# endif
#endif
#if defined GL_STENCIL_TEST
# if defined StencilTest
#  pragma push_macro("StencilTest")
#  undef StencilTest
	 , StencilTest(_base())
#  pragma pop_macro("StencilTest")
# else
	 , StencilTest(_base())
# endif
#endif
#if defined GL_SCISSOR_TEST
# if defined ScissorTest
#  pragma push_macro("ScissorTest")
#  undef ScissorTest
	 , ScissorTest(_base())
#  pragma pop_macro("ScissorTest")
# else
	 , ScissorTest(_base())
# endif
#endif
#if defined GL_CULL_FACE
# if defined CullFace
#  pragma push_macro("CullFace")
#  undef CullFace
	 , CullFace(_base())
#  pragma pop_macro("CullFace")
# else
	 , CullFace(_base())
# endif
#endif
#if defined GL_RASTERIZER_DISCARD
# if defined RasterizerDiscard
#  pragma push_macro("RasterizerDiscard")
#  undef RasterizerDiscard
	 , RasterizerDiscard(_base())
#  pragma pop_macro("RasterizerDiscard")
# else
	 , RasterizerDiscard(_base())
# endif
#endif
#if defined GL_POLYGON_OFFSET_POINT
# if defined PolygonOffsetPoint
#  pragma push_macro("PolygonOffsetPoint")
#  undef PolygonOffsetPoint
	 , PolygonOffsetPoint(_base())
#  pragma pop_macro("PolygonOffsetPoint")
# else
	 , PolygonOffsetPoint(_base())
# endif
#endif
#if defined GL_POLYGON_OFFSET_LINE
# if defined PolygonOffsetLine
#  pragma push_macro("PolygonOffsetLine")
#  undef PolygonOffsetLine
	 , PolygonOffsetLine(_base())
#  pragma pop_macro("PolygonOffsetLine")
# else
	 , PolygonOffsetLine(_base())
# endif
#endif
#if defined GL_POLYGON_OFFSET_FILL
# if defined PolygonOffsetFill
#  pragma push_macro("PolygonOffsetFill")
#  undef PolygonOffsetFill
	 , PolygonOffsetFill(_base())
#  pragma pop_macro("PolygonOffsetFill")
# else
	 , PolygonOffsetFill(_base())
# endif
#endif
#if defined GL_BLEND
# if defined Blend
#  pragma push_macro("Blend")
#  undef Blend
	 , Blend(_base())
#  pragma pop_macro("Blend")
# else
	 , Blend(_base())
# endif
#endif
#if defined GL_COLOR_LOGIC_OP
# if defined ColorLogicOp
#  pragma push_macro("ColorLogicOp")
#  undef ColorLogicOp
	 , ColorLogicOp(_base())
#  pragma pop_macro("ColorLogicOp")
# else
	 , ColorLogicOp(_base())
# endif
#endif
#if defined GL_DITHER
# if defined Dither
#  pragma push_macro("Dither")
#  undef Dither
	 , Dither(_base())
#  pragma pop_macro("Dither")
# else
	 , Dither(_base())
# endif
#endif
#if defined GL_MULTISAMPLE
# if defined Multisample
#  pragma push_macro("Multisample")
#  undef Multisample
	 , Multisample(_base())
#  pragma pop_macro("Multisample")
# else
	 , Multisample(_base())
# endif
#endif
#if defined GL_SAMPLE_SHADING
# if defined SampleShading
#  pragma push_macro("SampleShading")
#  undef SampleShading
	 , SampleShading(_base())
#  pragma pop_macro("SampleShading")
# else
	 , SampleShading(_base())
# endif
#endif
#if defined GL_LINE_SMOOTH
# if defined LineSmooth
#  pragma push_macro("LineSmooth")
#  undef LineSmooth
	 , LineSmooth(_base())
#  pragma pop_macro("LineSmooth")
# else
	 , LineSmooth(_base())
# endif
#endif
#if defined GL_POLYGON_SMOOTH
# if defined PolygonSmooth
#  pragma push_macro("PolygonSmooth")
#  undef PolygonSmooth
	 , PolygonSmooth(_base())
#  pragma pop_macro("PolygonSmooth")
# else
	 , PolygonSmooth(_base())
# endif
#endif
#if defined GL_PROGRAM_POINT_SIZE
# if defined ProgramPointSize
#  pragma push_macro("ProgramPointSize")
#  undef ProgramPointSize
	 , ProgramPointSize(_base())
#  pragma pop_macro("ProgramPointSize")
# else
	 , ProgramPointSize(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_SEAMLESS
# if defined TextureCubeMapSeamless
#  pragma push_macro("TextureCubeMapSeamless")
#  undef TextureCubeMapSeamless
	 , TextureCubeMapSeamless(_base())
#  pragma pop_macro("TextureCubeMapSeamless")
# else
	 , TextureCubeMapSeamless(_base())
# endif
#endif
#if defined GL_SAMPLE_ALPHA_TO_COVERAGE
# if defined SampleAlphaToCoverage
#  pragma push_macro("SampleAlphaToCoverage")
#  undef SampleAlphaToCoverage
	 , SampleAlphaToCoverage(_base())
#  pragma pop_macro("SampleAlphaToCoverage")
# else
	 , SampleAlphaToCoverage(_base())
# endif
#endif
#if defined GL_SAMPLE_ALPHA_TO_ONE
# if defined SampleAlphaToOne
#  pragma push_macro("SampleAlphaToOne")
#  undef SampleAlphaToOne
	 , SampleAlphaToOne(_base())
#  pragma pop_macro("SampleAlphaToOne")
# else
	 , SampleAlphaToOne(_base())
# endif
#endif
#if defined GL_SAMPLE_COVERAGE
# if defined SampleCoverage
#  pragma push_macro("SampleCoverage")
#  undef SampleCoverage
	 , SampleCoverage(_base())
#  pragma pop_macro("SampleCoverage")
# else
	 , SampleCoverage(_base())
# endif
#endif
#if defined GL_SAMPLE_MASK
# if defined SampleMask
#  pragma push_macro("SampleMask")
#  undef SampleMask
	 , SampleMask(_base())
#  pragma pop_macro("SampleMask")
# else
	 , SampleMask(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_SRGB
# if defined FramebufferSRGB
#  pragma push_macro("FramebufferSRGB")
#  undef FramebufferSRGB
	 , FramebufferSRGB(_base())
#  pragma pop_macro("FramebufferSRGB")
# else
	 , FramebufferSRGB(_base())
# endif
#endif
#if defined GL_DEBUG_OUTPUT_SYNCHRONOUS
# if defined DebugOutputSynchronous
#  pragma push_macro("DebugOutputSynchronous")
#  undef DebugOutputSynchronous
	 , DebugOutputSynchronous(_base())
#  pragma pop_macro("DebugOutputSynchronous")
# else
	 , DebugOutputSynchronous(_base())
# endif
#endif
#if defined GL_STREAM_RASTERIZATION_AMD
# if defined StreamRasterization
#  pragma push_macro("StreamRasterization")
#  undef StreamRasterization
	 , StreamRasterization(_base())
#  pragma pop_macro("StreamRasterization")
# else
	 , StreamRasterization(_base())
# endif
#endif
#if defined GL_BLEND_ADVANCED_COHERENT_KHR
# if defined BlendAdvancedCoherent
#  pragma push_macro("BlendAdvancedCoherent")
#  undef BlendAdvancedCoherent
	 , BlendAdvancedCoherent(_base())
#  pragma pop_macro("BlendAdvancedCoherent")
# else
	 , BlendAdvancedCoherent(_base())
# endif
#endif
#if defined GL_FRAGMENT_COVERAGE_TO_COLOR_NV
# if defined FragmentCoverageToColor
#  pragma push_macro("FragmentCoverageToColor")
#  undef FragmentCoverageToColor
	 , FragmentCoverageToColor(_base())
#  pragma pop_macro("FragmentCoverageToColor")
# else
	 , FragmentCoverageToColor(_base())
# endif
#endif
	{ }
};

} // namespace enums

