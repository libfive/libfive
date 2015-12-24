/**
 *  @file oglplus/text/bitmap_glyph/renderer.ipp
 *  @brief Implementation of Bitmap-font-based text renderer
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
BitmapGlyphRenderer::BitmapGlyphRenderer(
	BitmapGlyphRenderingBase& parent,
	const Sequence<ShaderName>& shaders
): _parent(parent)
 , _program(ObjectDesc("BitmapGlyphRenderer"))
 , _bitmap_sampler(_program)
 , _metric_sampler(_program)
 , _pg_map_sampler(_program)
 , _layout_width(_program)
 , _layout_width_active(false)
 , _prev_font_essence(nullptr)
 , _prev_layout_storage(nullptr)
{
	VertexShader vs(ObjectDesc("BitmapGlyphRenderer - Vertex"));
	vs.Source(StrCRef(
		"#if GL_ARB_explicit_attrib_location\n"
		"#version 150\n"
		"#extension GL_ARB_explicit_attrib_location : enable\n"
		"#else\n"
		"#version 330\n"
		"#endif\n"

		"uniform uint GlyphsPerPage;"

		"uniform sampler2DRect oglpMetric;"
		"uniform usamplerBuffer oglpPageMap;"

		"layout (location = 0) in uint CodePoint;"
		"layout (location = 1) in float XOffset;"

		"out vec4 vertLogData;"
		"out vec4 vertInkData;"
		"out vec4 vertTexData;"
		"out float vertXOffset;"
		"out float vertFrame;"

		"void main(void)"
		"{"
		"	int goffs = int(CodePoint % GlyphsPerPage)*3;"
		"	int page =  int(CodePoint / GlyphsPerPage);"
		"	int frame = int(texelFetch(oglpPageMap,page).r);"

		"	vertLogData = texelFetch("
		"		oglpMetric,"
		"		ivec2(goffs+0, frame)"
		"	);"
		"	vertInkData = texelFetch("
		"		oglpMetric,"
		"		ivec2(goffs+1, frame)"
		"	);"
		"	vertTexData = texelFetch("
		"		oglpMetric,"
		"		ivec2(goffs+2, frame)"
		"	);"
		"	vertXOffset = XOffset;"
		"	vertFrame = float(frame);"
		"}"
	));
	vs.Compile();
	_program.AttachShader(vs);

	GeometryShader gs(ObjectDesc("BitmapGlyphRenderer - Geometry"));
	gs.Source(StrCRef(
		"#version 150\n"
		"layout (points) in;"
		"layout (triangle_strip, max_vertices = 6) out;"

		"vec3 TransformGlyph("
		"	vec4 LogMetrics,"
		"	vec4 InkMetrics,"
		"	vec2 Position,"
		"	float XOffset,"
		"	float LayoutWidth,"
		"	int Index"
		");"

		"vec4 TransformLayout(vec3 GlyphPosition);"

		"in vec4 vertLogData[1];"
		"in vec4 vertInkData[1];"
		"in vec4 vertTexData[1];"
		"in float vertXOffset[1];"
		"in float vertFrame[1];"

		"out vec4 geomGlyphPos;"
		"out vec4 geomGlyphCoord;"
		"out vec3 geomTexCoord;"

		"uniform float oglpLayoutWidth;"

		"void make_vertex(vec2 Position, vec2 GlyphCoord, vec2 TexCoord)"
		"{"
		"	geomGlyphPos = vec4(TransformGlyph("
		"		vertLogData[0],"
		"		vertInkData[0],"
		"		Position,"
		"		vertXOffset[0],"
		"		oglpLayoutWidth,"
		"		int(gl_PrimitiveIDIn)"
		"	), vertXOffset[0]);"
		"	gl_Position = TransformLayout(geomGlyphPos.xyz);"
		"	geomGlyphCoord = vec4(Position, GlyphCoord);"
		"	geomTexCoord = vec3(TexCoord, vertFrame[0]);"
		"	EmitVertex();"
		"}"

		"void main(void)"
		"{"
		//      left bearing
		"       float lb = vertInkData[0].x;"
		//      right bearing
		"       float rb = vertInkData[0].y;"
		//      ascender
		"       float as = vertInkData[0].z;"
		//      descender
		"       float ds = vertInkData[0].w;"
		//      height
		"       float ht = as + ds;"
		//      glyph origin in texture space
		"       vec2  to = vertTexData[0].xy;"
		//      glyph width in texture space
		"       float tw = vertTexData[0].z;"
		//      glyph height in texture space
		"       float th = vertTexData[0].w;"
		//      glyph ascent in texture space
		"       float ta = th * (as / ht);"
		//      glyph descent in texture space
		"       float td = th * (ds / ht);"

		"       make_vertex(vec2(rb,-ds), vec2(1.0,-1.0), to+vec2( tw, -td));"
		"       make_vertex(vec2(lb,-ds), vec2(0.0,-1.0), to+vec2(0.0, -td));"
		"       make_vertex(vec2(rb,0.0), vec2(1.0, 0.0), to+vec2( tw, 0.0));"
		"       make_vertex(vec2(lb,0.0), vec2(0.0, 0.0), to+vec2(0.0, 0.0));"
		"       make_vertex(vec2(rb, as), vec2(1.0, 1.0), to+vec2( tw,  ta));"
		"       make_vertex(vec2(lb, as), vec2(0.0, 1.0), to+vec2(0.0,  ta));"
		"       EndPrimitive();"
		"}"
	));
	gs.Compile();
	_program.AttachShader(gs);

	FragmentShader fs(ObjectDesc("BitmapGlyphRenderer - Fragment"));
	fs.Source(StrCRef(
		"#version 150\n"
		"uniform sampler2DArray oglpBitmap;"

		"uniform float oglpLayoutWidth;"

		"in vec4 geomGlyphPos;"
		"in vec4 geomGlyphCoord;"
		"in vec3 geomTexCoord;"

		"out vec4 fragColor;"

		"vec4 PixelColor("
		"	vec4 TexelColor,"
		"	vec3 GlyphPosition,"
		"	float GlyphXOffset,"
		"	vec2 GlyphExtent,"
		"	vec2 GlyphCoord,"
		"	float LayoutWidth"
		");"

		"void main(void)"
		"{"
		"       fragColor = PixelColor("
		"		texture(oglpBitmap, geomTexCoord),"
		"		geomGlyphPos.xyz,"
		"		geomGlyphPos.w,"
		"		geomGlyphCoord.xy,"
		"		geomGlyphCoord.zw,"
		"		oglpLayoutWidth"
		"	);"
		"}"

	));
	fs.Compile();
	_program.AttachShader(fs);
	_program.AttachShaders(shaders);

	_program.Link();

	_bitmap_sampler.BindTo("oglpBitmap");
	_metric_sampler.BindTo("oglpMetric");
	_pg_map_sampler.BindTo("oglpPageMap");
	_layout_width.BindTo("oglpLayoutWidth");

	ProgramUniform<GLuint>(_program, "GlyphsPerPage").Set(
		BitmapGlyphGlyphsPerPage(_parent)
	);
	_layout_width_active = _layout_width.IsActive();
}

#if OGLPLUS_NO_DEFAULTED_FUNCTIONS
OGLPLUS_LIB_FUNC
BitmapGlyphRenderer::BitmapGlyphRenderer(BitmapGlyphRenderer&& tmp)
 : _parent(tmp._parent)
 , _program(std::move(tmp._program))
 , _bitmap_sampler(std::move(tmp._bitmap_sampler))
 , _metric_sampler(std::move(tmp._metric_sampler))
 , _pg_map_sampler(std::move(tmp._pg_map_sampler))
 , _layout_width(std::move(tmp._layout_width))
 , _layout_width_active(std::move(tmp._layout_width_active))
 , _prev_font_essence(std::move(tmp._prev_font_essence))
 , _prev_layout_storage(std::move(tmp._prev_layout_storage))
{ }
#endif


OGLPLUS_LIB_FUNC
BitmapGlyphDefaultRenderer::BitmapGlyphDefaultRenderer(
	BitmapGlyphRenderingBase& parent,
	const FragmentShader& pixel_color_shader
): DefaultRendererTpl<BitmapGlyphRenderer>(
	parent,
	StaticGroup<ShaderName, 3>(
		GeometryShader(
			ObjectDesc("BitmapGlyphRenderer - Layout transform"),
			StrCRef(
			"#version 150\n"
			"uniform mat4 "
			"	oglpProjectionMatrix,"
			"	oglpCameraMatrix,"
			"	oglpLayoutMatrix;"
			"mat4 Matrix = "
			"	oglpProjectionMatrix*"
			"	oglpCameraMatrix*"
			"	oglpLayoutMatrix;"

			"vec4 TransformLayout(vec3 GlyphPosition)"
			"{"
			"	return Matrix * vec4(GlyphPosition, 1.0);"
			"}")
		),
		GeometryShader(
			ObjectDesc("BitmapGlyphRenderer - Glyph transform"),
			StrCRef(
			"#version 150\n"
			"uniform float oglpAlignCoef;"
			"uniform float oglpDirCoef;"

			"float oglpAlignCoef2 = 0.5*oglpDirCoef-oglpAlignCoef;"
			"float oglpDirCoef2 = min(oglpDirCoef, 0.0);"

			"vec3 TransformGlyph("
			"	vec4 LogMetrics,"
			"	vec4 InkMetrics,"
			"	vec2 Pos,"
			"	float XOffs,"
			"	float LayoutWidth,"
			"	int Idx"
			")"
			"{"
			"	float LogWidth = LogMetrics.y - LogMetrics.x;"
			"	XOffs = oglpDirCoef * XOffs+"
			"		oglpDirCoef2* LogWidth-"
			"		oglpAlignCoef2*LayoutWidth;"
			"	return vec3("
			"		Pos.x+XOffs,"
			"		Pos.y,"
			"		0.0"
			"	);"
			"}")
		),
		pixel_color_shader
	)
)
{ }

#if OGLPLUS_NO_DEFAULTED_FUNCTIONS
OGLPLUS_LIB_FUNC
BitmapGlyphDefaultRenderer::BitmapGlyphDefaultRenderer(
	BitmapGlyphDefaultRenderer&& tmp
): DefaultRendererTpl<BitmapGlyphRenderer>(
	static_cast<DefaultRendererTpl<BitmapGlyphRenderer>&&>(tmp)
){ }
#endif

} // namespace text
} // namespace oglplus

