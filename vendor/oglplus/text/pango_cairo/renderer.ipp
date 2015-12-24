/**
 *  @file oglplus/text/pango_cairo/renderer.ipp
 *  @brief Implementation of pango/cairo-based text rendering, renderer
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
PangoCairoRenderer::PangoCairoRenderer(
	PangoCairoRendering& parent,
	const Sequence<ShaderName>& shaders
): _parent(parent)
 , _program()
 , _bitmap(_program)
 , _log_coords(_program)
 , _tex_coords(_program)
{
	VertexShader vs(ObjectDesc("PangoCairoRenderer - Vertex"));
	vs.Source(StrCRef(
		"#if GL_ARB_explicit_attrib_location\n"
		"#version 150\n"
		"#extension GL_ARB_explicit_attrib_location : enable\n"
		"#else\n"
		"#version 330\n"
		"#endif\n"
		
		"uniform vec4 oglpLogCoords;"
		"uniform float oglpAlignCoef;"

		"float oglpLayoutWidth = oglpLogCoords.y-oglpLogCoords.x;"

		"void main(void)"
		"{"
		"	float XOffs = (oglpAlignCoef-0.5)*oglpLayoutWidth;"
		"	gl_Position = vec4(XOffs, 0.0, 0.0, 1.0);"
		"}"
	));
	vs.Compile();
	_program.AttachShader(vs);

	GeometryShader gs(ObjectDesc("PangoCairoRenderer - Geometry"));
	gs.Source(StrCRef(
		"#version 150\n"
		"layout (points) in;"
		"layout (triangle_strip, max_vertices = 4) out;"

		"uniform vec4 oglpLogCoords, oglpTexCoords;"

		"vec4 TransformLayout(vec3 LayoutPosition);"

		"out vec4 geomLayoutPos;"
		"out vec4 geomLayoutCoord;"
		"out vec2 geomTexCoord;"

		"uniform float oglpLayoutWidth;"

		"void make_vertex(vec2 Position, vec2 LayoutCoord, vec2 TexCoord)"
		"{"
		"	Position += gl_in[0].gl_Position.xy;"
		"	geomLayoutPos = vec4(Position, 0.0, 0.0);"
		"	gl_Position = TransformLayout(geomLayoutPos.xyz);"
		"	geomLayoutCoord = vec4(Position, LayoutCoord);"
		"	geomTexCoord = TexCoord;"
		"	EmitVertex();"
		"}"

		"void main(void)"
		"{"
		//      left bearing
		"       float lb = oglpLogCoords.x;"
		//      right bearing
		"       float rb = oglpLogCoords.y;"
		//      ascender
		"       float as = oglpLogCoords.z;"
		//      descender
		"       float ds = oglpLogCoords.w;"
		//      layout origin in texture space
		"       vec2  to = oglpTexCoords.xy;"
		//      layout width in texture space
		"       float tw = oglpTexCoords.z;"
		//      layout height in texture space
		"       float th = oglpTexCoords.w;"

		"       make_vertex(vec2(lb,-ds), vec2(0.0,-1.0), to+vec2(0.0,  th));"
		"       make_vertex(vec2(lb, as), vec2(0.0, 1.0), to+vec2(0.0, 0.0));"
		"       make_vertex(vec2(rb,-ds), vec2(1.0,-1.0), to+vec2( tw,  th));"
		"       make_vertex(vec2(rb, as), vec2(1.0, 1.0), to+vec2( tw, 0.0));"
		"       EndPrimitive();"
		"}"
	));

	gs.Compile();
	_program.AttachShader(gs);


	FragmentShader fs(ObjectDesc("PangoCairoRenderer - Fragment"));
	fs.Source(StrCRef(
		"#version 150\n"
		"uniform sampler2DRect oglpBitmap;"

		"uniform vec4 oglpLogCoords;"

		"in vec4 geomLayoutPos;"
		"in vec4 geomLayoutCoord;"
		"in vec2 geomTexCoord;"

		"out vec4 fragColor;"

		"vec4 PixelColor("
		"	vec4 TexelColor,"
		"	vec3 LayoutPosition,"
		"	float LayoutXOffset,"
		"	vec2 LayoutExtent,"
		"	vec2 LayoutCoord,"
		"	float LayoutWidth"
		");"

		"void main(void)"
		"{"
		"       fragColor = PixelColor("
		"		texture(oglpBitmap, geomTexCoord),"
		"		geomLayoutPos.xyz,"
		"		geomLayoutPos.w,"
		"		geomLayoutCoord.xy,"
		"		geomLayoutCoord.zw,"
		"		geomLayoutPos.y-geomLayoutPos.x"
		"	);"
		"}"

	));

	fs.Compile();
	_program.AttachShader(fs);
	_program.AttachShaders(shaders);
	_program.Link();
	_program.Use();

	_bitmap.BindTo("oglpBitmap");
	_log_coords.BindTo("oglpLogCoords");
	_tex_coords.BindTo("oglpTexCoords");

	Uniform<GLfloat>(_program, "oglpAlignCoef").Set(0.5f);
}

OGLPLUS_LIB_FUNC
PangoCairoDefaultRenderer::PangoCairoDefaultRenderer(
	PangoCairoRendering& parent,
	const FragmentShader& pixel_color_fs
): DefaultRendererTpl<PangoCairoRenderer>(
	parent,
	StaticGroup<ShaderName, 2>(
		pixel_color_fs,
		GeometryShader(
		ObjectDesc("PangoCairoRenderer - Layout transform"),
		StrCRef("#version 150\n"
		"uniform mat4 "
		"	oglpProjectionMatrix,"
		"	oglpCameraMatrix,"
		"	oglpLayoutMatrix;"
		"mat4 Matrix = "
		"	oglpProjectionMatrix*"
		"	oglpCameraMatrix*"
		"	oglpLayoutMatrix;"

		"vec4 TransformLayout(vec3 LayoutPosition)"
		"{"
		"	return Matrix * vec4(LayoutPosition, 1.0);"
		"}")
		)
	)
)
{
}

} // namespace text
} // namespace oglplus

