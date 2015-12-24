/**
 *  @file oglplus/text/pango_cairo/renderer.hpp
 *  @brief Pango/Cairo-based text rendering - renderer.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_PANGO_CAIRO_RENDERER_HPP
#define OGLPLUS_TEXT_PANGO_CAIRO_RENDERER_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/text/common.hpp>
#include <oglplus/text/pango_cairo/fwd.hpp>
#include <oglplus/text/pango_cairo/layout.hpp>
#include <oglplus/context.hpp>
#include <oglplus/vertex_array.hpp>
#include <oglplus/uniform.hpp>


namespace oglplus {
namespace text {

class PangoCairoRenderer
{
private:
	PangoCairoRendering& _parent;

	Context _gl;

	Program _program;

	UniformSampler _bitmap;
	Uniform<Vec4f> _log_coords, _tex_coords;
protected:
	const Program& _get_program(void) const
	{
		return _program;
	}
public:
	PangoCairoRenderer(
		PangoCairoRendering& parent,
		const Sequence<ShaderName>& shaders
	);

	void Use(void)
	{
		_program.Use();
		NoVertexArray().Bind();
	}

	void Render(const PangoCairoLayout& layout)
	{
		_bitmap.Set(GLint(layout.Use()));
		_log_coords.Set(layout._log_coords);
		_tex_coords.Set(layout._tex_coords);
		_gl.DrawArrays(PrimitiveType::Points, 0, 1);
	}

	template <typename T>
	ProgramUniform<T> GetUniform(const GLchar* name) const
	{
		return ProgramUniform<T>(_program, name);
	}
};

class PangoCairoDefaultRenderer
 : public DefaultRendererTpl<PangoCairoRenderer>
{
public:
	PangoCairoDefaultRenderer(
		PangoCairoRendering& parent,
		const FragmentShader& pixel_color_fs
	);
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/pango_cairo/renderer.ipp>
#endif

#endif // include guard
