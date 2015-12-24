/**
 *  @file oglplus/ext/NV_path_rendering.hpp
 *  @brief Wrapper for the NV_path_rendering extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_NV_PATH_RENDERING_1203031902_HPP
#define OGLPLUS_EXT_NV_PATH_RENDERING_1203031902_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_path_rendering

#include <oglplus/ext/NV_path_rendering/path_array.hpp>
#include <oglplus/ext/NV_path_rendering/color.hpp>
#include <oglplus/ext/NV_path_rendering/color_format.hpp>
#include <oglplus/ext/NV_path_rendering/gen_mode.hpp>

#include <oglplus/texture_unit.hpp>
#include <oglplus/extension.hpp>

namespace oglplus {

/// Wrapper for the NV_path_rendering extension
/**
 *  @glsymbols
 *  @glextref{NV,path_rendering}
 *
 *  @ingroup gl_extensions
 */
class NV_path_rendering
{
public:
	OGLPLUS_EXTENSION_CLASS(NV, path_rendering)

	/// Sets the depth @p function to be used with path cover functions
	/**
	 *  @glsymbols
	 *  @glfunref{PathCoverDepthFuncNV}
	 */
	static void CoverDepthFunc(CompareFunction function)
	{
		OGLPLUS_GLFUNC(PathCoverDepthFuncNV)(GLenum(function));
		OGLPLUS_CHECK_SIMPLE(PathCoverDepthFuncNV);
	}

	/// Sets the depth offset for rendered paths
	/**
	 *  @glsymbols
	 *  @glfunref{PathStencilDepthOffsetNV}
	 */
	static void DepthOffset(GLfloat factor, GLint units)
	{
		OGLPLUS_GLFUNC(PathStencilDepthOffsetNV)(factor, units);
		OGLPLUS_CHECK_SIMPLE(PathStencilDepthOffsetNV);
	}

	/// Sets how colors are computed for path covering fragment operations
	/**
	 *  @glsymbols
	 *  @glfunref{PathColorGenNV}
	 */
	static void ColorGen(
		PathNVColor color,
		PathNVGenMode gen_mode,
		PathNVColorFormat color_format,
		const GLfloat* coeffs
	)
	{
		OGLPLUS_GLFUNC(PathColorGenNV)(
			GLenum(color),
			GLenum(gen_mode),
			GLenum(color_format),
			coeffs
		);
		OGLPLUS_CHECK_SIMPLE(PathColorGenNV);
	}

	/// Sets how tex-coords are computed for path covering fragment operations
	/**
	 *  @glsymbols
	 *  @glfunref{PathTexGenNV}
	 */
	static void TexGen(
		TextureUnitSelector tex_unit,
		PathNVGenMode gen_mode,
		GLint components,
		const GLfloat* coeffs
	)
	{
		OGLPLUS_GLFUNC(PathTexGenNV)(
			GLenum(GL_TEXTURE0 + GLuint(tex_unit)),
			GLenum(gen_mode),
			components,
			coeffs
		);
		OGLPLUS_CHECK_SIMPLE(PathTexGenNV);
	}
};

} // namespace oglplus

#endif // NV_path_rendering

#endif // include guard
