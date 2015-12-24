/**
 *  @file oglplus/ext/NV_path_rendering/path_array.hpp
 *  @brief Wrapper for the NV_path_rendering path array class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_NV_PATH_RENDERING_PATH_ARRAY_1203031902_HPP
#define OGLPLUS_EXT_NV_PATH_RENDERING_PATH_ARRAY_1203031902_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_path_rendering

#include <oglplus/ext/NV_path_rendering/path.hpp>

#include <oglplus/ext/NV_path_rendering/font_target.hpp>
#include <oglplus/ext/NV_path_rendering/font_style.hpp>
#include <oglplus/ext/NV_path_rendering/list_mode.hpp>
#include <oglplus/ext/NV_path_rendering/missing_glyph.hpp>
#include <oglplus/ext/NV_path_rendering/metric_query.hpp>

namespace oglplus {

/// Array of PathNV objects and related operations
/**
 *  @see PathNV
 */
class PathArrayNV
 : public Array<PathNV>
{
public:
	/// Creates a sequence of @p count paths
	PathArrayNV(SizeType count)
	 : Array<PathNV>(count)
	{ }

	/// Creates a range of paths from specified font for specified chars
	/**
	 *  @glsymbols
	 *  @glfunref{PathGlyphsNV}
	 */
	template <typename CharType>
	PathArrayNV& Glyphs(
		PathNVFontTarget font_target,
		const GLchar* font_name,
		Bitfield<PathNVFontStyle> font_style,
		SizeType num_glyphs,
		const CharType* char_codes,
		PathNVMissingGlyph handle_missing_glyphs,
		GLuint parameter_template,
		GLfloat em_scale
	)
	{
		OGLPLUS_GLFUNC(PathGlyphsNV)(
			this->_names[0],
			GLenum(font_target),
			static_cast<const void*>(font_name),
			GLbitfield(font_style),
			num_glyphs,
			GetDataType<CharType>(),
			static_cast<const void*>(char_codes),
			GLenum(handle_missing_glyphs),
			parameter_template,
			em_scale
		);
		OGLPLUS_CHECK(
			PathGlyphsNV,
			Error,
			EnumParam(font_target)
		);
		return *this;
	}

	/// Creates a range of paths from specified font for specified chars
	/**
	 *  @glsymbols
	 *  @glfunref{PathGlyphsNV}
	 */
	template <typename CharType>
	PathArrayNV& Glyphs(
		PathNVFontTarget font_target,
		const GLchar* font_name,
		Bitfield<PathNVFontStyle> font_style,
		const std::vector<CharType>& char_codes,
		PathNVMissingGlyph handle_missing_glyphs,
		GLuint parameter_template,
		GLfloat em_scale
	)
	{
		OGLPLUS_GLFUNC(PathGlyphsNV)(
			this->_names[0],
			GLenum(font_target),
			static_cast<const void*>(font_name),
			GLbitfield(font_style),
			GLsizei(char_codes.size()),
			GetDataType<CharType>(),
			static_cast<const void*>(char_codes.data()),
			GLenum(handle_missing_glyphs),
			parameter_template,
			em_scale
		);
		OGLPLUS_CHECK(
			PathGlyphsNV,
			Error,
			EnumParam(font_target)
		);
		return *this;
	}

	/// Creates a range of paths from specified font for specified chars
	/**
	 *  @glsymbols
	 *  @glfunref{PathGlyphsNV}
	 */
	PathArrayNV& Glyphs(
		PathNVFontTarget font_target,
		const GLchar* font_name,
		Bitfield<PathNVFontStyle> font_style,
		StrCRef char_codes,
		PathNVMissingGlyph handle_missing_glyphs,
		GLuint parameter_template,
		GLfloat em_scale
	)
	{
		OGLPLUS_GLFUNC(PathGlyphsNV)(
			this->_names[0],
			GLenum(font_target),
			static_cast<const void*>(font_name),
			GLbitfield(font_style),
			GLsizei(char_codes.size()),
			GL_UTF8_NV,
			static_cast<const void*>(char_codes.c_str()),
			GLenum(handle_missing_glyphs),
			parameter_template,
			em_scale
		);
		OGLPLUS_CHECK(
			PathGlyphsNV,
			Error,
			EnumParam(font_target)
		);
		return *this;
	}

	/// Creates a range of paths from specified font
	/**
	 *  @glsymbols
	 *  @glfunref{PathGlyphRangeNV}
	 */
	PathArrayNV& GlyphRange(
		PathNVFontTarget font_target,
		const GLchar* font_name,
		Bitfield<PathNVFontStyle> font_style,
		GLuint first_glyph,
		SizeType num_glyphs,
		PathNVMissingGlyph handle_missing_glyphs,
		GLuint parameter_template,
		GLfloat em_scale
	)
	{
		OGLPLUS_GLFUNC(PathGlyphRangeNV)(
			this->_names[0],
			GLenum(font_target),
			static_cast<const void*>(font_name),
			GLbitfield(font_style),
			first_glyph,
			num_glyphs,
			GLenum(handle_missing_glyphs),
			parameter_template,
			em_scale
		);
		OGLPLUS_CHECK(
			PathGlyphRangeNV,
			Error,
			EnumParam(font_target)
		);
		return *this;
	}

	/// Queries the glyph spacing for paths in the array
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathSpacingNV}
	 */
	template <typename IndexType>
	PathArrayNV& GetSpacing(
		PathNVListMode list_mode,
		SizeType num_indices,
		const IndexType* indices,
		GLfloat advance_scale,
		GLfloat kerning_scale,
		PathNVTransformType transform_type,
		GLfloat* returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathSpacingNV)(
			GLenum(list_mode),
			num_indices,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(indices),
			this->_names[0],
			advance_scale,
			kerning_scale,
			GLenum(transform_type),
			returned_values
		);
		OGLPLUS_CHECK(
			GetPathSpacingNV,
			Error,
			EnumParam(list_mode)
		);
		return *this;
	}

	/// Queries the glyph spacing for paths in the array
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathSpacingNV}
	 */
	template <typename IndexType>
	PathArrayNV& GetSpacing(
		PathNVListMode list_mode,
		const std::vector<IndexType>& indices,
		GLfloat advance_scale,
		GLfloat kerning_scale,
		PathNVTransformType transform_type,
		std::vector<GLfloat>& returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathSpacingNV)(
			GLenum(list_mode),
			GLsizei(indices.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(indices.data()),
			this->_names[0],
			advance_scale,
			kerning_scale,
			GLenum(transform_type),
			returned_values.data()
		);
		OGLPLUS_CHECK(
			GetPathSpacingNV,
			Error,
			EnumParam(list_mode)
		);
		return *this;
	}

	/// Queries the glyph spacing for paths in the array
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathSpacingNV}
	 */
	PathArrayNV& GetSpacing(
		PathNVListMode list_mode,
		StrCRef indices,
		GLfloat advance_scale,
		GLfloat kerning_scale,
		PathNVTransformType transform_type,
		std::vector<GLfloat>& returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathSpacingNV)(
			GLenum(list_mode),
			GLsizei(indices.size()+1), //include null terminator
			GL_UTF8_NV,
			static_cast<const void*>(indices.c_str()),
			this->_names[0],
			advance_scale,
			kerning_scale,
			GLenum(transform_type),
			returned_values.data()
		);
		OGLPLUS_CHECK(
			GetPathSpacingNV,
			Error,
			EnumParam(list_mode)
		);
		return *this;
	}

	/// Queries the glyph metrics associated with a sequence of path objects
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathMetricsNV}
	 */
	template <typename IndexType>
	PathArrayNV& GetMetrics(
		Bitfield<PathNVMetricQuery> query_mask,
		SizeType num_indices,
		const IndexType* indices,
		SizeType stride,
		GLfloat* returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathMetricsNV)(
			GLbitfield(query_mask),
			num_indices,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(indices),
			this->_names[0],
			stride,
			returned_values
		);
		OGLPLUS_CHECK_SIMPLE(GetPathMetricsNV);
		return *this;
	}

	/// Queries the glyph metrics associated with a sequence of path objects
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathMetricsNV}
	 */
	template <typename IndexType>
	PathArrayNV& GetMetrics(
		Bitfield<PathNVMetricQuery> query_mask,
		const std::vector<IndexType>& indices,
		SizeType stride,
		GLfloat* returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathMetricsNV)(
			GLbitfield(query_mask),
			GLsizei(indices.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(indices.data()),
			this->_names[0],
			stride,
			returned_values
		);
		OGLPLUS_CHECK_SIMPLE(GetPathMetricsNV);
		return *this;
	}

	/// Queries the glyph metrics associated with a sequence of path objects
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathMetricsNV}
	 */
	PathArrayNV& GetMetrics(
		Bitfield<PathNVMetricQuery> query_mask,
		StrCRef indices,
		SizeType stride,
		GLfloat* returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathMetricsNV)(
			GLbitfield(query_mask),
			GLsizei(indices.size()+1),
			GL_UTF8_NV,
			static_cast<const void*>(indices.c_str()),
			this->_names[0],
			stride,
			returned_values
		);
		OGLPLUS_CHECK_SIMPLE(GetPathMetricsNV);
		return *this;
	}

	/// Queries the glyph metrics associated with a range of path objects
	/**
	 *  @glsymbols
	 *  @glfunref{GetPathMetricRangeNV}
	 */
	PathArrayNV& GetMetricRange(
		Bitfield<PathNVMetricQuery> query_mask,
		SizeType num_paths,
		SizeType stride,
		GLfloat* returned_values
	)
	{
		OGLPLUS_GLFUNC(GetPathMetricRangeNV)(
			GLbitfield(query_mask),
			this->_names[0],
			num_paths,
			stride,
			returned_values
		);
		OGLPLUS_CHECK_SIMPLE(GetPathMetricRangeNV);
		return *this;
	}

	/// Writes the path interiors (fill) into the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& StencilFillInstanced(
		SizeType num_paths,
		const IndexType* paths,
		PathNVFillMode mode,
		GLuint mask,
		PathNVTransformType transform_type,
		const GLfloat* transform_values
	)
	{
		OGLPLUS_GLFUNC(StencilFillPathInstancedNV)(
			num_paths,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths),
			this->_names[0],
			GLenum(mode),
			mask,
			GLenum(transform_type),
			transform_values
		);
		OGLPLUS_CHECK(
			StencilFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Writes the path interiors (fill) into the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& StencilFillInstanced(
		const std::vector<IndexType>& paths,
		PathNVFillMode mode,
		GLuint mask,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		// TODO: check if enough transform values
		// are provided for transform type
		OGLPLUS_GLFUNC(StencilFillPathInstancedNV)(
			GLsizei(paths.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths.data()),
			this->_names[0],
			GLenum(mode),
			mask,
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			StencilFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Writes the path interiors (fill) into the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilFillPathInstancedNV}
	 */
	PathArrayNV& StencilFillInstanced(
		StrCRef paths,
		PathNVFillMode mode,
		GLuint mask,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		// TODO: check if enough transform values
		// are provided for transform type
		OGLPLUS_GLFUNC(StencilFillPathInstancedNV)(
			GLsizei(paths.size()),
			GL_UTF8_NV,
			static_cast<const void*>(paths.c_str()),
			this->_names[0],
			GLenum(mode),
			mask,
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			StencilFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Covers the path interiors (fill)
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& CoverFillInstanced(
		SizeType num_paths,
		const IndexType* paths,
		PathNVFillCoverMode mode,
		PathNVTransformType transform_type,
		const GLfloat* transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverFillPathInstancedNV)(
			num_paths,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values
		);
		OGLPLUS_CHECK(
			CoverFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Covers the path interiors (fill)
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& CoverFillInstanced(
		const std::vector<IndexType>& paths,
		PathNVFillCoverMode mode,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverFillPathInstancedNV)(
			GLsizei(paths.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths.data()),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			CoverFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Covers the path interiors (fill)
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	PathArrayNV& CoverFillInstanced(
		StrCRef paths,
		PathNVFillCoverMode mode,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverFillPathInstancedNV)(
			GLsizei(paths.size()),
			GL_UTF8_NV,
			static_cast<const void*>(paths.c_str()),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			CoverFillPathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Writes the path interiors strokes to the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilStrokePathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& StencilStrokeInstanced(
		SizeType num_paths,
		const IndexType* paths,
		GLint reference,
		GLuint mask,
		PathNVTransformType transform_type,
		const GLfloat* transform_values
	)
	{
		OGLPLUS_GLFUNC(StencilStrokePathInstancedNV)(
			num_paths,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths),
			this->_names[0],
			reference,
			mask,
			GLenum(transform_type),
			transform_values
		);
		OGLPLUS_CHECK_SIMPLE(StencilStrokePathInstancedNV);
		return *this;
	}

	/// Writes the path interiors strokes to the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilStrokePathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& StencilStrokeInstanced(
		const std::vector<IndexType>& paths,
		GLint reference,
		GLuint mask,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(StencilStrokePathInstancedNV)(
			GLsizei(paths.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths.data()),
			this->_names[0],
			reference,
			mask,
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK_SIMPLE(StencilStrokePathInstancedNV);
		return *this;
	}

	/// Writes the path interiors strokes to the stencil buffer
	/**
	 *  @glsymbols
	 *  @glfunref{StencilStrokePathInstancedNV}
	 */
	PathArrayNV& StencilStrokeInstanced(
		StrCRef paths,
		GLint reference,
		GLuint mask,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(StencilStrokePathInstancedNV)(
			GLsizei(paths.size()),
			GL_UTF8_NV,
			static_cast<const void*>(paths.c_str()),
			this->_names[0],
			reference,
			mask,
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK_SIMPLE(StencilStrokePathInstancedNV);
		return *this;
	}

	/// Covers the path strokes
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& CoverStrokeInstanced(
		SizeType num_paths,
		const IndexType* paths,
		PathNVStrokeCoverMode mode,
		PathNVTransformType transform_type,
		const GLfloat* transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverStrokePathInstancedNV)(
			num_paths,
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values
		);
		OGLPLUS_CHECK(
			CoverStrokePathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Covers the path strokes
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	template <typename IndexType>
	PathArrayNV& CoverStrokeInstanced(
		const std::vector<IndexType>& paths,
		PathNVStrokeCoverMode mode,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverStrokePathInstancedNV)(
			GLsizei(paths.size()),
			GLenum(GetDataType<IndexType>()),
			static_cast<const void*>(paths.data()),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			CoverStrokePathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}

	/// Covers the path strokes
	/**
	 *  @glsymbols
	 *  @glfunref{CoverFillPathInstancedNV}
	 */
	PathArrayNV& CoverStrokeInstanced(
		StrCRef paths,
		PathNVStrokeCoverMode mode,
		PathNVTransformType transform_type,
		const std::vector<GLfloat>& transform_values
	)
	{
		OGLPLUS_GLFUNC(CoverStrokePathInstancedNV)(
			GLsizei(paths.size()),
			GL_UTF8_NV,
			static_cast<const void*>(paths.c_str()),
			this->_names[0],
			GLenum(mode),
			GLenum(transform_type),
			transform_values.data()
		);
		OGLPLUS_CHECK(
			CoverStrokePathInstancedNV,
			Error,
			EnumParam(mode)
		);
		return *this;
	}
};

} // namespace oglplus

#endif // NV_path_rendering

#endif // include guard
