/**
 *  @file oglplus/text/common.hpp
 *  @brief Common classes and functions used in text rendering
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_COMMON_HPP
#define OGLPLUS_TEXT_COMMON_HPP

#include <oglplus/text/unicode.hpp>
#include <oglplus/dsa/uniform.hpp>
#include <oglplus/math/matrix.hpp>
#include <oglplus/size_type.hpp>

namespace oglplus {

/** @defgroup text_rendering Text rendering
 *
 *  OGLplus implements a set of utilities for simple text rendering.
 *
 *  @note The text rendering utilities are still work-in-progress
 *  and there are several limitations, for example, some special scripts
 *  like arabic may be rendered properly, combining characters are not
 *  supported, etc.
 */

/// Namespace containing text rendering utilities
namespace text {

/// Floating-point rectangle used for Glyph and Layout measurements
/**
 *  @ingroup text_rendering
 */
class Rectangle
{
private:
	GLfloat _left, _right, _bottom, _top;
public:
	Rectangle(GLfloat left, GLfloat right, GLfloat bottom, GLfloat top)
	 : _left(left)
	 , _right(right)
	 , _bottom(bottom)
	 , _top(top)
	{ }

	/// The left edge of the rectangle
	GLfloat Left(void) const { return _left; }

	/// the right edge of the rectangle
	GLfloat Right(void) const { return _right; }

	/// the bottom edge of the rectangle
	GLfloat Bottom(void) const { return _bottom; }

	/// the top edge of the rectangle
	GLfloat Top(void) const { return _top; }

	/// The width of the rectangle
	GLfloat Width(void) const {return Right() - Left(); }

	/// The height of the rectangle
	GLfloat Height(void) const {return Top() - Bottom(); }
};

/// Enumeration specifying the horizonal alignment when rendering layouts
/**
 *  @ingroup text_rendering
 */
OGLPLUS_ENUM_CLASS_BEGIN(Alignment, int)
#if OGLPLUS_DOCUMENTATION_ONLY
	/// Glyphs are placed to the right of the layout origin
	Left,
	/// Glyphs are placed to the both sided of the layout origin
	Center,
	/// Glyphs are placed to the left of the layout origin
	Right
#else
	OGLPLUS_ENUM_CLASS_VALUE(Left, -1)
	OGLPLUS_ENUM_CLASS_COMMA
	OGLPLUS_ENUM_CLASS_VALUE(Center, 0)
	OGLPLUS_ENUM_CLASS_COMMA
	OGLPLUS_ENUM_CLASS_VALUE(Right, 1)
#endif
OGLPLUS_ENUM_CLASS_END(Alignment)

/// Enumeration specifying the writing direction when rendering layouts
/**
 *  @ingroup text_rendering
 */
OGLPLUS_ENUM_CLASS_BEGIN(Direction, int)
#if OGLPLUS_DOCUMENTATION_ONLY
	/// Left-to-right writing direction
	LeftToRight,
	/// Right-to-left writing direction
	RightToLeft
	// TODO TopDown?
#else
	OGLPLUS_ENUM_CLASS_VALUE(LeftToRight, 1)
	OGLPLUS_ENUM_CLASS_COMMA
	OGLPLUS_ENUM_CLASS_VALUE(RightToLeft, -1)
#endif
OGLPLUS_ENUM_CLASS_END(Direction)

#if OGLPLUS_DOCUMENTATION_ONLY

// TODO: docs

/// Represents a font in a particular text rendering utility
/** @note There is no real class called UnspecifiedFont,
 *  it is here for documentation purporses only. Concrete
 *  implementations of text RenderingUtility have their own
 *  font classes. The exact type of these can be obtained
 *  from the @c Font typedef in the particular @c RenderingUtility.
 *  Concrete implementations of font may have additional
 *  members.
 *
 *  Font encapsulates the data describing individual glyphs.
 *  Depending on the implementation, the glyphs may be stored
 *  as bitmaps in a texture or in a polygonal representation, etc.
 *
 *  Instances of font are constructed by their parent RenderingUtility.
 *
 *  All implementations of fonts use the following convention
 *  regarding the size and scaling of the glyphs:
 *  In any font the distance (in OpenGL units) between the font's
 *  ascender and descender is one - this means that the logical
 *  height of the glyphs in the font is one unit. The logical width
 *  of the glyphs is proportional to the height. To scale the glyphs
 *  during rendering the renderer should be set-up accordingly.
 *  This means that all fonts have the same scale, but (based on the
 *  implementation) they can have different resolutions. For example
 *  in bitmap-based fonts, the resolution is determined by the reslution
 *  of the bitmap storing the glyphs. In polygonal representation
 *  the resolution is determined by the "coarseness" of the mesh.
 *
 *  @see RenderingUtility
 *
 *  @ingroup text_rendering
 */
class UnspecifiedFont
{
public:
	/// Returns the logical metrics for the specified glyph
	Rectangle GlyphLogicalMetrics(CodePoint code_point);

	/// Query the x-offsets (advance) of the individual glyphs
	GLfloat QueryXOffsets(
		const CodePoint* cps,
		SizeType size,
		std::vector<GLfloat>& x_offsets
	);
};


/// Represents a text layout in a particular text rendering utility
/** @note There is no real class called UnspecifiedLayout,
 *  it is here for documentation purporses only. Concrete
 *  implementations of text RenderingUtility have their own
 *  layout classes. The exact type of these can be obtained
 *  from the @c Layout typedef in the particular @c RenderingUtility.
 *  Concrete implementations of layouts may have additional
 *  members.
 *
 *  Layouts encapsulate the data describing the layout of the glyphs
 *  in a single line of text. Layouts can be rendered by a renderer
 *  provided by the same implementation of text RenderingUtility.
 *
 *  Instances of layout are constructed by their parent RenderingUtility.
 *
 *  @see RenderingUtility
 *
 *  @ingroup text_rendering
 */
class UnspecifiedLayout
{
public:
	/// The maximum number of glyphs that can be stored in the layout
	/** Layouts have a maximum capacity, that is determined at the time
	 *  of construction. The number of code-points that are set via
	 *  the @c Set member function must not be greater than the capacity.
	 *
	 *  @see Set
	 */
	SizeType Capacity(void) const;

	/// The width of the layout in font size units
	GLfloat Width(void) const;

	/// Sets a new text to be layed-out
	/** This function lays-out a new sequence of glyphs representing
	 *  the text specified by the @c code_points argument.
	 *  The previous glyph layout (if any) is discarded.
	 *
	 *  The @c length must not be greater that the value returned
	 *  by @c Capacity.
	 *
	 *  @see Capacity
	 */
	void Set(const CodePoint* code_points, SizeType length);

	/// Sets a new text to be layed-out
	/** This function lays-out a new sequence of glyphs representing
	 *  the text specified by the @c string argument.
	 *  The previous glyph layout (if any) is discarded.
	 *
	 *  The @c string must be encoded in (normalized) UTF-8.
	 *  The number of code points after the conversion to UTF-32 must not
	 *  exceed the value returned by @c Capacity.
	 *
	 *  @see Capacity
	 */
	void Set(StrCRef string);
};


/// Can be used to render text layouts from the same text RenderingUtility
/** @note There is no real class called UnspecifiedRenderer,
 *  it is here for documentation purporses only. Concrete
 *  implementations of text RenderingUtility have their own
 *  renderer classes. The exact type of these can be obtained
 *  from the @c Renderer typedef in the particular @c RenderingUtility.
 *  Concrete implementations of layouts may have additional
 *  members.
 *
 *  Renderers are used to draw transformed text layouts in 3D space.
 *  Default renderers allow to set separate projection, camera and
 *  whole-layout transformation matrices and layout horizontal alignment.
 *
 *  During rendering the alignment (left, center, right) is first applied
 *  and then every vertex in the layout is transformed according to:
 *  @code
 *  vec4 ScreenPos =
 *      ProjectionMatrix*
 *      CameraMatrix*
 *      LayoutTransformMatrix*
 *      vec4(AlignedPosition, 0.0, 1.0);
 *  @endcode
 *
 *  Instances of renderers are constructed by their parent RenderingUtility.
 *
 *  @see RenderingUtility
 *
 *  @ingroup text_rendering
 */
class UnspecifiedRenderer
{
public:
	/// Sets the projection matrix
	/** The new matrix applies to subsequent rendering operations.
	 *
	 *  @see Render()
	 *  @see SetCamera()
	 *  @see SetLayoutTransform()
	 *  @see SetAlignment()
	 *  @see SetDirection()
	 */
	void SetProjection(const Mat4f& projection_matrix);

	/// Sets the camera matrix
	/** The new matrix applies to subsequent rendering operations.
	 *
	 *  @see Render()
	 *  @see SetProjection()
	 *  @see SetLayoutTransform()
	 *  @see SetAlignment()
	 *  @see SetDirection()
	 */
	void SetCamera(const Mat4f& camera_matrix);

	/// Sets the layout transformation matrix
	/** The new matrix applies to subsequent rendering operations.
	 *
	 *  @see Render()
	 *  @see SetProjection()
	 *  @see SetCamera()
	 *  @see SetAlignment()
	 *  @see SetDirection()
	 */
	void SetLayoutTransform(const Mat4f& layout_matrix);

	/// Sets the layout horizontal alignment
	/** The new alignment applies to subsequent rendering operations.
	 *
	 *  @see Render()
	 *  @see SetProjection()
	 *  @see SetCamera()
	 *  @see SetLayoutTransform()
	 *  @see SetDirection()
	 */
	void SetAlignment(Alignment alignment);

	/// Sets the writing direction
	/** The new direction applies to subsequent rendering operations.
	 *
	 *  @see Render()
	 *  @see SetProjection()
	 *  @see SetCamera()
	 *  @see SetLayoutTransform()
	 *  @see SetAlignment()
	 */
	void SetDirection(Direction direction);

	/// Use the renderer
	/** Call this function before using the renderer
	 */
	void Use(void);

	/// Transforms and renders the specified @p layout
	/** The currently set alignment and matrices are used
	 *  to transform the layout to screen space.
	 */
	void Render(Layout layout);
};

/// Represents an implementation of a text rendering utility
/** @note There is no real RenderingUtility class, it is here
 *  for documentation purporses only.
 *  Concrete implementations of rendering utilities may have additional
 *  members and may support additional specific functionality.
 */
class RenderingUtility
{
public:
	/// The concrete font type of the text rendering utility
	typedef UnspecifiedFont Font;

	/// Loads a font with the specified name
	/** Font loading is a potentially time-consuming operation. Programs
	 *  should load the required fonts once and re-use them, if possible
	 *  for optimal performance.
	 */
	Font LoadFont(const char* font_name);

	/// The concrete layout type of the text rendering utility
	typedef UnspecifiedLayout Layout;

	/// Makes a new layout object using the @p font with specified @p capacity
	/** Layout construction is a potentially time-consuming operation.
	 *  Programs should request layouts with sufficient capacity and try
	 *  to reuse them, if possible for optimal performance.
	 */
	Layout MakeLayout(Font font, SizeType capacity);

	/// The concrete renderer type of the text rendering utility
	typedef UnspecifiedRenderer Renderer;

	/// Returns a new default renderer
	/** The renderer uses a custom fragment shader that should
	 *  implement a function with the following signature:
	 *  @code
	 *  vec4 PixelColor(vec4 Color);
	 *  @endcode
	 *  This shader can be used to change the color and alpha
	 *  components of the final fragment color on the rendered
	 *  glyphs. The @c Color value passed to this function depends
	 *  on the concrete implementation. For bitmap-based rendering
	 *  utilities it is for example the sampled value from the font
	 *  texture.
	 *
	 *  Renderer construction is a potentially time-consuming operation.
	 *  Programs should get a renderer once and re-use it, if possible
	 *  for optimal performance.
	 */
	Renderer GetRenderer(const FragmentShader& fragment_shader);
};

#endif

template <class ConcreteRenderer>
class DefaultRendererTpl
 : public ConcreteRenderer
{
private:
	ProgramUniform<Mat4f>
		_projection_matrix,
		_camera_matrix,
		_layout_matrix;

	ProgramUniform<GLfloat>
		_align_coef,
		_dir_coef;

	typedef ConcreteRenderer Base;
public:
	template <class ConcreteRenderingImpl>
	DefaultRendererTpl(
		ConcreteRenderingImpl& parent,
		const Sequence<ShaderName>& shaders
	): ConcreteRenderer(parent, shaders)
	 , _projection_matrix(Base::_get_program(), "oglpProjectionMatrix")
	 , _camera_matrix(Base::_get_program(), "oglpCameraMatrix")
	 , _layout_matrix(Base::_get_program(), "oglpLayoutMatrix")
	 , _align_coef(Base::_get_program(), "oglpAlignCoef", false)
	 , _dir_coef(Base::_get_program(), "oglpDirCoef", false)
	{
		_projection_matrix.Set(Mat4f());
		_camera_matrix.Set(Mat4f());
		_layout_matrix.Set(Mat4f());
		if(_align_coef) _align_coef.Set(0.5f);
		if(_dir_coef) _dir_coef.Set(1.0f);
	}

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	DefaultRendererTpl(DefaultRendererTpl&&) = default;
#else
	DefaultRendererTpl(DefaultRendererTpl&& tmp)
	 : Base(static_cast<Base&&>(tmp))
	 , _projection_matrix(std::move(tmp._projection_matrix))
	 , _camera_matrix(std::move(tmp._camera_matrix))
	 , _layout_matrix(std::move(tmp._layout_matrix))
	 , _align_coef(std::move(tmp._align_coef))
	 , _dir_coef(std::move(tmp._dir_coef))
	{ }
#endif

	void SetProjection(const Mat4f& projection_matrix)
	{
		_projection_matrix.Set(projection_matrix);
	}

	void SetCamera(const Mat4f& camera_matrix)
	{
		_camera_matrix.Set(camera_matrix);
	}

	void SetLayoutTransform(const Mat4f& layout_matrix)
	{
		_layout_matrix.Set(layout_matrix);
	}

	void SetDirection(Direction direction)
	{
		if(!_dir_coef) return;
		if(direction == Direction::LeftToRight)
			_dir_coef.Set(+1.0f);
		else if(direction == Direction::RightToLeft)
			_dir_coef.Set(-1.0f);
	}

	void SetAlignOffset(GLfloat offset)
	{
		if(!_align_coef) return;
		_align_coef.Set(offset);
	}

	void SetAlignment(Alignment alignment)
	{
		if(alignment == Alignment::Left)
			SetAlignOffset(+0.5);
		else if(alignment == Alignment::Center)
			SetAlignOffset( 0.0f);
		else if(alignment == Alignment::Right)
			SetAlignOffset(-0.5f);
	}
};

} // namespace text
} // namespace oglplus

#endif // include guard
