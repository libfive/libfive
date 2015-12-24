/**
 *  @file oglplus/texture_swizzle.hpp
 *  @brief Texture swizzle-related classes and enumerations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXTURE_SWIZZLE_1107121519_HPP
#define OGLPLUS_TEXTURE_SWIZZLE_1107121519_HPP

#include <oglplus/enums/texture_swizzle_coord.hpp>
#include <oglplus/enums/texture_swizzle.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle
/// A tuple of swizzle values for all texture components
class TextureSwizzleTuple
{
private:
	GLint _values[4];
public:
	const GLint* Values(void) const
	{
		return _values;
	}

	GLint* Values(void)
	{
		return _values;
	}

	/// Default construction
	TextureSwizzleTuple(void)
	{
		_values[0] = GL_RED;
		_values[1] = GL_GREEN;
		_values[2] = GL_BLUE;
		_values[3] = GL_ALPHA;
	}

	/// Specifies modes for all components/coords
	TextureSwizzleTuple(
		TextureSwizzle mode_r,
		TextureSwizzle mode_g,
		TextureSwizzle mode_b,
		TextureSwizzle mode_a
	)
	{
		_values[0] = GLint(GLenum(mode_r));
		_values[1] = GLint(GLenum(mode_g));
		_values[2] = GLint(GLenum(mode_b));
		_values[3] = GLint(GLenum(mode_a));
	}

	/// Sets the swizzle value for red component
	TextureSwizzleTuple& SwizzleR(TextureSwizzle mode)
	{
		_values[0] = GLint(GLenum(mode));
		return *this;
	}

	/// Returns the swizzle value for red component
	TextureSwizzle SwizzleR(void) const
	{
		return TextureSwizzle(_values[0]);
	}

	/// Synonym for SwizzleR
	TextureSwizzleTuple& Red(TextureSwizzle mode)
	{
		return SwizzleR(mode);
	}

	/// Synonym for SwizzleR
	TextureSwizzle Red(void) const
	{
		return SwizzleR();
	}

	/// Sets the swizzle value for green component
	TextureSwizzleTuple& SwizzleG(TextureSwizzle mode)
	{
		_values[1] = GLint(GLenum(mode));
		return *this;
	}

	/// Returns the swizzle value for green component
	TextureSwizzle SwizzleG(void) const
	{
		return TextureSwizzle(_values[1]);
	}

	/// Synonym for SwizzleG
	TextureSwizzleTuple& Green(TextureSwizzle mode)
	{
		return SwizzleG(mode);
	}

	/// Synonym for SwizzleG
	TextureSwizzle Green(void) const
	{
		return SwizzleG();
	}

	/// Sets the swizzle value for blue component
	TextureSwizzleTuple& SwizzleB(TextureSwizzle mode)
	{
		_values[2] = GLint(GLenum(mode));
		return *this;
	}

	/// Returns the swizzle value for blue component
	TextureSwizzle SwizzleB(void) const
	{
		return TextureSwizzle(_values[2]);
	}

	/// Synonym for SwizzleB
	TextureSwizzleTuple& Blue(TextureSwizzle mode)
	{
		return SwizzleB(mode);
	}

	/// Synonym for SwizzleB
	TextureSwizzle Blue(void) const
	{
		return SwizzleB();
	}

	/// Sets the swizzle value for alpha component
	TextureSwizzleTuple& SwizzleA(TextureSwizzle mode)
	{
		_values[3] = GLint(GLenum(mode));
		return *this;
	}

	/// Returns the swizzle value for alpha component
	TextureSwizzle SwizzleA(void) const
	{
		return TextureSwizzle(_values[3]);
	}

	/// Synonym for SwizzleA
	TextureSwizzleTuple& Alpha(TextureSwizzle mode)
	{
		return SwizzleA(mode);
	}

	/// Synonym for SwizzleA
	TextureSwizzle Alpha(void) const
	{
		return SwizzleA();
	}

	/// Sets the swizzle value for the specified component/coord
	TextureSwizzleTuple& Swizzle(
		TextureSwizzleCoord coord,
		TextureSwizzle mode
	)
	{
		switch(GLenum(coord))
		{
			case GL_TEXTURE_SWIZZLE_R:
				SwizzleR(mode);
				break;
			case GL_TEXTURE_SWIZZLE_G:
				SwizzleG(mode);
				break;
			case GL_TEXTURE_SWIZZLE_B:
				SwizzleB(mode);
				break;
			case GL_TEXTURE_SWIZZLE_A:
				SwizzleA(mode);
				break;
		}
		return *this;
	}

	/// Returns the swizzle value for the specified component/coord
	TextureSwizzle Swizzle(TextureSwizzleCoord coord) const
	{
		switch(GLenum(coord))
		{
			case GL_TEXTURE_SWIZZLE_R:
				return SwizzleR();
			case GL_TEXTURE_SWIZZLE_G:
				return SwizzleG();
			case GL_TEXTURE_SWIZZLE_B:
				return SwizzleB();
			case GL_TEXTURE_SWIZZLE_A:
				return SwizzleA();
		}
		return TextureSwizzle();
	}
};
#endif

} // namespace oglplus

#endif // include guard
