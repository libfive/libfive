/**
 *  @file oglplus/texture_unit.hpp
 *  @brief Texture and image unit selectors
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXTURE_UNIT_1107121519_HPP
#define OGLPLUS_TEXTURE_UNIT_1107121519_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the texture unit selector (implementation-dependent limited) number
class TextureUnitSelector
 : public LimitedCount
{
public:
	TextureUnitSelector(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	TextureUnitSelector,
	MAX_COMBINED_TEXTURE_IMAGE_UNITS
)
#endif

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the image unit selector (implementation-dependent limited) number
class ImageUnitSelector
 : public LimitedCount
{
public:
	ImageUnitSelector(GLuint count);
};
#elif GL_VERSION_4_2 || GL_ARB_shader_image_load_store
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	ImageUnitSelector,
	MAX_IMAGE_UNITS
)
#else
typedef GLuint ImageUnitSelector;
#endif

} // namespace oglplus

#endif // include guard
