/**
 *  @file oglplus/text/pango_cairo/fwd.hpp
 *  @brief Pango/Cairo-based text rendering - forward declarations.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_PANGO_CAIRO_FWD_HPP
#define OGLPLUS_TEXT_PANGO_CAIRO_FWD_HPP

#include <oglplus/texture_unit.hpp>

namespace oglplus {
namespace text {

struct PangoCairoLayoutData;

class PangoCairoFont;
class PangoCairoLayout;
class PangoCairoRenderer;

class PangoCairoRendering;

void PangoCairoAllocateLayoutData(
	PangoCairoRendering& parent,
	PangoCairoLayoutData& layout_data,
	SizeType width,
	SizeType height
);

void PangoCairoDeallocateLayoutData(
	PangoCairoRendering& parent,
	PangoCairoLayoutData& layout_data
);

void PangoCairoInitializeLayoutData(
	PangoCairoRendering& parent,
	PangoCairoLayoutData& layout_data,
	SizeType width,
	SizeType height,
	const void* raw_data
);

TextureUnitSelector PangoCairoUseLayoutData(
	PangoCairoRendering& parent,
	const PangoCairoLayoutData& layout_data
);

} // namespace text
} // namespace oglplus

#endif // include guard
