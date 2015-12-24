/**
 *  @file oglplus/text/pango_cairo/layout_storage.hpp
 *  @brief Pango/Cairo-based text rendering - layout storage.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_PANGO_CAIRO_LAYOUT_STORAGE_HPP
#define OGLPLUS_TEXT_PANGO_CAIRO_LAYOUT_STORAGE_HPP

#include <oglplus/text/common.hpp>
#include <oglplus/text/pango_cairo/fwd.hpp>
#include <oglplus/texture.hpp>

namespace oglplus {
namespace text {

struct PangoCairoLayoutData
{
	Texture _storage;

	PangoCairoLayoutData(void)
	{ }

	PangoCairoLayoutData(PangoCairoLayoutData&& tmp)
	 : _storage(std::move(tmp._storage))
	{ }
};

class PangoCairoLayoutStorage
{
private:
	// TODO
public:
	PangoCairoLayoutStorage(void)
	{ }

};

} // namespace text
} // namespace oglplus

#endif // include guard
