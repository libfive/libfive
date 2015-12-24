/**
 *  .file oglplus/prog_var/varpara_fns.hpp
 *  .brief Helper tools for working with OpenGL function "overloads" for various types
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_VARPARA_FNS_1107121519_HPP
#define OGLPLUS_PROG_VAR_VARPARA_FNS_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <type_traits>

namespace oglplus {

#define OGLPLUS_AUX_VARPARA_FNC(NAME, SUFFIX, SELECTOR, TYPE, NPARAM)\
	static inline std::decay< \
		decltype( :: gl ## NAME ## SUFFIX ) \
	>::type _fns_ ## SELECTOR( \
		std::integral_constant<std::size_t, NPARAM>, \
		const TYPE*, ... \
	) \
	{ \
		return OGLPLUS_GLFUNC(NAME ## SUFFIX); \
	}

#define OGLPLUS_AUX_VARPARA_FNS(NAME, SUFFIX, SELECTOR, TYPE) \
	OGLPLUS_AUX_VARPARA_FNC(NAME ## 1, SUFFIX, SELECTOR, TYPE, 1) \
	OGLPLUS_AUX_VARPARA_FNC(NAME ## 2, SUFFIX, SELECTOR, TYPE, 2) \
	OGLPLUS_AUX_VARPARA_FNC(NAME ## 3, SUFFIX, SELECTOR, TYPE, 3) \
	OGLPLUS_AUX_VARPARA_FNC(NAME ## 4, SUFFIX, SELECTOR, TYPE, 4)

#define OGLPLUS_AUX_VARPARA_FNS_EXT(NAME, SUFFIX, EXT, SELECTOR, TYPE) \
	OGLPLUS_AUX_VARPARA_FNS(NAME, SUFFIX##EXT, SELECTOR, TYPE)

#define OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, C, R, CxR)\
	static inline std::decay< \
		decltype( :: gl ## NAME ## CxR ## SUFFIX ) \
	>::type _fns_ ## SELECTOR( \
		std::integral_constant<std::size_t, C>, \
		std::integral_constant<std::size_t, R>, \
		const TYPE*, ... \
	) \
	{ \
		return OGLPLUS_GLFUNC(NAME ## CxR ## SUFFIX); \
	}

#define OGLPLUS_AUX_VARPARA_MAT_FNS(NAME, SUFFIX, SELECTOR, TYPE) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 2, 2, 2) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 2, 3, 2x3) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 2, 4, 2x4) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 3, 2, 3x2) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 3, 3, 3) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 3, 4, 3x4) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 4, 2, 4x2) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 4, 3, 4x3) \
	OGLPLUS_AUX_VARPARA_MAT_FNC(NAME, SUFFIX, SELECTOR, TYPE, 4, 4, 4)

#define OGLPLUS_AUX_VARPARA_MAT_FNS_EXT(NAME, SUFFIX, EXT, SELECTOR, TYPE) \
	OGLPLUS_AUX_VARPARA_MAT_FNS(NAME, SUFFIX##EXT, SELECTOR, TYPE)

} // namespace oglplus

#endif // include guard
