/**
 *  @file oglplus/lib/images_png.hpp
 *  @brief All-in-one include file for the separatelly-built png-lib functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_LIB_PNG_1208310818_HPP
#define OGLPLUS_LIB_PNG_1208310818_HPP

#ifdef None
# pragma push_macro("None")
# undef None
# define OGLPLUS_None_WAS_DEFINED
#endif

#ifndef OGLPLUS_IMPLEMENTING_LIBRARY
#define OGLPLUS_IMPLEMENTING_LIBRARY 1
#endif

#if OGLPLUS_PNG_FOUND
#include <oglplus/images/png.hpp>
#endif

#undef OGLPLUS_IMPLEMENTING_LIBRARY

#ifdef OGLPLUS_None_WAS_DEFINED
# pragma pop_macro("None")
#endif

#endif // include guard
