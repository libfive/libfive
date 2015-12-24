/**
 *  @file oglplus/config/object.hpp
 *  @brief Object-related compile-time configuration options
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONFIG_OBJECT_1107121519_HPP
#define OGLPLUS_CONFIG_OBJECT_1107121519_HPP

#include <oglplus/config/basic.hpp>

#if OGLPLUS_DOCUMENTATION_ONLY
/// Compile-time switch disabling textual object descriptions
/** Setting this preprocessor option to a non-zero integer value
 *  disables the @ref oglplus_object_description attached to
 *  various specializations of @c Object (like Program, Shader,
 *  Texture, etc.) during construction by the means of the ObjectDesc
 *  parameter in constructor of Object.
 *
 *  By default this option is set to the same value as #OGLPLUS_LOW_PROFILE,
 *  i.e. objects descriptions are enabled, when not in low-profile mode
 *  and disabled otherwise.
 *
 *  @note Object descriptions use statically initialized data which
 *  may cause problems if the final executable is built together from
 *  several different object files. Because of this, if object descriptions
 *  are enabled it is recommended that OGLplus applications are built with
 *  #OGLPLUS_LINK_LIBRARY set to non-zero or are built as a single translation
 *  unit.
 *
 *  @see OGLPLUS_LINK_LIBRARY
 *
 *  @ingroup compile_time_config
 */
#define OGLPLUS_NO_OBJECT_DESC
#else
# ifndef OGLPLUS_NO_OBJECT_DESC
#  define OGLPLUS_NO_OBJECT_DESC OGLPLUS_LOW_PROFILE
# endif
#endif

#endif // include guard
