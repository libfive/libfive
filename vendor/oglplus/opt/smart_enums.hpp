/**
 *  @file oglplus/opt/smart_enums.hpp
 *  @brief Implements syntax sugar for shortening enumerated values
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_SMART_ENUMS_1204051600_HPP
#define OGLPLUS_OPT_SMART_ENUMS_1204051600_HPP

#include <oglplus/config/compiler.hpp>

namespace oglplus {


/** @defgroup smart_enums Smart enumerations
 *
 *  The @ref enumerations bring additional type safety and robustness to
 *  applications, but also have a potential downside that they require
 *  lengthy specification.
 *  Smart enumerations provide "syntactic sugar" for simpler specification
 *  of strongly-typed enumeration values. See @ref oglplus_smart_enums
 *  for more information.
 */

/** @page oglplus_smart_enums Smart enumerations
 *
 *  @OGLplus defines strongly-typed enumeration types like
 *  @ref oglplus::DataType "DataType", @ref oglplus::ColorBuffer "ColorBuffer",
 *  @ref oglplus::Capability "Capability", and @ref enumerations "many others",
 *  which provide additional type safety and robustness to applications, but
 *  require lenghty enumerated value name specification. This may be viewed
 *  as a downside. The "smart enumerations" are special types that simplify
 *  the usage of enumerations in certain situations without degrading
 *  the type safety.
 *
 *  Instead of explicitly specifying the name of the type of the enumeration:
 *
 *  @code
 *  Texture::MinFilter(TextureMinFilter::Linear);
 *  Texture::MagFilter(TextureMagFilter::Linear);
 *  @endcode
 *
 *  a smart enumeration type can be used like this:
 *
 *  @code
 *  namespace se = oglplus::smart_enums;
 *  ...
 *  Texture::MinFilter(se::Linear());
 *  Texture::MagFilter(se::Linear());
 *  @endcode
 *
 *  or this:
 *
 *  @code
 *  namespace se = oglplus::smart_enums;
 *  se::Linear linear;
 *  ...
 *  Texture::MinFilter(linear);
 *  Texture::MagFilter(linear);
 *  @endcode
 *
 *  @note Smart enumerations are not included by @c oglplus/all.hpp.
 *  To use them the @c oglplus/opt/smart_enums.hpp file must be included.
 */

/// @ref smart_enums are defined in this namespace
/**
 *  @ingroup smart_enums
 */
namespace smart_enums {

#if OGLPLUS_DOCUMENTATION_ONLY
# include <oglplus/detail/enum_shorteners_doc.ipp>
#else

# if !OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS
#  include <oglplus/detail/smart_enums.ipp>
# else
#  error "Smart enumerations require support for function template default args!"
# endif

#endif

} // namespace smart_enums

namespace smart_values
{
#if !OGLPLUS_NO_FUNCTION_TEMPLATE_DEFAULT_ARGS
# include <oglplus/detail/smart_values.ipp>
#endif
} // namespace smart_values
} // namespace oglplus

#endif // include guard
