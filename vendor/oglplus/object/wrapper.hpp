/**
 *  @file oglplus/object/wrapper.hpp
 *  @brief Generic OpenGL object wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_WRAPPER_1107121519_HPP
#define OGLPLUS_OBJECT_WRAPPER_1107121519_HPP

#include <oglplus/object/name.hpp>
#include <oglplus/object/type.hpp>
#include <oglplus/object/wrap_tpl.hpp>

namespace oglplus {

/** @defgroup oglplus_objects OGLplus objects
 *
 *  An @ref oglplus_object is a class wrapping around OpenGL objects
 *  like shaders, programs, textures, etc. It is using the RAII technique
 *  to provide automated resource management for these OpenGL resources.
 *  They also wrap operations related to these objects and provide
 *  additional type safety and more robust error handling.
 */

} // namespace oglplus

#endif // include guard
