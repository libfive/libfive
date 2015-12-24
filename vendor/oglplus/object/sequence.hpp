/**
 *  @file oglplus/object/sequence.hpp
 *  @brief Sequence of Object names
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_SEQUENCE_1405011014_HPP
#define OGLPLUS_OBJECT_SEQUENCE_1405011014_HPP

#include <oglplus/object/seq_tpl.hpp>

namespace oglplus {

/// Returns a pointer to array of GL object names stored in a @p sequence
template <typename ObjName>
inline const GLuint*
GetGLNames(const Sequence<ObjName>& sequence)
{
	return GetNames(sequence);
}

} // namespace oglplus

#endif // include guard
