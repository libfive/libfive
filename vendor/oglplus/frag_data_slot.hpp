/**
 *  @file oglplus/frag_data_slot.hpp
 *  @brief FragDataSlot object
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_FRAG_DATA_SLOT_1405030825_HPP
#define OGLPLUS_FRAG_DATA_SLOT_1405030825_HPP

#include <oglplus/limited_value.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the fragment data output slot (implementation-dependent limited) number
/**
 *  @see FragData
 *
 *  @ingroup shader_variables
 */
class FragDataSlot
 : public LimitedCount
{
public:
	FragDataSlot(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	FragDataSlot,
	MAX_DRAW_BUFFERS
)
#endif

typedef FragDataSlot FragmentDataSlot;

} // namespace oglplus

#endif // include guard
