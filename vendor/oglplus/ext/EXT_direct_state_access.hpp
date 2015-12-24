/**
 *  @file oglplus/ext/EXT_direct_state_access.hpp
 *  @brief Wrapper for a subset of the EXT_direct_state_access extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_EXT_DIRECT_STATE_ACCESS_1203031902_HPP
#define OGLPLUS_EXT_EXT_DIRECT_STATE_ACCESS_1203031902_HPP

#include <oglplus/extension.hpp>
#include <oglplus/ext/EXT_direct_state_access/matrix.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_EXT_direct_state_access

/// Wrapper for the EXT_direct_state_access extension
/**
 *  @glsymbols
 *  @glextref{EXT,direct_state_access}
 *
 *  @ingroup gl_extensions
 */
class EXT_direct_state_access
{
public:
	OGLPLUS_EXTENSION_CLASS(EXT, direct_state_access)

	/// Returns the DSA wrapper for modelview matrix operations
	static DSAModelviewMatrixEXT ModelviewMatrix(void)
	{
		return DSAModelviewMatrixEXT();
	}

	/// Returns the DSA wrapper for projection matrix operations
	static DSAProjectionMatrixEXT ProjectionMatrix(void)
	{
		return DSAProjectionMatrixEXT();
	}
};
#endif

} // namespace oglplus

#endif // include guard
