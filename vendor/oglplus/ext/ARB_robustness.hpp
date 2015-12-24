/**
 *  @file oglplus/ext/ARB_robustness.hpp
 *  @brief Wrapper for a subset of the ARB_robustness extension
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
#include <oglplus/ext/ARB_robustness/graphics_reset_status.hpp>
#include <oglplus/ext/ARB_robustness/reset_notif_strategy.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_robustness

/// Wrapper for the ARB_robustness extension
/**
 *  @glsymbols
 *  @glextref{ARB,robustness}
 *
 *  @ingroup gl_extensions
 */
class ARB_robustness
{
public:
	OGLPLUS_EXTENSION_CLASS(ARB, robustness)

	/// Returns the context graphics reset notification strategy
	/**
	 *  @glsymbols
	 *  @glfunref{GetIntegerv}
	 *  @gldefref{RESET_NOTIFICATION_STRATEGY_ARB}
	 */
	static oglplus::ResetNotificationStrategy
	ResetNotificationStrategy(void)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_RESET_NOTIFICATION_STRATEGY_ARB,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::ResetNotificationStrategy(result);
	}

	/// Returns the graphics reset status
	/**
	 *  @glsymbols
	 *  @glfunref{GetGraphicsResetStatusARB}
	 */
	static oglplus::GraphicsResetStatus
	GraphicsResetStatus(void)
	{
		GLenum result = OGLPLUS_GLFUNC(GetGraphicsResetStatusARB)();
		OGLPLUS_VERIFY_SIMPLE(GetGraphicsResetStatusARB);
		return oglplus::GraphicsResetStatus(result);
	}
};
#endif

} // namespace oglplus

#endif // include guard
