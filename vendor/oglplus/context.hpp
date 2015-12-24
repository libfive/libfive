/**
 *  @file oglplus/context.hpp
 *  @brief Declaration of OpenGL's state wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_1107121317_HPP
#define OGLPLUS_CONTEXT_1107121317_HPP

#include <oglplus/context/errors.hpp>
#include <oglplus/context/capabilities.hpp>
#include <oglplus/context/viewport.hpp>
#include <oglplus/context/clip_control.hpp>
#include <oglplus/context/buffer_selection.hpp>
#include <oglplus/context/buffer_masking.hpp>
#include <oglplus/context/buffer_clearing.hpp>
#include <oglplus/context/rasterization.hpp>
#include <oglplus/context/drawing.hpp>
#include <oglplus/context/computing.hpp>
#include <oglplus/context/depth_test.hpp>
#include <oglplus/context/stencil_test.hpp>
#include <oglplus/context/blending.hpp>
#include <oglplus/context/logic_ops.hpp>
#include <oglplus/context/scissor_test.hpp>
#include <oglplus/context/pixel_ops.hpp>
#include <oglplus/context/synchronization.hpp>
#include <oglplus/context/hints.hpp>
#include <oglplus/context/limit_queries.hpp>
#include <oglplus/context/numeric_queries.hpp>
#include <oglplus/context/string_queries.hpp>
#include <oglplus/context/object_binding.hpp>
#include <oglplus/context/object_dsa.hpp>

#include <oglplus/face_mode.hpp>
#include <oglplus/compare_function.hpp>


namespace oglplus {

/// Namespace containing definitions of GL context-related operation wrappers
namespace context { }

/** @defgroup ogl_context Current OpenGL context operations
 *
 *  Here are listed classes and functions wrapping operations on the current
 *  OpenGL context, which are not related to any explicitly allocated
 *  object or resource, i.e. operations like enabling and disabling
 *  various OpenGL oglplus::Capability "capabilities" and changing the state
 *  values of the current context.
 */

/// Wrapper for the current OpenGL context operations
/**
 *  @ingroup ogl_context
 */
class Context
 : public context::Errors
 , public context::Capabilities
 , public context::ViewportState
 , public context::ViewportOps
 , public context::ClipControlState
 , public context::BufferSelection
 , public context::BufferMaskingState
 , public context::BufferClearingState
 , public context::BufferClearingOps
 , public context::RasterizationState
 , public context::RasterizationOps
 , public context::DrawingOps
 , public context::DrawingState
 , public context::ComputingOps
 , public context::DepthTest
 , public context::StencilTest
 , public context::ScissorTest
 , public context::LogicOpState
 , public context::PixelState
 , public context::PixelOps
 , public context::BlendingOps
 , public context::BlendingState
 , public context::Synchronization
 , public context::Hints
 , public context::LimitQueries
 , public context::NumericQueries
 , public context::StringQueries
 , public context::ObjectBinding
 , public context::ObjectDSA
{ };

} // namespace oglplus

#endif // include guard
