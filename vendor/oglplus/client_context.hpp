/**
 *  @file oglplus/client_context.hpp
 *  @brief Client context.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_CONTEXT_1412071213_HPP
#define OGLPLUS_CLIENT_CONTEXT_1412071213_HPP

#include <oglplus/client/object_binding.hpp>
#include <oglplus/client/capabilities.hpp>
#include <oglplus/client/hints.hpp>

#include <oglplus/client/depth_test.hpp>
#include <oglplus/client/logic_ops.hpp>
#include <oglplus/client/viewport.hpp>
#include <oglplus/client/clip_control.hpp>
#include <oglplus/client/stencil_test.hpp>
#include <oglplus/client/scissor_test.hpp>
#include <oglplus/client/rasterization.hpp>
#include <oglplus/client/pixel_ops.hpp>
#include <oglplus/client/blending.hpp>
#include <oglplus/client/buffer_clearing.hpp>
#include <oglplus/client/buffer_masking.hpp>

#include <oglplus/client/drawing.hpp>
#include <oglplus/client/computing.hpp>
#include <oglplus/client/synchronization.hpp>

#include <oglplus/client/limit_queries.hpp>
#include <oglplus/client/numeric_queries.hpp>
#include <oglplus/client/string_queries.hpp>

namespace oglplus {

class ClientContext
 : public client::CapabilityState
 , public client::ViewportState
 , public client::ClipControlState
 , public client::BufferMaskingState
 , public client::BufferClearingOps
 , public client::BufferClearingState
 , public client::RasterizationState
 , public client::RasterizationOps
 , public client::DrawingState
 , public client::DrawingOps
 , public client::ComputingOps
 , public client::DepthTestState
 , public client::StencilTestState
 , public client::ScissorTestState
 , public client::LogicOpState
 , public client::PixelState
 , public client::PixelOps
 , public client::BlendingOps
 , public client::BlendingState
 , public client::Synchronization
 , public client::HintState
 , public client::LimitQueries
 , public client::NumericQueries
 , public client::StringQueries
 , public client::CurrentObjects
{ };

} // namespace oglplus

#endif // include guard
