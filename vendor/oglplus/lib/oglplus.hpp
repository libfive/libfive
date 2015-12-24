/**
 *  @file oglplus/lib/oglplus.hpp
 *  @brief All-in-one include file for the separatelly-built oglplus functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_LIB_OGLPLUS_1208310818_HPP
#define OGLPLUS_LIB_OGLPLUS_1208310818_HPP

#ifdef None
# pragma push_macro("None")
# undef None
# define OGLPLUS_None_WAS_DEFINED
#endif

#ifndef OGLPLUS_IMPLEMENTING_LIBRARY
#define OGLPLUS_IMPLEMENTING_LIBRARY 1
#endif

#include <oglplus/fwd.hpp>

#include <oglplus/string/utf8.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/object/desc.hpp>

#include <oglplus/error/basic.hpp>
#include <oglplus/error/framebuffer.hpp>
#include <oglplus/error/limit.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/error/program.hpp>

#include <oglplus/detail/base_range.hpp>
#include <oglplus/detail/info_log.hpp>
#include <oglplus/detail/glsl_source.hpp>

#include <oglplus/utils/xml.hpp>

#include <oglplus/prog_var/typecheck.hpp>

#include <oglplus/images/image.hpp>

#include <oglplus/vertex_attrib.hpp>
#include <oglplus/uniform.hpp>
#include <oglplus/uniform_block.hpp>
#include <oglplus/uniform_subroutines.hpp>
#include <oglplus/framebuffer.hpp>
#include <oglplus/renderbuffer.hpp>
#include <oglplus/buffer.hpp>
#include <oglplus/texture.hpp>
#include <oglplus/transform_feedback.hpp>
#include <oglplus/shader.hpp>
#include <oglplus/program.hpp>
#include <oglplus/program_resource.hpp>
#include <oglplus/program_pipeline.hpp>

#include <oglplus/shader_storage_block.hpp>

#include <oglplus/imports/blend_file.hpp>

#include <oglplus/shapes/cage.hpp>
#include <oglplus/shapes/cube.hpp>
#include <oglplus/shapes/grid.hpp>
#include <oglplus/shapes/icosahedron.hpp>
#include <oglplus/shapes/plane.hpp>
#include <oglplus/shapes/torus.hpp>
#include <oglplus/shapes/sphere.hpp>
#include <oglplus/shapes/subdiv_sphere.hpp>
#include <oglplus/shapes/spiral_sphere.hpp>
#include <oglplus/shapes/tetrahedrons.hpp>
#include <oglplus/shapes/twisted_torus.hpp>
#include <oglplus/shapes/wicker_torus.hpp>

#include <oglplus/shapes/blender_mesh.hpp>
#include <oglplus/shapes/obj_mesh.hpp>

#include <oglplus/shapes/draw.hpp>
#include <oglplus/shapes/wrapper.hpp>
#include <oglplus/shapes/analyzer.hpp>

#include <oglplus/images/brushed_metal.hpp>
#include <oglplus/images/checker.hpp>
#include <oglplus/images/metaballs.hpp>
#include <oglplus/images/cloud.hpp>
#include <oglplus/images/squares.hpp>
#include <oglplus/images/sphere_bmap.hpp>
#include <oglplus/images/normal_map.hpp>
#include <oglplus/images/random.hpp>
#include <oglplus/images/xpm.hpp>
#include <oglplus/images/sort_nw.hpp>
#include <oglplus/images/voronoi.hpp>
#include <oglplus/images/worley.hpp>

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects || GL_EXT_direct_state_access
#include <oglplus/text/unicode.hpp>
#endif
#endif //OGLPLUS_NO_VARIADIC_TEMPLATES

#include <oglplus/opt/resources.hpp>

#include <oglplus/ext/ARB_debug_output.hpp>

#if GL_EXT_direct_state_access
#include <oglplus/dsa/ext/buffer.hpp>
#include <oglplus/dsa/ext/framebuffer.hpp>
#include <oglplus/dsa/ext/renderbuffer.hpp>
#include <oglplus/dsa/ext/texture.hpp>
#endif

#include <oglplus/native/common_glx.hpp>
#include <oglplus/native/common_wgl.hpp>

namespace oglplus {

// EnumValueName implementations
#include <oglplus/lib/enum_value_name.ipp>
// EnumValueRange implementations
#include <oglplus/lib/enum_value_range.ipp>

} // namespace oglplus

#undef OGLPLUS_IMPLEMENTING_LIBRARY

#ifdef OGLPLUS_None_WAS_DEFINED
# pragma pop_macro("None")
#endif

#endif // include guard
