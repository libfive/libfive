/**
 *  .file oglplus/detail/glsl_to_cpp.hpp
 *  .brief Mapping of GLSL types to CPP types
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifndef OGLPLUS_AUX_GLSL_TO_CPP_1102101236_HPP
#define OGLPLUS_AUX_GLSL_TO_CPP_1102101236_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/data_type.hpp>

namespace oglplus {
namespace aux {

template <oglplus::SLDataType>
struct GLSL2Cpp;

#include <oglplus/detail/glsl_to_cpp.ipp>

} // namespace aux

template <oglplus::SLDataType>
struct SLtoCpp;

} // namespace oglplus

#endif
