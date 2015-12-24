/**
 *  @file oglplus/interop/glm.hpp
 *  @brief Interoperability with GLM matrices and vectors.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_INTEROP_GLM_1205181555_HPP
#define OGLPLUS_INTEROP_GLM_1205181555_HPP

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
#include <glm/vec2.hpp>
#include <glm/vec3.hpp>
#include <glm/vec4.hpp>
#include <glm/mat2x2.hpp>
#include <glm/mat2x3.hpp>
#include <glm/mat2x4.hpp>
#include <glm/mat3x2.hpp>
#include <glm/mat3x3.hpp>
#include <glm/mat3x4.hpp>
#include <glm/mat4x2.hpp>
#include <glm/mat4x3.hpp>
#include <glm/mat4x4.hpp>
#include <glm/gtc/type_ptr.hpp>
#pragma GCC diagnostic pop

#include <oglplus/fwd.hpp>

namespace oglplus {

#if	(GLM_VERSION_MAJOR > 0) || ( \
	(GLM_VERSION_MINOR > 9) || ( \
		(GLM_VERSION_MINOR == 9) && \
		(GLM_VERSION_PATCH > 5) \
	))
#define OGLPLUS_GLM_TPLNS ::glm
#else
#define OGLPLUS_GLM_TPLNS ::glm::detail
#endif

#define OGLPLUS_IMPL_GLM_VEC_UNIFORM_OPS(DIM) \
template <typename OpsTag, typename T, glm::precision P> \
class ProgVarGetSetOps<OpsTag, tag::Uniform, OGLPLUS_GLM_TPLNS::tvec##DIM<T, P>> \
 : public ProgVarCommonOps<tag::Uniform> \
 , public ProgVarBaseSetOps<OpsTag, tag::Uniform, tag::NativeTypes, T, 4> \
{ \
protected: \
	ProgVarGetSetOps(UniformLoc uloc) \
	 : ProgVarCommonOps<tag::Uniform>(uloc) \
	{ } \
public: \
	void SetValue(const OGLPLUS_GLM_TPLNS::tvec##DIM<T, P>& value) \
	{ \
		this->template _do_set<DIM>( \
			_program, \
			_location, \
			glm::value_ptr(value) \
		); \
	} \
}; \
template <typename T, glm::precision P> \
struct GLSLtoCppTypeMatcher<OGLPLUS_GLM_TPLNS::tvec##DIM<T, P>> \
 : GLSLtoCppTypeMatcher<oglplus::Vector<T, DIM> > { }; \

OGLPLUS_IMPL_GLM_VEC_UNIFORM_OPS(2)
OGLPLUS_IMPL_GLM_VEC_UNIFORM_OPS(3)
OGLPLUS_IMPL_GLM_VEC_UNIFORM_OPS(4)

#undef OGLPLUS_IMPL_GLM_VEC_UNIFORM_OPS

#define OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(R, C) \
template <typename OpsTag, typename T, glm::precision P> \
class ProgVarGetSetOps<OpsTag, tag::Uniform, OGLPLUS_GLM_TPLNS::tmat##C##x##R<T, P>> \
 : public ProgVarCommonOps<tag::Uniform> \
 , public ProgVarBaseSetOps<OpsTag, tag::Uniform, tag::MatrixTypes, T, 16> \
{ \
protected: \
	ProgVarGetSetOps(UniformLoc uloc) \
	 : ProgVarCommonOps<tag::Uniform>(uloc) \
	{ } \
public: \
	void SetValue(const OGLPLUS_GLM_TPLNS::tmat##C##x##R<T, P>& value) \
	{ \
		this->template _do_set_mat<C, R>( \
			this->_program, \
			this->_location, \
			1, \
			false, \
			glm::value_ptr(value) \
		); \
	} \
}; \
template <typename T, glm::precision P> \
struct GLSLtoCppTypeMatcher<OGLPLUS_GLM_TPLNS::tmat##C##x##R<T, P>> \
 : GLSLtoCppTypeMatcher<oglplus::Matrix<T, R, C> > { }; \

OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(2, 2)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(2, 3)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(2, 4)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(3, 2)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(3, 3)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(3, 4)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(4, 2)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(4, 3)
OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS(4, 4)

#undef OGLPLUS_IMPL_GLM_MAT_UNIFORM_OPS
#undef OGLPLUS_GLM_TPLNS

} // namespace oglplus

#endif // include guard
