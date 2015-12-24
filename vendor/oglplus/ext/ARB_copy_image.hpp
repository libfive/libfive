/**
 *  @file oglplus/ext/ARB_copy_image.hpp
 *  @brief Wrapper for the ARB_copy_image extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_ARB_COPY_IMAGE_1406270722_HPP
#define OGLPLUS_EXT_ARB_COPY_IMAGE_1406270722_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_copy_image

#include <oglplus/object/name.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/extension.hpp>

namespace oglplus {

/// Wrapper for the ARB_copy_image extension
/**
 *  @glsymbols
 *  @glextref{ARB,copy_image}
 *
 *  @ingroup gl_extensions
 */
class ARB_copy_image
{
private:
	template <typename SrcObjTag, typename DstObjTag>
	static void _chk_params(void)
	{
		static_assert(
			std::is_same<SrcObjTag, tag::Texture>() ||
			std::is_same<SrcObjTag, tag::Renderbuffer>(),
			"Source object must be a Texture or a Renderbuffer"
		);
		static_assert(
			std::is_same<DstObjTag, tag::Texture>() ||
			std::is_same<DstObjTag, tag::Renderbuffer>(),
			"Destination object must be a Texture or a Renderbuffer"
		);
	}
public:
	OGLPLUS_EXTENSION_CLASS(ARB, copy_image)

	template <typename SrcObjTag, typename DstObjTag>
	static void CopyImageSubData(
		ObjectName<SrcObjTag> src_name,
		typename ObjBindingOps<SrcObjTag>::Target src_target,
		GLint src_level,
		GLint src_x, GLint src_y, GLint src_z,
		ObjectName<DstObjTag> dst_name,
		typename ObjBindingOps<DstObjTag>::Target dst_target,
		GLint dst_level,
		GLint dst_x, GLint dst_y, GLint dst_z,
		SizeType width, SizeType height, SizeType depth
	)
	{
		_chk_params<SrcObjTag, DstObjTag>();
		OGLPLUS_GLFUNC(CopyImageSubData)(
			GetGLName(src_name),
			GLenum(src_target),
			src_level,
			src_x, src_y, src_z,
			GetGLName(dst_name),
			GLenum(dst_target),
			dst_level,
			dst_x, dst_y, dst_z,
			width, height, depth
		);
		OGLPLUS_CHECK(
			CopyImageSubData,
			ObjectPairError,
			Subject(dst_name).
			SubjectBinding(dst_target).
			Object(src_name).
			ObjectBinding(src_target)
		);
	}
};

} // namespace oglplus

#endif // ARB_copy_image

#endif // include guard
