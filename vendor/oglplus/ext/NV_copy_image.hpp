/**
 *  @file oglplus/ext/NV_copy_image.hpp
 *  @brief Wrapper for the NV_copy_image extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_NV_COPY_IMAGE_1406270722_HPP
#define OGLPLUS_EXT_NV_COPY_IMAGE_1406270722_HPP

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_copy_image

#include <oglplus/object/name.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/extension.hpp>

namespace oglplus {

/// Wrapper for the NV_copy_image extension
/**
 *  @glsymbols
 *  @glextref{NV,copy_image}
 *
 *  @ingroup gl_extensions
 */
class NV_copy_image
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
	OGLPLUS_EXTENSION_CLASS(NV, copy_image)

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
		OGLPLUS_GLFUNC(CopyImageSubDataNV)(
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
			CopyImageSubDataNV,
			ObjectPairError,
			Subject(dst_name).
			SubjectBinding(dst_target).
			Object(src_name).
			ObjectBinding(src_target)
		);
	}

#if OGLPLUS_NATIVE_GLX
	template <typename SrcObjTag, typename DstObjTag>
	static void CopyImageSubData(
		const native::ContextGLX& src_context,
		ObjectName<SrcObjTag> src_name,
		typename ObjBindingOps<SrcObjTag>::Target src_target,
		GLint src_level,
		GLint src_x, GLint src_y, GLint src_z,
		const native::ContextGLX& dst_context,
		ObjectName<DstObjTag> dst_name,
		typename ObjBindingOps<DstObjTag>::Target dst_target,
		GLint dst_level,
		GLint dst_x, GLint dst_y, GLint dst_z,
		SizeType width, SizeType height, SizeType depth
	)
	{
		using native::GetGLXContext;

		_chk_params<SrcObjTag, DstObjTag>();
		OGLPLUS_GLXFUNC(CopyImageSubDataNV)(
			GetGLXDisplay(src_context),
			GetGLXContext(src_context),
			GetGLName(src_name),
			GLenum(src_target),
			src_level,
			src_x, src_y, src_z,
			GetGLXContext(dst_context),
			GetGLName(dst_name),
			GLenum(dst_target),
			dst_level,
			dst_x, dst_y, dst_z,
			width, height, depth
		);
		OGLPLUS_HANDLE_ERROR_IF(
			error_code != GL_NO_ERROR,
			OGLPLUS_GLFUNC(GetError)(),
			ObjectPairError::Message(error_code),
			ObjectPairError,
			Subject(dst_name).
			SubjectBinding(dst_target).
			Object(src_name).
			ObjectBinding(src_target).
			GLLib_glX().
			GLFunc("CopyImageSubData")
		);
	}
#endif // OGLPLUS_NATIVE_GLX

#if OGLPLUS_NATIVE_WGL
	template <typename SrcObjTag, typename DstObjTag>
	static void CopyImageSubData(
		const native::ContextWGL& src_context,
		ObjectName<SrcObjTag> src_name,
		typename ObjBindingOps<SrcObjTag>::Target src_target,
		GLint src_level,
		GLint src_x, GLint src_y, GLint src_z,
		const native::ContextWGL& dst_context,
		ObjectName<DstObjTag> dst_name,
		typename ObjBindingOps<DstObjTag>::Target dst_target,
		GLint dst_level,
		GLint dst_x, GLint dst_y, GLint dst_z,
		SizeType width, SizeType height, SizeType depth
	)
	{
		using native::GetHGLRC;

		_chk_params<SrcObjTag, DstObjTag>();
		OGLPLUS_WGLFUNC(CopyImageSubDataNV)(
			GetHGLRC(src_context),
			GetGLName(src_name),
			GLenum(src_target),
			src_level,
			src_x, src_y, src_z,
			GetHGLRC(dst_context),
			GetGLName(dst_name),
			GLenum(dst_target),
			dst_level,
			dst_x, dst_y, dst_z,
			width, height, depth
		);
		OGLPLUS_HANDLE_ERROR_IF(
			error_code != GL_NO_ERROR,
			OGLPLUS_GLFUNC(GetError)(),
			ObjectPairError::Message(error_code),
			ObjectPairError,
			Subject(dst_name).
			SubjectBinding(dst_target).
			Object(src_name).
			ObjectBinding(src_target).
			GLLib_wgl().
			GLFunc("CopyImageSubData")
		);
	}
#endif // OGLPLUS_NATIVE_WGL
};

} // namespace oglplus

#endif // NV_copy_image

#endif // include guard
