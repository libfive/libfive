//  File implement/oglplus/enums/ext/nv_path_transform_type_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_transform_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	PathNVTransformType
> ValueRange_(PathNVTransformType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVTRANSFORMTYPE)
#define OGLPLUS_IMPL_EVR_PATHNVTRANSFORMTYPE
{
static const GLenum _values[] = {
#if defined GL_NONE
GL_NONE,
#endif
#if defined GL_TRANSLATE_X_NV
GL_TRANSLATE_X_NV,
#endif
#if defined GL_TRANSLATE_Y_NV
GL_TRANSLATE_Y_NV,
#endif
#if defined GL_TRANSLATE_2D_NV
GL_TRANSLATE_2D_NV,
#endif
#if defined GL_TRANSLATE_3D_NV
GL_TRANSLATE_3D_NV,
#endif
#if defined GL_AFFINE_2D_NV
GL_AFFINE_2D_NV,
#endif
#if defined GL_AFFINE_3D_NV
GL_AFFINE_3D_NV,
#endif
#if defined GL_TRANSPOSE_AFFINE_2D_NV
GL_TRANSPOSE_AFFINE_2D_NV,
#endif
#if defined GL_TRANSPOSE_AFFINE_3D_NV
GL_TRANSPOSE_AFFINE_3D_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVTransformType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums

