//  File implement/oglplus/enums/texture_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureTarget> class Transform>
class EnumToClass<Base, TextureTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TEXTURE_1D
# if defined _1D
#  pragma push_macro("_1D")
#  undef _1D
	Transform<TextureTarget::_1D> _1D;
#  pragma pop_macro("_1D")
# else
	Transform<TextureTarget::_1D> _1D;
# endif
#endif
#if defined GL_TEXTURE_2D
# if defined _2D
#  pragma push_macro("_2D")
#  undef _2D
	Transform<TextureTarget::_2D> _2D;
#  pragma pop_macro("_2D")
# else
	Transform<TextureTarget::_2D> _2D;
# endif
#endif
#if defined GL_TEXTURE_3D
# if defined _3D
#  pragma push_macro("_3D")
#  undef _3D
	Transform<TextureTarget::_3D> _3D;
#  pragma pop_macro("_3D")
# else
	Transform<TextureTarget::_3D> _3D;
# endif
#endif
#if defined GL_TEXTURE_1D_ARRAY
# if defined _1DArray
#  pragma push_macro("_1DArray")
#  undef _1DArray
	Transform<TextureTarget::_1DArray> _1DArray;
#  pragma pop_macro("_1DArray")
# else
	Transform<TextureTarget::_1DArray> _1DArray;
# endif
#endif
#if defined GL_TEXTURE_2D_ARRAY
# if defined _2DArray
#  pragma push_macro("_2DArray")
#  undef _2DArray
	Transform<TextureTarget::_2DArray> _2DArray;
#  pragma pop_macro("_2DArray")
# else
	Transform<TextureTarget::_2DArray> _2DArray;
# endif
#endif
#if defined GL_TEXTURE_RECTANGLE
# if defined Rectangle
#  pragma push_macro("Rectangle")
#  undef Rectangle
	Transform<TextureTarget::Rectangle> Rectangle;
#  pragma pop_macro("Rectangle")
# else
	Transform<TextureTarget::Rectangle> Rectangle;
# endif
#endif
#if defined GL_TEXTURE_BUFFER
# if defined Buffer
#  pragma push_macro("Buffer")
#  undef Buffer
	Transform<TextureTarget::Buffer> Buffer;
#  pragma pop_macro("Buffer")
# else
	Transform<TextureTarget::Buffer> Buffer;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP
# if defined CubeMap
#  pragma push_macro("CubeMap")
#  undef CubeMap
	Transform<TextureTarget::CubeMap> CubeMap;
#  pragma pop_macro("CubeMap")
# else
	Transform<TextureTarget::CubeMap> CubeMap;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_ARRAY
# if defined CubeMapArray
#  pragma push_macro("CubeMapArray")
#  undef CubeMapArray
	Transform<TextureTarget::CubeMapArray> CubeMapArray;
#  pragma pop_macro("CubeMapArray")
# else
	Transform<TextureTarget::CubeMapArray> CubeMapArray;
# endif
#endif
#if defined GL_TEXTURE_2D_MULTISAMPLE
# if defined _2DMultisample
#  pragma push_macro("_2DMultisample")
#  undef _2DMultisample
	Transform<TextureTarget::_2DMultisample> _2DMultisample;
#  pragma pop_macro("_2DMultisample")
# else
	Transform<TextureTarget::_2DMultisample> _2DMultisample;
# endif
#endif
#if defined GL_TEXTURE_2D_MULTISAMPLE_ARRAY
# if defined _2DMultisampleArray
#  pragma push_macro("_2DMultisampleArray")
#  undef _2DMultisampleArray
	Transform<TextureTarget::_2DMultisampleArray> _2DMultisampleArray;
#  pragma pop_macro("_2DMultisampleArray")
# else
	Transform<TextureTarget::_2DMultisampleArray> _2DMultisampleArray;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_X
# if defined CubeMapPositiveX
#  pragma push_macro("CubeMapPositiveX")
#  undef CubeMapPositiveX
	Transform<TextureTarget::CubeMapPositiveX> CubeMapPositiveX;
#  pragma pop_macro("CubeMapPositiveX")
# else
	Transform<TextureTarget::CubeMapPositiveX> CubeMapPositiveX;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_X
# if defined CubeMapNegativeX
#  pragma push_macro("CubeMapNegativeX")
#  undef CubeMapNegativeX
	Transform<TextureTarget::CubeMapNegativeX> CubeMapNegativeX;
#  pragma pop_macro("CubeMapNegativeX")
# else
	Transform<TextureTarget::CubeMapNegativeX> CubeMapNegativeX;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_Y
# if defined CubeMapPositiveY
#  pragma push_macro("CubeMapPositiveY")
#  undef CubeMapPositiveY
	Transform<TextureTarget::CubeMapPositiveY> CubeMapPositiveY;
#  pragma pop_macro("CubeMapPositiveY")
# else
	Transform<TextureTarget::CubeMapPositiveY> CubeMapPositiveY;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
# if defined CubeMapNegativeY
#  pragma push_macro("CubeMapNegativeY")
#  undef CubeMapNegativeY
	Transform<TextureTarget::CubeMapNegativeY> CubeMapNegativeY;
#  pragma pop_macro("CubeMapNegativeY")
# else
	Transform<TextureTarget::CubeMapNegativeY> CubeMapNegativeY;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_Z
# if defined CubeMapPositiveZ
#  pragma push_macro("CubeMapPositiveZ")
#  undef CubeMapPositiveZ
	Transform<TextureTarget::CubeMapPositiveZ> CubeMapPositiveZ;
#  pragma pop_macro("CubeMapPositiveZ")
# else
	Transform<TextureTarget::CubeMapPositiveZ> CubeMapPositiveZ;
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
# if defined CubeMapNegativeZ
#  pragma push_macro("CubeMapNegativeZ")
#  undef CubeMapNegativeZ
	Transform<TextureTarget::CubeMapNegativeZ> CubeMapNegativeZ;
#  pragma pop_macro("CubeMapNegativeZ")
# else
	Transform<TextureTarget::CubeMapNegativeZ> CubeMapNegativeZ;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TEXTURE_1D
# if defined _1D
#  pragma push_macro("_1D")
#  undef _1D
	 , _1D(_base())
#  pragma pop_macro("_1D")
# else
	 , _1D(_base())
# endif
#endif
#if defined GL_TEXTURE_2D
# if defined _2D
#  pragma push_macro("_2D")
#  undef _2D
	 , _2D(_base())
#  pragma pop_macro("_2D")
# else
	 , _2D(_base())
# endif
#endif
#if defined GL_TEXTURE_3D
# if defined _3D
#  pragma push_macro("_3D")
#  undef _3D
	 , _3D(_base())
#  pragma pop_macro("_3D")
# else
	 , _3D(_base())
# endif
#endif
#if defined GL_TEXTURE_1D_ARRAY
# if defined _1DArray
#  pragma push_macro("_1DArray")
#  undef _1DArray
	 , _1DArray(_base())
#  pragma pop_macro("_1DArray")
# else
	 , _1DArray(_base())
# endif
#endif
#if defined GL_TEXTURE_2D_ARRAY
# if defined _2DArray
#  pragma push_macro("_2DArray")
#  undef _2DArray
	 , _2DArray(_base())
#  pragma pop_macro("_2DArray")
# else
	 , _2DArray(_base())
# endif
#endif
#if defined GL_TEXTURE_RECTANGLE
# if defined Rectangle
#  pragma push_macro("Rectangle")
#  undef Rectangle
	 , Rectangle(_base())
#  pragma pop_macro("Rectangle")
# else
	 , Rectangle(_base())
# endif
#endif
#if defined GL_TEXTURE_BUFFER
# if defined Buffer
#  pragma push_macro("Buffer")
#  undef Buffer
	 , Buffer(_base())
#  pragma pop_macro("Buffer")
# else
	 , Buffer(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP
# if defined CubeMap
#  pragma push_macro("CubeMap")
#  undef CubeMap
	 , CubeMap(_base())
#  pragma pop_macro("CubeMap")
# else
	 , CubeMap(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_ARRAY
# if defined CubeMapArray
#  pragma push_macro("CubeMapArray")
#  undef CubeMapArray
	 , CubeMapArray(_base())
#  pragma pop_macro("CubeMapArray")
# else
	 , CubeMapArray(_base())
# endif
#endif
#if defined GL_TEXTURE_2D_MULTISAMPLE
# if defined _2DMultisample
#  pragma push_macro("_2DMultisample")
#  undef _2DMultisample
	 , _2DMultisample(_base())
#  pragma pop_macro("_2DMultisample")
# else
	 , _2DMultisample(_base())
# endif
#endif
#if defined GL_TEXTURE_2D_MULTISAMPLE_ARRAY
# if defined _2DMultisampleArray
#  pragma push_macro("_2DMultisampleArray")
#  undef _2DMultisampleArray
	 , _2DMultisampleArray(_base())
#  pragma pop_macro("_2DMultisampleArray")
# else
	 , _2DMultisampleArray(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_X
# if defined CubeMapPositiveX
#  pragma push_macro("CubeMapPositiveX")
#  undef CubeMapPositiveX
	 , CubeMapPositiveX(_base())
#  pragma pop_macro("CubeMapPositiveX")
# else
	 , CubeMapPositiveX(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_X
# if defined CubeMapNegativeX
#  pragma push_macro("CubeMapNegativeX")
#  undef CubeMapNegativeX
	 , CubeMapNegativeX(_base())
#  pragma pop_macro("CubeMapNegativeX")
# else
	 , CubeMapNegativeX(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_Y
# if defined CubeMapPositiveY
#  pragma push_macro("CubeMapPositiveY")
#  undef CubeMapPositiveY
	 , CubeMapPositiveY(_base())
#  pragma pop_macro("CubeMapPositiveY")
# else
	 , CubeMapPositiveY(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
# if defined CubeMapNegativeY
#  pragma push_macro("CubeMapNegativeY")
#  undef CubeMapNegativeY
	 , CubeMapNegativeY(_base())
#  pragma pop_macro("CubeMapNegativeY")
# else
	 , CubeMapNegativeY(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_POSITIVE_Z
# if defined CubeMapPositiveZ
#  pragma push_macro("CubeMapPositiveZ")
#  undef CubeMapPositiveZ
	 , CubeMapPositiveZ(_base())
#  pragma pop_macro("CubeMapPositiveZ")
# else
	 , CubeMapPositiveZ(_base())
# endif
#endif
#if defined GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
# if defined CubeMapNegativeZ
#  pragma push_macro("CubeMapNegativeZ")
#  undef CubeMapNegativeZ
	 , CubeMapNegativeZ(_base())
#  pragma pop_macro("CubeMapNegativeZ")
# else
	 , CubeMapNegativeZ(_base())
# endif
#endif
	{ }
};

} // namespace enums

