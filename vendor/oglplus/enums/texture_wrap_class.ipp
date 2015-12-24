//  File implement/oglplus/enums/texture_wrap_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_wrap.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureWrap> class Transform>
class EnumToClass<Base, TextureWrap, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CLAMP_TO_EDGE
# if defined ClampToEdge
#  pragma push_macro("ClampToEdge")
#  undef ClampToEdge
	Transform<TextureWrap::ClampToEdge> ClampToEdge;
#  pragma pop_macro("ClampToEdge")
# else
	Transform<TextureWrap::ClampToEdge> ClampToEdge;
# endif
#endif
#if defined GL_REPEAT
# if defined Repeat
#  pragma push_macro("Repeat")
#  undef Repeat
	Transform<TextureWrap::Repeat> Repeat;
#  pragma pop_macro("Repeat")
# else
	Transform<TextureWrap::Repeat> Repeat;
# endif
#endif
#if defined GL_CLAMP_TO_BORDER
# if defined ClampToBorder
#  pragma push_macro("ClampToBorder")
#  undef ClampToBorder
	Transform<TextureWrap::ClampToBorder> ClampToBorder;
#  pragma pop_macro("ClampToBorder")
# else
	Transform<TextureWrap::ClampToBorder> ClampToBorder;
# endif
#endif
#if defined GL_MIRRORED_REPEAT
# if defined MirroredRepeat
#  pragma push_macro("MirroredRepeat")
#  undef MirroredRepeat
	Transform<TextureWrap::MirroredRepeat> MirroredRepeat;
#  pragma pop_macro("MirroredRepeat")
# else
	Transform<TextureWrap::MirroredRepeat> MirroredRepeat;
# endif
#endif
#if defined GL_MIRROR_CLAMP_TO_EDGE
# if defined MirrorClampToEdge
#  pragma push_macro("MirrorClampToEdge")
#  undef MirrorClampToEdge
	Transform<TextureWrap::MirrorClampToEdge> MirrorClampToEdge;
#  pragma pop_macro("MirrorClampToEdge")
# else
	Transform<TextureWrap::MirrorClampToEdge> MirrorClampToEdge;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CLAMP_TO_EDGE
# if defined ClampToEdge
#  pragma push_macro("ClampToEdge")
#  undef ClampToEdge
	 , ClampToEdge(_base())
#  pragma pop_macro("ClampToEdge")
# else
	 , ClampToEdge(_base())
# endif
#endif
#if defined GL_REPEAT
# if defined Repeat
#  pragma push_macro("Repeat")
#  undef Repeat
	 , Repeat(_base())
#  pragma pop_macro("Repeat")
# else
	 , Repeat(_base())
# endif
#endif
#if defined GL_CLAMP_TO_BORDER
# if defined ClampToBorder
#  pragma push_macro("ClampToBorder")
#  undef ClampToBorder
	 , ClampToBorder(_base())
#  pragma pop_macro("ClampToBorder")
# else
	 , ClampToBorder(_base())
# endif
#endif
#if defined GL_MIRRORED_REPEAT
# if defined MirroredRepeat
#  pragma push_macro("MirroredRepeat")
#  undef MirroredRepeat
	 , MirroredRepeat(_base())
#  pragma pop_macro("MirroredRepeat")
# else
	 , MirroredRepeat(_base())
# endif
#endif
#if defined GL_MIRROR_CLAMP_TO_EDGE
# if defined MirrorClampToEdge
#  pragma push_macro("MirrorClampToEdge")
#  undef MirrorClampToEdge
	 , MirrorClampToEdge(_base())
#  pragma pop_macro("MirrorClampToEdge")
# else
	 , MirrorClampToEdge(_base())
# endif
#endif
	{ }
};

} // namespace enums

