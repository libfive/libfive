//  File implement/oglplus/enums/ext/nv_path_join_style_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_join_style.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVJoinStyle> class Transform>
class EnumToClass<Base, PathNVJoinStyle, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<PathNVJoinStyle::None> None;
#  pragma pop_macro("None")
# else
	Transform<PathNVJoinStyle::None> None;
# endif
#endif
#if defined GL_ROUND_NV
# if defined Round
#  pragma push_macro("Round")
#  undef Round
	Transform<PathNVJoinStyle::Round> Round;
#  pragma pop_macro("Round")
# else
	Transform<PathNVJoinStyle::Round> Round;
# endif
#endif
#if defined GL_BEVEL_NV
# if defined Bevel
#  pragma push_macro("Bevel")
#  undef Bevel
	Transform<PathNVJoinStyle::Bevel> Bevel;
#  pragma pop_macro("Bevel")
# else
	Transform<PathNVJoinStyle::Bevel> Bevel;
# endif
#endif
#if defined GL_MITER_REVERT_NV
# if defined MiterRevert
#  pragma push_macro("MiterRevert")
#  undef MiterRevert
	Transform<PathNVJoinStyle::MiterRevert> MiterRevert;
#  pragma pop_macro("MiterRevert")
# else
	Transform<PathNVJoinStyle::MiterRevert> MiterRevert;
# endif
#endif
#if defined GL_MITER_TRUNCATE_NV
# if defined MiterTruncate
#  pragma push_macro("MiterTruncate")
#  undef MiterTruncate
	Transform<PathNVJoinStyle::MiterTruncate> MiterTruncate;
#  pragma pop_macro("MiterTruncate")
# else
	Transform<PathNVJoinStyle::MiterTruncate> MiterTruncate;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	 , None(_base())
#  pragma pop_macro("None")
# else
	 , None(_base())
# endif
#endif
#if defined GL_ROUND_NV
# if defined Round
#  pragma push_macro("Round")
#  undef Round
	 , Round(_base())
#  pragma pop_macro("Round")
# else
	 , Round(_base())
# endif
#endif
#if defined GL_BEVEL_NV
# if defined Bevel
#  pragma push_macro("Bevel")
#  undef Bevel
	 , Bevel(_base())
#  pragma pop_macro("Bevel")
# else
	 , Bevel(_base())
# endif
#endif
#if defined GL_MITER_REVERT_NV
# if defined MiterRevert
#  pragma push_macro("MiterRevert")
#  undef MiterRevert
	 , MiterRevert(_base())
#  pragma pop_macro("MiterRevert")
# else
	 , MiterRevert(_base())
# endif
#endif
#if defined GL_MITER_TRUNCATE_NV
# if defined MiterTruncate
#  pragma push_macro("MiterTruncate")
#  undef MiterTruncate
	 , MiterTruncate(_base())
#  pragma pop_macro("MiterTruncate")
# else
	 , MiterTruncate(_base())
# endif
#endif
	{ }
};

} // namespace enums

