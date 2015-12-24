//  File implement/oglplus/enums/ext/nv_path_dash_offset_reset_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_dash_offset_reset.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVDashOffsetReset> class Transform>
class EnumToClass<Base, PathNVDashOffsetReset, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_MOVE_TO_RESET_NV
# if defined MoveToReset
#  pragma push_macro("MoveToReset")
#  undef MoveToReset
	Transform<PathNVDashOffsetReset::MoveToReset> MoveToReset;
#  pragma pop_macro("MoveToReset")
# else
	Transform<PathNVDashOffsetReset::MoveToReset> MoveToReset;
# endif
#endif
#if defined GL_MOVE_TO_CONTINUES_NV
# if defined MoveToContinues
#  pragma push_macro("MoveToContinues")
#  undef MoveToContinues
	Transform<PathNVDashOffsetReset::MoveToContinues> MoveToContinues;
#  pragma pop_macro("MoveToContinues")
# else
	Transform<PathNVDashOffsetReset::MoveToContinues> MoveToContinues;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_MOVE_TO_RESET_NV
# if defined MoveToReset
#  pragma push_macro("MoveToReset")
#  undef MoveToReset
	 , MoveToReset(_base())
#  pragma pop_macro("MoveToReset")
# else
	 , MoveToReset(_base())
# endif
#endif
#if defined GL_MOVE_TO_CONTINUES_NV
# if defined MoveToContinues
#  pragma push_macro("MoveToContinues")
#  undef MoveToContinues
	 , MoveToContinues(_base())
#  pragma pop_macro("MoveToContinues")
# else
	 , MoveToContinues(_base())
# endif
#endif
	{ }
};

} // namespace enums

