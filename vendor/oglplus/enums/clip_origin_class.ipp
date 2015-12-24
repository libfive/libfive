//  File implement/oglplus/enums/clip_origin_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/clip_origin.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ClipOrigin> class Transform>
class EnumToClass<Base, ClipOrigin, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_LOWER_LEFT
# if defined LowerLeft
#  pragma push_macro("LowerLeft")
#  undef LowerLeft
	Transform<ClipOrigin::LowerLeft> LowerLeft;
#  pragma pop_macro("LowerLeft")
# else
	Transform<ClipOrigin::LowerLeft> LowerLeft;
# endif
#endif
#if defined GL_UPPER_LEFT
# if defined UpperLeft
#  pragma push_macro("UpperLeft")
#  undef UpperLeft
	Transform<ClipOrigin::UpperLeft> UpperLeft;
#  pragma pop_macro("UpperLeft")
# else
	Transform<ClipOrigin::UpperLeft> UpperLeft;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_LOWER_LEFT
# if defined LowerLeft
#  pragma push_macro("LowerLeft")
#  undef LowerLeft
	 , LowerLeft(_base())
#  pragma pop_macro("LowerLeft")
# else
	 , LowerLeft(_base())
# endif
#endif
#if defined GL_UPPER_LEFT
# if defined UpperLeft
#  pragma push_macro("UpperLeft")
#  undef UpperLeft
	 , UpperLeft(_base())
#  pragma pop_macro("UpperLeft")
# else
	 , UpperLeft(_base())
# endif
#endif
	{ }
};

} // namespace enums

