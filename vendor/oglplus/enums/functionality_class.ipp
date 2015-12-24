//  File implement/oglplus/enums/functionality_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/functionality.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<Functionality> class Transform>
class EnumToClass<Base, Functionality, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CLIP_DISTANCE0
# if defined ClipDistance
#  pragma push_macro("ClipDistance")
#  undef ClipDistance
	Transform<Functionality::ClipDistance> ClipDistance;
#  pragma pop_macro("ClipDistance")
# else
	Transform<Functionality::ClipDistance> ClipDistance;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CLIP_DISTANCE0
# if defined ClipDistance
#  pragma push_macro("ClipDistance")
#  undef ClipDistance
	 , ClipDistance(_base())
#  pragma pop_macro("ClipDistance")
# else
	 , ClipDistance(_base())
# endif
#endif
	{ }
};

} // namespace enums

