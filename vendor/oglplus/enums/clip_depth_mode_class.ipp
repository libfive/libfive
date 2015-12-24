//  File implement/oglplus/enums/clip_depth_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/clip_depth_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ClipDepthMode> class Transform>
class EnumToClass<Base, ClipDepthMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NEGATIVE_ONE_TO_ONE
# if defined NegativeOneToOne
#  pragma push_macro("NegativeOneToOne")
#  undef NegativeOneToOne
	Transform<ClipDepthMode::NegativeOneToOne> NegativeOneToOne;
#  pragma pop_macro("NegativeOneToOne")
# else
	Transform<ClipDepthMode::NegativeOneToOne> NegativeOneToOne;
# endif
#endif
#if defined GL_ZERO_TO_ONE
# if defined ZeroToOne
#  pragma push_macro("ZeroToOne")
#  undef ZeroToOne
	Transform<ClipDepthMode::ZeroToOne> ZeroToOne;
#  pragma pop_macro("ZeroToOne")
# else
	Transform<ClipDepthMode::ZeroToOne> ZeroToOne;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NEGATIVE_ONE_TO_ONE
# if defined NegativeOneToOne
#  pragma push_macro("NegativeOneToOne")
#  undef NegativeOneToOne
	 , NegativeOneToOne(_base())
#  pragma pop_macro("NegativeOneToOne")
# else
	 , NegativeOneToOne(_base())
# endif
#endif
#if defined GL_ZERO_TO_ONE
# if defined ZeroToOne
#  pragma push_macro("ZeroToOne")
#  undef ZeroToOne
	 , ZeroToOne(_base())
#  pragma pop_macro("ZeroToOne")
# else
	 , ZeroToOne(_base())
# endif
#endif
	{ }
};

} // namespace enums

