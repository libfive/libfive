//  File implement/oglplus/enums/transform_feedback_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/transform_feedback_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TransformFeedbackMode> class Transform>
class EnumToClass<Base, TransformFeedbackMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_INTERLEAVED_ATTRIBS
# if defined InterleavedAttribs
#  pragma push_macro("InterleavedAttribs")
#  undef InterleavedAttribs
	Transform<TransformFeedbackMode::InterleavedAttribs> InterleavedAttribs;
#  pragma pop_macro("InterleavedAttribs")
# else
	Transform<TransformFeedbackMode::InterleavedAttribs> InterleavedAttribs;
# endif
#endif
#if defined GL_SEPARATE_ATTRIBS
# if defined SeparateAttribs
#  pragma push_macro("SeparateAttribs")
#  undef SeparateAttribs
	Transform<TransformFeedbackMode::SeparateAttribs> SeparateAttribs;
#  pragma pop_macro("SeparateAttribs")
# else
	Transform<TransformFeedbackMode::SeparateAttribs> SeparateAttribs;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_INTERLEAVED_ATTRIBS
# if defined InterleavedAttribs
#  pragma push_macro("InterleavedAttribs")
#  undef InterleavedAttribs
	 , InterleavedAttribs(_base())
#  pragma pop_macro("InterleavedAttribs")
# else
	 , InterleavedAttribs(_base())
# endif
#endif
#if defined GL_SEPARATE_ATTRIBS
# if defined SeparateAttribs
#  pragma push_macro("SeparateAttribs")
#  undef SeparateAttribs
	 , SeparateAttribs(_base())
#  pragma pop_macro("SeparateAttribs")
# else
	 , SeparateAttribs(_base())
# endif
#endif
	{ }
};

} // namespace enums

