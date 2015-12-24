//  File implement/oglplus/enums/transform_feedback_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/transform_feedback_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TransformFeedbackTarget> class Transform>
class EnumToClass<Base, TransformFeedbackTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TRANSFORM_FEEDBACK
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	Transform<TransformFeedbackTarget::TransformFeedback> TransformFeedback;
#  pragma pop_macro("TransformFeedback")
# else
	Transform<TransformFeedbackTarget::TransformFeedback> TransformFeedback;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TRANSFORM_FEEDBACK
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	 , TransformFeedback(_base())
#  pragma pop_macro("TransformFeedback")
# else
	 , TransformFeedback(_base())
# endif
#endif
	{ }
};

} // namespace enums

