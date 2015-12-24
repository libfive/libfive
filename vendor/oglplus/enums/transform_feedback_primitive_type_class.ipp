//  File implement/oglplus/enums/transform_feedback_primitive_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/transform_feedback_primitive_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TransformFeedbackPrimitiveType> class Transform>
class EnumToClass<Base, TransformFeedbackPrimitiveType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	Transform<TransformFeedbackPrimitiveType::Triangles> Triangles;
#  pragma pop_macro("Triangles")
# else
	Transform<TransformFeedbackPrimitiveType::Triangles> Triangles;
# endif
#endif
#if defined GL_LINES
# if defined Lines
#  pragma push_macro("Lines")
#  undef Lines
	Transform<TransformFeedbackPrimitiveType::Lines> Lines;
#  pragma pop_macro("Lines")
# else
	Transform<TransformFeedbackPrimitiveType::Lines> Lines;
# endif
#endif
#if defined GL_POINTS
# if defined Points
#  pragma push_macro("Points")
#  undef Points
	Transform<TransformFeedbackPrimitiveType::Points> Points;
#  pragma pop_macro("Points")
# else
	Transform<TransformFeedbackPrimitiveType::Points> Points;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	 , Triangles(_base())
#  pragma pop_macro("Triangles")
# else
	 , Triangles(_base())
# endif
#endif
#if defined GL_LINES
# if defined Lines
#  pragma push_macro("Lines")
#  undef Lines
	 , Lines(_base())
#  pragma pop_macro("Lines")
# else
	 , Lines(_base())
# endif
#endif
#if defined GL_POINTS
# if defined Points
#  pragma push_macro("Points")
#  undef Points
	 , Points(_base())
#  pragma pop_macro("Points")
# else
	 , Points(_base())
# endif
#endif
	{ }
};

} // namespace enums

