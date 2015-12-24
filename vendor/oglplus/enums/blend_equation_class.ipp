//  File implement/oglplus/enums/blend_equation_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blend_equation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BlendEquation> class Transform>
class EnumToClass<Base, BlendEquation, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FUNC_ADD
# if defined Add
#  pragma push_macro("Add")
#  undef Add
	Transform<BlendEquation::Add> Add;
#  pragma pop_macro("Add")
# else
	Transform<BlendEquation::Add> Add;
# endif
#endif
#if defined GL_FUNC_SUBTRACT
# if defined Subtract
#  pragma push_macro("Subtract")
#  undef Subtract
	Transform<BlendEquation::Subtract> Subtract;
#  pragma pop_macro("Subtract")
# else
	Transform<BlendEquation::Subtract> Subtract;
# endif
#endif
#if defined GL_FUNC_REVERSE_SUBTRACT
# if defined ReverseSubtract
#  pragma push_macro("ReverseSubtract")
#  undef ReverseSubtract
	Transform<BlendEquation::ReverseSubtract> ReverseSubtract;
#  pragma pop_macro("ReverseSubtract")
# else
	Transform<BlendEquation::ReverseSubtract> ReverseSubtract;
# endif
#endif
#if defined GL_MIN
# if defined Min
#  pragma push_macro("Min")
#  undef Min
	Transform<BlendEquation::Min> Min;
#  pragma pop_macro("Min")
# else
	Transform<BlendEquation::Min> Min;
# endif
#endif
#if defined GL_MAX
# if defined Max
#  pragma push_macro("Max")
#  undef Max
	Transform<BlendEquation::Max> Max;
#  pragma pop_macro("Max")
# else
	Transform<BlendEquation::Max> Max;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FUNC_ADD
# if defined Add
#  pragma push_macro("Add")
#  undef Add
	 , Add(_base())
#  pragma pop_macro("Add")
# else
	 , Add(_base())
# endif
#endif
#if defined GL_FUNC_SUBTRACT
# if defined Subtract
#  pragma push_macro("Subtract")
#  undef Subtract
	 , Subtract(_base())
#  pragma pop_macro("Subtract")
# else
	 , Subtract(_base())
# endif
#endif
#if defined GL_FUNC_REVERSE_SUBTRACT
# if defined ReverseSubtract
#  pragma push_macro("ReverseSubtract")
#  undef ReverseSubtract
	 , ReverseSubtract(_base())
#  pragma pop_macro("ReverseSubtract")
# else
	 , ReverseSubtract(_base())
# endif
#endif
#if defined GL_MIN
# if defined Min
#  pragma push_macro("Min")
#  undef Min
	 , Min(_base())
#  pragma pop_macro("Min")
# else
	 , Min(_base())
# endif
#endif
#if defined GL_MAX
# if defined Max
#  pragma push_macro("Max")
#  undef Max
	 , Max(_base())
#  pragma pop_macro("Max")
# else
	 , Max(_base())
# endif
#endif
	{ }
};

} // namespace enums

