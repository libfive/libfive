//  File implement/oglplus/enums/stencil_operation_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/stencil_operation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<StencilOperation> class Transform>
class EnumToClass<Base, StencilOperation, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_KEEP
# if defined Keep
#  pragma push_macro("Keep")
#  undef Keep
	Transform<StencilOperation::Keep> Keep;
#  pragma pop_macro("Keep")
# else
	Transform<StencilOperation::Keep> Keep;
# endif
#endif
#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	Transform<StencilOperation::Zero> Zero;
#  pragma pop_macro("Zero")
# else
	Transform<StencilOperation::Zero> Zero;
# endif
#endif
#if defined GL_REPLACE
# if defined Replace
#  pragma push_macro("Replace")
#  undef Replace
	Transform<StencilOperation::Replace> Replace;
#  pragma pop_macro("Replace")
# else
	Transform<StencilOperation::Replace> Replace;
# endif
#endif
#if defined GL_INCR
# if defined Incr
#  pragma push_macro("Incr")
#  undef Incr
	Transform<StencilOperation::Incr> Incr;
#  pragma pop_macro("Incr")
# else
	Transform<StencilOperation::Incr> Incr;
# endif
#endif
#if defined GL_DECR
# if defined Decr
#  pragma push_macro("Decr")
#  undef Decr
	Transform<StencilOperation::Decr> Decr;
#  pragma pop_macro("Decr")
# else
	Transform<StencilOperation::Decr> Decr;
# endif
#endif
#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	Transform<StencilOperation::Invert> Invert;
#  pragma pop_macro("Invert")
# else
	Transform<StencilOperation::Invert> Invert;
# endif
#endif
#if defined GL_INCR_WRAP
# if defined IncrWrap
#  pragma push_macro("IncrWrap")
#  undef IncrWrap
	Transform<StencilOperation::IncrWrap> IncrWrap;
#  pragma pop_macro("IncrWrap")
# else
	Transform<StencilOperation::IncrWrap> IncrWrap;
# endif
#endif
#if defined GL_DECR_WRAP
# if defined DecrWrap
#  pragma push_macro("DecrWrap")
#  undef DecrWrap
	Transform<StencilOperation::DecrWrap> DecrWrap;
#  pragma pop_macro("DecrWrap")
# else
	Transform<StencilOperation::DecrWrap> DecrWrap;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_KEEP
# if defined Keep
#  pragma push_macro("Keep")
#  undef Keep
	 , Keep(_base())
#  pragma pop_macro("Keep")
# else
	 , Keep(_base())
# endif
#endif
#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	 , Zero(_base())
#  pragma pop_macro("Zero")
# else
	 , Zero(_base())
# endif
#endif
#if defined GL_REPLACE
# if defined Replace
#  pragma push_macro("Replace")
#  undef Replace
	 , Replace(_base())
#  pragma pop_macro("Replace")
# else
	 , Replace(_base())
# endif
#endif
#if defined GL_INCR
# if defined Incr
#  pragma push_macro("Incr")
#  undef Incr
	 , Incr(_base())
#  pragma pop_macro("Incr")
# else
	 , Incr(_base())
# endif
#endif
#if defined GL_DECR
# if defined Decr
#  pragma push_macro("Decr")
#  undef Decr
	 , Decr(_base())
#  pragma pop_macro("Decr")
# else
	 , Decr(_base())
# endif
#endif
#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	 , Invert(_base())
#  pragma pop_macro("Invert")
# else
	 , Invert(_base())
# endif
#endif
#if defined GL_INCR_WRAP
# if defined IncrWrap
#  pragma push_macro("IncrWrap")
#  undef IncrWrap
	 , IncrWrap(_base())
#  pragma pop_macro("IncrWrap")
# else
	 , IncrWrap(_base())
# endif
#endif
#if defined GL_DECR_WRAP
# if defined DecrWrap
#  pragma push_macro("DecrWrap")
#  undef DecrWrap
	 , DecrWrap(_base())
#  pragma pop_macro("DecrWrap")
# else
	 , DecrWrap(_base())
# endif
#endif
	{ }
};

} // namespace enums

