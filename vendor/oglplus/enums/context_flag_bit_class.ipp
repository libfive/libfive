//  File implement/oglplus/enums/context_flag_bit_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_flag_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ContextFlagBit> class Transform>
class EnumToClass<Base, ContextFlagBit, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
# if defined ForwardCompatible
#  pragma push_macro("ForwardCompatible")
#  undef ForwardCompatible
	Transform<ContextFlagBit::ForwardCompatible> ForwardCompatible;
#  pragma pop_macro("ForwardCompatible")
# else
	Transform<ContextFlagBit::ForwardCompatible> ForwardCompatible;
# endif
#endif
#if defined GL_CONTEXT_FLAG_DEBUG_BIT
# if defined Debug
#  pragma push_macro("Debug")
#  undef Debug
	Transform<ContextFlagBit::Debug> Debug;
#  pragma pop_macro("Debug")
# else
	Transform<ContextFlagBit::Debug> Debug;
# endif
#endif
#if defined GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
# if defined RobustAccess
#  pragma push_macro("RobustAccess")
#  undef RobustAccess
	Transform<ContextFlagBit::RobustAccess> RobustAccess;
#  pragma pop_macro("RobustAccess")
# else
	Transform<ContextFlagBit::RobustAccess> RobustAccess;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
# if defined ForwardCompatible
#  pragma push_macro("ForwardCompatible")
#  undef ForwardCompatible
	 , ForwardCompatible(_base())
#  pragma pop_macro("ForwardCompatible")
# else
	 , ForwardCompatible(_base())
# endif
#endif
#if defined GL_CONTEXT_FLAG_DEBUG_BIT
# if defined Debug
#  pragma push_macro("Debug")
#  undef Debug
	 , Debug(_base())
#  pragma pop_macro("Debug")
# else
	 , Debug(_base())
# endif
#endif
#if defined GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
# if defined RobustAccess
#  pragma push_macro("RobustAccess")
#  undef RobustAccess
	 , RobustAccess(_base())
#  pragma pop_macro("RobustAccess")
# else
	 , RobustAccess(_base())
# endif
#endif
	{ }
};

} // namespace enums

