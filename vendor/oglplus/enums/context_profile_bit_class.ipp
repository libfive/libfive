//  File implement/oglplus/enums/context_profile_bit_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_profile_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ContextProfileBit> class Transform>
class EnumToClass<Base, ContextProfileBit, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CONTEXT_CORE_PROFILE_BIT
# if defined Core
#  pragma push_macro("Core")
#  undef Core
	Transform<ContextProfileBit::Core> Core;
#  pragma pop_macro("Core")
# else
	Transform<ContextProfileBit::Core> Core;
# endif
#endif
#if defined GL_CONTEXT_COMPATIBILITY_PROFILE_BIT
# if defined Compatibility
#  pragma push_macro("Compatibility")
#  undef Compatibility
	Transform<ContextProfileBit::Compatibility> Compatibility;
#  pragma pop_macro("Compatibility")
# else
	Transform<ContextProfileBit::Compatibility> Compatibility;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CONTEXT_CORE_PROFILE_BIT
# if defined Core
#  pragma push_macro("Core")
#  undef Core
	 , Core(_base())
#  pragma pop_macro("Core")
# else
	 , Core(_base())
# endif
#endif
#if defined GL_CONTEXT_COMPATIBILITY_PROFILE_BIT
# if defined Compatibility
#  pragma push_macro("Compatibility")
#  undef Compatibility
	 , Compatibility(_base())
#  pragma pop_macro("Compatibility")
# else
	 , Compatibility(_base())
# endif
#endif
	{ }
};

} // namespace enums

