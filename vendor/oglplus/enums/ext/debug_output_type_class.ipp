//  File implement/oglplus/enums/ext/debug_output_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_output_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<DebugOutputARBType> class Transform>
class EnumToClass<Base, DebugOutputARBType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEBUG_TYPE_ERROR_ARB
# if defined Error
#  pragma push_macro("Error")
#  undef Error
	Transform<DebugOutputARBType::Error> Error;
#  pragma pop_macro("Error")
# else
	Transform<DebugOutputARBType::Error> Error;
# endif
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
# if defined DeprecatedBehavior
#  pragma push_macro("DeprecatedBehavior")
#  undef DeprecatedBehavior
	Transform<DebugOutputARBType::DeprecatedBehavior> DeprecatedBehavior;
#  pragma pop_macro("DeprecatedBehavior")
# else
	Transform<DebugOutputARBType::DeprecatedBehavior> DeprecatedBehavior;
# endif
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
# if defined UndefinedBehavior
#  pragma push_macro("UndefinedBehavior")
#  undef UndefinedBehavior
	Transform<DebugOutputARBType::UndefinedBehavior> UndefinedBehavior;
#  pragma pop_macro("UndefinedBehavior")
# else
	Transform<DebugOutputARBType::UndefinedBehavior> UndefinedBehavior;
# endif
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY_ARB
# if defined Portability
#  pragma push_macro("Portability")
#  undef Portability
	Transform<DebugOutputARBType::Portability> Portability;
#  pragma pop_macro("Portability")
# else
	Transform<DebugOutputARBType::Portability> Portability;
# endif
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE_ARB
# if defined Performance
#  pragma push_macro("Performance")
#  undef Performance
	Transform<DebugOutputARBType::Performance> Performance;
#  pragma pop_macro("Performance")
# else
	Transform<DebugOutputARBType::Performance> Performance;
# endif
#endif
#if defined GL_DEBUG_TYPE_OTHER_ARB
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	Transform<DebugOutputARBType::Other> Other;
#  pragma pop_macro("Other")
# else
	Transform<DebugOutputARBType::Other> Other;
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	Transform<DebugOutputARBType::DontCare> DontCare;
#  pragma pop_macro("DontCare")
# else
	Transform<DebugOutputARBType::DontCare> DontCare;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DEBUG_TYPE_ERROR_ARB
# if defined Error
#  pragma push_macro("Error")
#  undef Error
	 , Error(_base())
#  pragma pop_macro("Error")
# else
	 , Error(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
# if defined DeprecatedBehavior
#  pragma push_macro("DeprecatedBehavior")
#  undef DeprecatedBehavior
	 , DeprecatedBehavior(_base())
#  pragma pop_macro("DeprecatedBehavior")
# else
	 , DeprecatedBehavior(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
# if defined UndefinedBehavior
#  pragma push_macro("UndefinedBehavior")
#  undef UndefinedBehavior
	 , UndefinedBehavior(_base())
#  pragma pop_macro("UndefinedBehavior")
# else
	 , UndefinedBehavior(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY_ARB
# if defined Portability
#  pragma push_macro("Portability")
#  undef Portability
	 , Portability(_base())
#  pragma pop_macro("Portability")
# else
	 , Portability(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE_ARB
# if defined Performance
#  pragma push_macro("Performance")
#  undef Performance
	 , Performance(_base())
#  pragma pop_macro("Performance")
# else
	 , Performance(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_OTHER_ARB
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	 , Other(_base())
#  pragma pop_macro("Other")
# else
	 , Other(_base())
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	 , DontCare(_base())
#  pragma pop_macro("DontCare")
# else
	 , DontCare(_base())
# endif
#endif
	{ }
};

} // namespace enums

