//  File implement/oglplus/enums/ext/debug_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<DebugType> class Transform>
class EnumToClass<Base, DebugType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEBUG_TYPE_ERROR
# if defined Error
#  pragma push_macro("Error")
#  undef Error
	Transform<DebugType::Error> Error;
#  pragma pop_macro("Error")
# else
	Transform<DebugType::Error> Error;
# endif
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
# if defined DeprecatedBehavior
#  pragma push_macro("DeprecatedBehavior")
#  undef DeprecatedBehavior
	Transform<DebugType::DeprecatedBehavior> DeprecatedBehavior;
#  pragma pop_macro("DeprecatedBehavior")
# else
	Transform<DebugType::DeprecatedBehavior> DeprecatedBehavior;
# endif
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
# if defined UndefinedBehavior
#  pragma push_macro("UndefinedBehavior")
#  undef UndefinedBehavior
	Transform<DebugType::UndefinedBehavior> UndefinedBehavior;
#  pragma pop_macro("UndefinedBehavior")
# else
	Transform<DebugType::UndefinedBehavior> UndefinedBehavior;
# endif
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY
# if defined Portability
#  pragma push_macro("Portability")
#  undef Portability
	Transform<DebugType::Portability> Portability;
#  pragma pop_macro("Portability")
# else
	Transform<DebugType::Portability> Portability;
# endif
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE
# if defined Performance
#  pragma push_macro("Performance")
#  undef Performance
	Transform<DebugType::Performance> Performance;
#  pragma pop_macro("Performance")
# else
	Transform<DebugType::Performance> Performance;
# endif
#endif
#if defined GL_DEBUG_TYPE_OTHER
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	Transform<DebugType::Other> Other;
#  pragma pop_macro("Other")
# else
	Transform<DebugType::Other> Other;
# endif
#endif
#if defined GL_DEBUG_TYPE_MARKER
# if defined Marker
#  pragma push_macro("Marker")
#  undef Marker
	Transform<DebugType::Marker> Marker;
#  pragma pop_macro("Marker")
# else
	Transform<DebugType::Marker> Marker;
# endif
#endif
#if defined GL_DEBUG_TYPE_PUSH_GROUP
# if defined PushGroup
#  pragma push_macro("PushGroup")
#  undef PushGroup
	Transform<DebugType::PushGroup> PushGroup;
#  pragma pop_macro("PushGroup")
# else
	Transform<DebugType::PushGroup> PushGroup;
# endif
#endif
#if defined GL_DEBUG_TYPE_POP_GROUP
# if defined PopGroup
#  pragma push_macro("PopGroup")
#  undef PopGroup
	Transform<DebugType::PopGroup> PopGroup;
#  pragma pop_macro("PopGroup")
# else
	Transform<DebugType::PopGroup> PopGroup;
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	Transform<DebugType::DontCare> DontCare;
#  pragma pop_macro("DontCare")
# else
	Transform<DebugType::DontCare> DontCare;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DEBUG_TYPE_ERROR
# if defined Error
#  pragma push_macro("Error")
#  undef Error
	 , Error(_base())
#  pragma pop_macro("Error")
# else
	 , Error(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
# if defined DeprecatedBehavior
#  pragma push_macro("DeprecatedBehavior")
#  undef DeprecatedBehavior
	 , DeprecatedBehavior(_base())
#  pragma pop_macro("DeprecatedBehavior")
# else
	 , DeprecatedBehavior(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
# if defined UndefinedBehavior
#  pragma push_macro("UndefinedBehavior")
#  undef UndefinedBehavior
	 , UndefinedBehavior(_base())
#  pragma pop_macro("UndefinedBehavior")
# else
	 , UndefinedBehavior(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY
# if defined Portability
#  pragma push_macro("Portability")
#  undef Portability
	 , Portability(_base())
#  pragma pop_macro("Portability")
# else
	 , Portability(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE
# if defined Performance
#  pragma push_macro("Performance")
#  undef Performance
	 , Performance(_base())
#  pragma pop_macro("Performance")
# else
	 , Performance(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_OTHER
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	 , Other(_base())
#  pragma pop_macro("Other")
# else
	 , Other(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_MARKER
# if defined Marker
#  pragma push_macro("Marker")
#  undef Marker
	 , Marker(_base())
#  pragma pop_macro("Marker")
# else
	 , Marker(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_PUSH_GROUP
# if defined PushGroup
#  pragma push_macro("PushGroup")
#  undef PushGroup
	 , PushGroup(_base())
#  pragma pop_macro("PushGroup")
# else
	 , PushGroup(_base())
# endif
#endif
#if defined GL_DEBUG_TYPE_POP_GROUP
# if defined PopGroup
#  pragma push_macro("PopGroup")
#  undef PopGroup
	 , PopGroup(_base())
#  pragma pop_macro("PopGroup")
# else
	 , PopGroup(_base())
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

