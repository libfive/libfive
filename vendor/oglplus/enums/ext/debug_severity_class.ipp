//  File implement/oglplus/enums/ext/debug_severity_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_severity.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<DebugSeverity> class Transform>
class EnumToClass<Base, DebugSeverity, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEBUG_SEVERITY_HIGH
# if defined High
#  pragma push_macro("High")
#  undef High
	Transform<DebugSeverity::High> High;
#  pragma pop_macro("High")
# else
	Transform<DebugSeverity::High> High;
# endif
#endif
#if defined GL_DEBUG_SEVERITY_MEDIUM
# if defined Medium
#  pragma push_macro("Medium")
#  undef Medium
	Transform<DebugSeverity::Medium> Medium;
#  pragma pop_macro("Medium")
# else
	Transform<DebugSeverity::Medium> Medium;
# endif
#endif
#if defined GL_DEBUG_SEVERITY_LOW
# if defined Low
#  pragma push_macro("Low")
#  undef Low
	Transform<DebugSeverity::Low> Low;
#  pragma pop_macro("Low")
# else
	Transform<DebugSeverity::Low> Low;
# endif
#endif
#if defined GL_DEBUG_SEVERITY_NOTIFICATION
# if defined Notification
#  pragma push_macro("Notification")
#  undef Notification
	Transform<DebugSeverity::Notification> Notification;
#  pragma pop_macro("Notification")
# else
	Transform<DebugSeverity::Notification> Notification;
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	Transform<DebugSeverity::DontCare> DontCare;
#  pragma pop_macro("DontCare")
# else
	Transform<DebugSeverity::DontCare> DontCare;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DEBUG_SEVERITY_HIGH
# if defined High
#  pragma push_macro("High")
#  undef High
	 , High(_base())
#  pragma pop_macro("High")
# else
	 , High(_base())
# endif
#endif
#if defined GL_DEBUG_SEVERITY_MEDIUM
# if defined Medium
#  pragma push_macro("Medium")
#  undef Medium
	 , Medium(_base())
#  pragma pop_macro("Medium")
# else
	 , Medium(_base())
# endif
#endif
#if defined GL_DEBUG_SEVERITY_LOW
# if defined Low
#  pragma push_macro("Low")
#  undef Low
	 , Low(_base())
#  pragma pop_macro("Low")
# else
	 , Low(_base())
# endif
#endif
#if defined GL_DEBUG_SEVERITY_NOTIFICATION
# if defined Notification
#  pragma push_macro("Notification")
#  undef Notification
	 , Notification(_base())
#  pragma pop_macro("Notification")
# else
	 , Notification(_base())
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

