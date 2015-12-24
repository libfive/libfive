//  File implement/oglplus/enums/reset_notif_strategy_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/reset_notif_strategy.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ResetNotificationStrategy> class Transform>
class EnumToClass<Base, ResetNotificationStrategy, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NO_RESET_NOTIFICATION
# if defined NoResetNotification
#  pragma push_macro("NoResetNotification")
#  undef NoResetNotification
	Transform<ResetNotificationStrategy::NoResetNotification> NoResetNotification;
#  pragma pop_macro("NoResetNotification")
# else
	Transform<ResetNotificationStrategy::NoResetNotification> NoResetNotification;
# endif
#endif
#if defined GL_LOSE_CONTEXT_ON_RESET
# if defined LoseContextOnReset
#  pragma push_macro("LoseContextOnReset")
#  undef LoseContextOnReset
	Transform<ResetNotificationStrategy::LoseContextOnReset> LoseContextOnReset;
#  pragma pop_macro("LoseContextOnReset")
# else
	Transform<ResetNotificationStrategy::LoseContextOnReset> LoseContextOnReset;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NO_RESET_NOTIFICATION
# if defined NoResetNotification
#  pragma push_macro("NoResetNotification")
#  undef NoResetNotification
	 , NoResetNotification(_base())
#  pragma pop_macro("NoResetNotification")
# else
	 , NoResetNotification(_base())
# endif
#endif
#if defined GL_LOSE_CONTEXT_ON_RESET
# if defined LoseContextOnReset
#  pragma push_macro("LoseContextOnReset")
#  undef LoseContextOnReset
	 , LoseContextOnReset(_base())
#  pragma pop_macro("LoseContextOnReset")
# else
	 , LoseContextOnReset(_base())
# endif
#endif
	{ }
};

} // namespace enums

