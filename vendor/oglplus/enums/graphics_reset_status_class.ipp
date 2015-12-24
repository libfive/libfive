//  File implement/oglplus/enums/graphics_reset_status_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/graphics_reset_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<GraphicsResetStatus> class Transform>
class EnumToClass<Base, GraphicsResetStatus, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NO_ERROR
# if defined NoError
#  pragma push_macro("NoError")
#  undef NoError
	Transform<GraphicsResetStatus::NoError> NoError;
#  pragma pop_macro("NoError")
# else
	Transform<GraphicsResetStatus::NoError> NoError;
# endif
#endif
#if defined GL_GUILTY_CONTEXT_RESET
# if defined GuiltyContextReset
#  pragma push_macro("GuiltyContextReset")
#  undef GuiltyContextReset
	Transform<GraphicsResetStatus::GuiltyContextReset> GuiltyContextReset;
#  pragma pop_macro("GuiltyContextReset")
# else
	Transform<GraphicsResetStatus::GuiltyContextReset> GuiltyContextReset;
# endif
#endif
#if defined GL_INNOCENT_CONTEXT_RESET
# if defined InnocentContextReset
#  pragma push_macro("InnocentContextReset")
#  undef InnocentContextReset
	Transform<GraphicsResetStatus::InnocentContextReset> InnocentContextReset;
#  pragma pop_macro("InnocentContextReset")
# else
	Transform<GraphicsResetStatus::InnocentContextReset> InnocentContextReset;
# endif
#endif
#if defined GL_UNKNOWN_CONTEXT_RESET
# if defined UnknownContextReset
#  pragma push_macro("UnknownContextReset")
#  undef UnknownContextReset
	Transform<GraphicsResetStatus::UnknownContextReset> UnknownContextReset;
#  pragma pop_macro("UnknownContextReset")
# else
	Transform<GraphicsResetStatus::UnknownContextReset> UnknownContextReset;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NO_ERROR
# if defined NoError
#  pragma push_macro("NoError")
#  undef NoError
	 , NoError(_base())
#  pragma pop_macro("NoError")
# else
	 , NoError(_base())
# endif
#endif
#if defined GL_GUILTY_CONTEXT_RESET
# if defined GuiltyContextReset
#  pragma push_macro("GuiltyContextReset")
#  undef GuiltyContextReset
	 , GuiltyContextReset(_base())
#  pragma pop_macro("GuiltyContextReset")
# else
	 , GuiltyContextReset(_base())
# endif
#endif
#if defined GL_INNOCENT_CONTEXT_RESET
# if defined InnocentContextReset
#  pragma push_macro("InnocentContextReset")
#  undef InnocentContextReset
	 , InnocentContextReset(_base())
#  pragma pop_macro("InnocentContextReset")
# else
	 , InnocentContextReset(_base())
# endif
#endif
#if defined GL_UNKNOWN_CONTEXT_RESET
# if defined UnknownContextReset
#  pragma push_macro("UnknownContextReset")
#  undef UnknownContextReset
	 , UnknownContextReset(_base())
#  pragma pop_macro("UnknownContextReset")
# else
	 , UnknownContextReset(_base())
# endif
#endif
	{ }
};

} // namespace enums

