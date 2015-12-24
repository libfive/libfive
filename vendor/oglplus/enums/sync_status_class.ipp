//  File implement/oglplus/enums/sync_status_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SyncStatus> class Transform>
class EnumToClass<Base, SyncStatus, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_SIGNALED
# if defined Signaled
#  pragma push_macro("Signaled")
#  undef Signaled
	Transform<SyncStatus::Signaled> Signaled;
#  pragma pop_macro("Signaled")
# else
	Transform<SyncStatus::Signaled> Signaled;
# endif
#endif
#if defined GL_UNSIGNALED
# if defined Unsignaled
#  pragma push_macro("Unsignaled")
#  undef Unsignaled
	Transform<SyncStatus::Unsignaled> Unsignaled;
#  pragma pop_macro("Unsignaled")
# else
	Transform<SyncStatus::Unsignaled> Unsignaled;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_SIGNALED
# if defined Signaled
#  pragma push_macro("Signaled")
#  undef Signaled
	 , Signaled(_base())
#  pragma pop_macro("Signaled")
# else
	 , Signaled(_base())
# endif
#endif
#if defined GL_UNSIGNALED
# if defined Unsignaled
#  pragma push_macro("Unsignaled")
#  undef Unsignaled
	 , Unsignaled(_base())
#  pragma pop_macro("Unsignaled")
# else
	 , Unsignaled(_base())
# endif
#endif
	{ }
};

} // namespace enums

