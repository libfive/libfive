//  File implement/oglplus/enums/sync_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SyncType> class Transform>
class EnumToClass<Base, SyncType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_SYNC_FENCE
# if defined Fence
#  pragma push_macro("Fence")
#  undef Fence
	Transform<SyncType::Fence> Fence;
#  pragma pop_macro("Fence")
# else
	Transform<SyncType::Fence> Fence;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_SYNC_FENCE
# if defined Fence
#  pragma push_macro("Fence")
#  undef Fence
	 , Fence(_base())
#  pragma pop_macro("Fence")
# else
	 , Fence(_base())
# endif
#endif
	{ }
};

} // namespace enums

