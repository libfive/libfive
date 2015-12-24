//  File implement/oglplus/enums/sync_condition_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_condition.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SyncCondition> class Transform>
class EnumToClass<Base, SyncCondition, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_SYNC_GPU_COMMANDS_COMPLETE
# if defined GPUCommandsComplete
#  pragma push_macro("GPUCommandsComplete")
#  undef GPUCommandsComplete
	Transform<SyncCondition::GPUCommandsComplete> GPUCommandsComplete;
#  pragma pop_macro("GPUCommandsComplete")
# else
	Transform<SyncCondition::GPUCommandsComplete> GPUCommandsComplete;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_SYNC_GPU_COMMANDS_COMPLETE
# if defined GPUCommandsComplete
#  pragma push_macro("GPUCommandsComplete")
#  undef GPUCommandsComplete
	 , GPUCommandsComplete(_base())
#  pragma pop_macro("GPUCommandsComplete")
# else
	 , GPUCommandsComplete(_base())
# endif
#endif
	{ }
};

} // namespace enums

