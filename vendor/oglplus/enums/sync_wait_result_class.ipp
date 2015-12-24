//  File implement/oglplus/enums/sync_wait_result_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_wait_result.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SyncWaitResult> class Transform>
class EnumToClass<Base, SyncWaitResult, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CONDITION_SATISFIED
# if defined ConditionSatisfied
#  pragma push_macro("ConditionSatisfied")
#  undef ConditionSatisfied
	Transform<SyncWaitResult::ConditionSatisfied> ConditionSatisfied;
#  pragma pop_macro("ConditionSatisfied")
# else
	Transform<SyncWaitResult::ConditionSatisfied> ConditionSatisfied;
# endif
#endif
#if defined GL_ALREADY_SIGNALED
# if defined AlreadySignaled
#  pragma push_macro("AlreadySignaled")
#  undef AlreadySignaled
	Transform<SyncWaitResult::AlreadySignaled> AlreadySignaled;
#  pragma pop_macro("AlreadySignaled")
# else
	Transform<SyncWaitResult::AlreadySignaled> AlreadySignaled;
# endif
#endif
#if defined GL_TIMEOUT_EXPIRED
# if defined TimeoutExpired
#  pragma push_macro("TimeoutExpired")
#  undef TimeoutExpired
	Transform<SyncWaitResult::TimeoutExpired> TimeoutExpired;
#  pragma pop_macro("TimeoutExpired")
# else
	Transform<SyncWaitResult::TimeoutExpired> TimeoutExpired;
# endif
#endif
#if defined GL_WAIT_FAILED
# if defined WaitFailed
#  pragma push_macro("WaitFailed")
#  undef WaitFailed
	Transform<SyncWaitResult::WaitFailed> WaitFailed;
#  pragma pop_macro("WaitFailed")
# else
	Transform<SyncWaitResult::WaitFailed> WaitFailed;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CONDITION_SATISFIED
# if defined ConditionSatisfied
#  pragma push_macro("ConditionSatisfied")
#  undef ConditionSatisfied
	 , ConditionSatisfied(_base())
#  pragma pop_macro("ConditionSatisfied")
# else
	 , ConditionSatisfied(_base())
# endif
#endif
#if defined GL_ALREADY_SIGNALED
# if defined AlreadySignaled
#  pragma push_macro("AlreadySignaled")
#  undef AlreadySignaled
	 , AlreadySignaled(_base())
#  pragma pop_macro("AlreadySignaled")
# else
	 , AlreadySignaled(_base())
# endif
#endif
#if defined GL_TIMEOUT_EXPIRED
# if defined TimeoutExpired
#  pragma push_macro("TimeoutExpired")
#  undef TimeoutExpired
	 , TimeoutExpired(_base())
#  pragma pop_macro("TimeoutExpired")
# else
	 , TimeoutExpired(_base())
# endif
#endif
#if defined GL_WAIT_FAILED
# if defined WaitFailed
#  pragma push_macro("WaitFailed")
#  undef WaitFailed
	 , WaitFailed(_base())
#  pragma pop_macro("WaitFailed")
# else
	 , WaitFailed(_base())
# endif
#endif
	{ }
};

} // namespace enums

