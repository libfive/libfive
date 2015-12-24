//  File implement/oglplus/enums/conditional_render_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/conditional_render_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ConditionalRenderMode> class Transform>
class EnumToClass<Base, ConditionalRenderMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_QUERY_WAIT
# if defined QueryWait
#  pragma push_macro("QueryWait")
#  undef QueryWait
	Transform<ConditionalRenderMode::QueryWait> QueryWait;
#  pragma pop_macro("QueryWait")
# else
	Transform<ConditionalRenderMode::QueryWait> QueryWait;
# endif
#endif
#if defined GL_QUERY_NO_WAIT
# if defined QueryNoWait
#  pragma push_macro("QueryNoWait")
#  undef QueryNoWait
	Transform<ConditionalRenderMode::QueryNoWait> QueryNoWait;
#  pragma pop_macro("QueryNoWait")
# else
	Transform<ConditionalRenderMode::QueryNoWait> QueryNoWait;
# endif
#endif
#if defined GL_QUERY_BY_REGION_WAIT
# if defined QueryByRegionWait
#  pragma push_macro("QueryByRegionWait")
#  undef QueryByRegionWait
	Transform<ConditionalRenderMode::QueryByRegionWait> QueryByRegionWait;
#  pragma pop_macro("QueryByRegionWait")
# else
	Transform<ConditionalRenderMode::QueryByRegionWait> QueryByRegionWait;
# endif
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT
# if defined QueryByRegionNoWait
#  pragma push_macro("QueryByRegionNoWait")
#  undef QueryByRegionNoWait
	Transform<ConditionalRenderMode::QueryByRegionNoWait> QueryByRegionNoWait;
#  pragma pop_macro("QueryByRegionNoWait")
# else
	Transform<ConditionalRenderMode::QueryByRegionNoWait> QueryByRegionNoWait;
# endif
#endif
#if defined GL_QUERY_WAIT_INVERTED
# if defined QueryWaitInverted
#  pragma push_macro("QueryWaitInverted")
#  undef QueryWaitInverted
	Transform<ConditionalRenderMode::QueryWaitInverted> QueryWaitInverted;
#  pragma pop_macro("QueryWaitInverted")
# else
	Transform<ConditionalRenderMode::QueryWaitInverted> QueryWaitInverted;
# endif
#endif
#if defined GL_QUERY_NO_WAIT_INVERTED
# if defined QueryNoWaitInverted
#  pragma push_macro("QueryNoWaitInverted")
#  undef QueryNoWaitInverted
	Transform<ConditionalRenderMode::QueryNoWaitInverted> QueryNoWaitInverted;
#  pragma pop_macro("QueryNoWaitInverted")
# else
	Transform<ConditionalRenderMode::QueryNoWaitInverted> QueryNoWaitInverted;
# endif
#endif
#if defined GL_QUERY_BY_REGION_WAIT_INVERTED
# if defined QueryByRegionWaitInverted
#  pragma push_macro("QueryByRegionWaitInverted")
#  undef QueryByRegionWaitInverted
	Transform<ConditionalRenderMode::QueryByRegionWaitInverted> QueryByRegionWaitInverted;
#  pragma pop_macro("QueryByRegionWaitInverted")
# else
	Transform<ConditionalRenderMode::QueryByRegionWaitInverted> QueryByRegionWaitInverted;
# endif
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT_INVERTED
# if defined QueryByRegionNoWaitInverted
#  pragma push_macro("QueryByRegionNoWaitInverted")
#  undef QueryByRegionNoWaitInverted
	Transform<ConditionalRenderMode::QueryByRegionNoWaitInverted> QueryByRegionNoWaitInverted;
#  pragma pop_macro("QueryByRegionNoWaitInverted")
# else
	Transform<ConditionalRenderMode::QueryByRegionNoWaitInverted> QueryByRegionNoWaitInverted;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_QUERY_WAIT
# if defined QueryWait
#  pragma push_macro("QueryWait")
#  undef QueryWait
	 , QueryWait(_base())
#  pragma pop_macro("QueryWait")
# else
	 , QueryWait(_base())
# endif
#endif
#if defined GL_QUERY_NO_WAIT
# if defined QueryNoWait
#  pragma push_macro("QueryNoWait")
#  undef QueryNoWait
	 , QueryNoWait(_base())
#  pragma pop_macro("QueryNoWait")
# else
	 , QueryNoWait(_base())
# endif
#endif
#if defined GL_QUERY_BY_REGION_WAIT
# if defined QueryByRegionWait
#  pragma push_macro("QueryByRegionWait")
#  undef QueryByRegionWait
	 , QueryByRegionWait(_base())
#  pragma pop_macro("QueryByRegionWait")
# else
	 , QueryByRegionWait(_base())
# endif
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT
# if defined QueryByRegionNoWait
#  pragma push_macro("QueryByRegionNoWait")
#  undef QueryByRegionNoWait
	 , QueryByRegionNoWait(_base())
#  pragma pop_macro("QueryByRegionNoWait")
# else
	 , QueryByRegionNoWait(_base())
# endif
#endif
#if defined GL_QUERY_WAIT_INVERTED
# if defined QueryWaitInverted
#  pragma push_macro("QueryWaitInverted")
#  undef QueryWaitInverted
	 , QueryWaitInverted(_base())
#  pragma pop_macro("QueryWaitInverted")
# else
	 , QueryWaitInverted(_base())
# endif
#endif
#if defined GL_QUERY_NO_WAIT_INVERTED
# if defined QueryNoWaitInverted
#  pragma push_macro("QueryNoWaitInverted")
#  undef QueryNoWaitInverted
	 , QueryNoWaitInverted(_base())
#  pragma pop_macro("QueryNoWaitInverted")
# else
	 , QueryNoWaitInverted(_base())
# endif
#endif
#if defined GL_QUERY_BY_REGION_WAIT_INVERTED
# if defined QueryByRegionWaitInverted
#  pragma push_macro("QueryByRegionWaitInverted")
#  undef QueryByRegionWaitInverted
	 , QueryByRegionWaitInverted(_base())
#  pragma pop_macro("QueryByRegionWaitInverted")
# else
	 , QueryByRegionWaitInverted(_base())
# endif
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT_INVERTED
# if defined QueryByRegionNoWaitInverted
#  pragma push_macro("QueryByRegionNoWaitInverted")
#  undef QueryByRegionNoWaitInverted
	 , QueryByRegionNoWaitInverted(_base())
#  pragma pop_macro("QueryByRegionNoWaitInverted")
# else
	 , QueryByRegionNoWaitInverted(_base())
# endif
#endif
	{ }
};

} // namespace enums

