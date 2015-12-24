//  File implement/oglplus/enums/ext/nv_path_list_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_list_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVListMode> class Transform>
class EnumToClass<Base, PathNVListMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ACCUM_ADJACENT_PAIRS_NV
# if defined AccumAdjacentPairs
#  pragma push_macro("AccumAdjacentPairs")
#  undef AccumAdjacentPairs
	Transform<PathNVListMode::AccumAdjacentPairs> AccumAdjacentPairs;
#  pragma pop_macro("AccumAdjacentPairs")
# else
	Transform<PathNVListMode::AccumAdjacentPairs> AccumAdjacentPairs;
# endif
#endif
#if defined GL_ADJACENT_PAIRS_NV
# if defined AdjacentPairs
#  pragma push_macro("AdjacentPairs")
#  undef AdjacentPairs
	Transform<PathNVListMode::AdjacentPairs> AdjacentPairs;
#  pragma pop_macro("AdjacentPairs")
# else
	Transform<PathNVListMode::AdjacentPairs> AdjacentPairs;
# endif
#endif
#if defined GL_FIRST_TO_REST_NV
# if defined FirstToRest
#  pragma push_macro("FirstToRest")
#  undef FirstToRest
	Transform<PathNVListMode::FirstToRest> FirstToRest;
#  pragma pop_macro("FirstToRest")
# else
	Transform<PathNVListMode::FirstToRest> FirstToRest;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_ACCUM_ADJACENT_PAIRS_NV
# if defined AccumAdjacentPairs
#  pragma push_macro("AccumAdjacentPairs")
#  undef AccumAdjacentPairs
	 , AccumAdjacentPairs(_base())
#  pragma pop_macro("AccumAdjacentPairs")
# else
	 , AccumAdjacentPairs(_base())
# endif
#endif
#if defined GL_ADJACENT_PAIRS_NV
# if defined AdjacentPairs
#  pragma push_macro("AdjacentPairs")
#  undef AdjacentPairs
	 , AdjacentPairs(_base())
#  pragma pop_macro("AdjacentPairs")
# else
	 , AdjacentPairs(_base())
# endif
#endif
#if defined GL_FIRST_TO_REST_NV
# if defined FirstToRest
#  pragma push_macro("FirstToRest")
#  undef FirstToRest
	 , FirstToRest(_base())
#  pragma pop_macro("FirstToRest")
# else
	 , FirstToRest(_base())
# endif
#endif
	{ }
};

} // namespace enums

