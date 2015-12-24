//  File implement/oglplus/enums/limit_query_type.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/limit_query.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//

namespace enums {
#if defined GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MaxFragmentInterpolationOffset>
{ typedef float Type; };
#endif
#if defined GL_MAX_PROGRAM_TEXEL_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MaxProgramTexelOffset>
{ typedef float Type; };
#endif
#if defined GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MaxProgramTextureGatherOffset>
{ typedef float Type; };
#endif
#if defined GL_MAX_SERVER_WAIT_TIMEOUT
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MaxServerWaitTimeout>
{ typedef int64_t Type; };
#endif
#if defined GL_MAX_TEXTURE_LOD_BIAS
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MaxTextureLodBias>
{ typedef float Type; };
#endif
#if defined GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MinFragmentInterpolationOffset>
{ typedef float Type; };
#endif
#if defined GL_MIN_PROGRAM_TEXEL_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MinProgramTexelOffset>
{ typedef float Type; };
#endif
#if defined GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
template <>
struct EnumAssocType<LimitQuery, LimitQuery::MinProgramTextureGatherOffset>
{ typedef float Type; };
#endif
} // namespace enums
