//  File implement/oglplus/enums/patch_parameter_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/patch_parameter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

#if defined GL_PATCH_VERTICES
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined PatchVertices
#  pragma push_macro("PatchVertices")
#  undef PatchVertices
   OGLPLUS_ENUM_CLASS_VALUE(PatchVertices, GL_PATCH_VERTICES)
#  pragma pop_macro("PatchVertices")
# else
   OGLPLUS_ENUM_CLASS_VALUE(PatchVertices, GL_PATCH_VERTICES)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_PATCH_DEFAULT_OUTER_LEVEL
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined PatchDefaultOuterLevel
#  pragma push_macro("PatchDefaultOuterLevel")
#  undef PatchDefaultOuterLevel
   OGLPLUS_ENUM_CLASS_VALUE(PatchDefaultOuterLevel, GL_PATCH_DEFAULT_OUTER_LEVEL)
#  pragma pop_macro("PatchDefaultOuterLevel")
# else
   OGLPLUS_ENUM_CLASS_VALUE(PatchDefaultOuterLevel, GL_PATCH_DEFAULT_OUTER_LEVEL)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_PATCH_DEFAULT_INNER_LEVEL
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined PatchDefaultInnerLevel
#  pragma push_macro("PatchDefaultInnerLevel")
#  undef PatchDefaultInnerLevel
   OGLPLUS_ENUM_CLASS_VALUE(PatchDefaultInnerLevel, GL_PATCH_DEFAULT_INNER_LEVEL)
#  pragma pop_macro("PatchDefaultInnerLevel")
# else
   OGLPLUS_ENUM_CLASS_VALUE(PatchDefaultInnerLevel, GL_PATCH_DEFAULT_INNER_LEVEL)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

