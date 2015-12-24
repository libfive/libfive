//  File implement/oglplus/enums/provoke_mode_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/provoke_mode.txt'
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

#if defined GL_FIRST_VERTEX_CONVENTION
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined FirstVertexConvention
#  pragma push_macro("FirstVertexConvention")
#  undef FirstVertexConvention
   OGLPLUS_ENUM_CLASS_VALUE(FirstVertexConvention, GL_FIRST_VERTEX_CONVENTION)
#  pragma pop_macro("FirstVertexConvention")
# else
   OGLPLUS_ENUM_CLASS_VALUE(FirstVertexConvention, GL_FIRST_VERTEX_CONVENTION)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_LAST_VERTEX_CONVENTION
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined LastVertexConvention
#  pragma push_macro("LastVertexConvention")
#  undef LastVertexConvention
   OGLPLUS_ENUM_CLASS_VALUE(LastVertexConvention, GL_LAST_VERTEX_CONVENTION)
#  pragma pop_macro("LastVertexConvention")
# else
   OGLPLUS_ENUM_CLASS_VALUE(LastVertexConvention, GL_LAST_VERTEX_CONVENTION)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

