//  File implement/oglplus/enums/ext/nv_path_format_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_format.txt'
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

#if defined GL_PATH_FORMAT_SVG_NV
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined SVG
#  pragma push_macro("SVG")
#  undef SVG
   OGLPLUS_ENUM_CLASS_VALUE(SVG, GL_PATH_FORMAT_SVG_NV)
#  pragma pop_macro("SVG")
# else
   OGLPLUS_ENUM_CLASS_VALUE(SVG, GL_PATH_FORMAT_SVG_NV)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_PATH_FORMAT_PS_NV
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined PS
#  pragma push_macro("PS")
#  undef PS
   OGLPLUS_ENUM_CLASS_VALUE(PS, GL_PATH_FORMAT_PS_NV)
#  pragma pop_macro("PS")
# else
   OGLPLUS_ENUM_CLASS_VALUE(PS, GL_PATH_FORMAT_PS_NV)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

