//  File implement/oglplus/enums/renderbuffer_target_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/renderbuffer_target.txt'
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

#if defined GL_RENDERBUFFER
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Renderbuffer
#  pragma push_macro("Renderbuffer")
#  undef Renderbuffer
   OGLPLUS_ENUM_CLASS_VALUE(Renderbuffer, GL_RENDERBUFFER)
#  pragma pop_macro("Renderbuffer")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Renderbuffer, GL_RENDERBUFFER)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

