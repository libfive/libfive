//  File implement/oglplus/enums/sync_status_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_status.txt'
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

#if defined GL_SIGNALED
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Signaled
#  pragma push_macro("Signaled")
#  undef Signaled
   OGLPLUS_ENUM_CLASS_VALUE(Signaled, GL_SIGNALED)
#  pragma pop_macro("Signaled")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Signaled, GL_SIGNALED)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_UNSIGNALED
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Unsignaled
#  pragma push_macro("Unsignaled")
#  undef Unsignaled
   OGLPLUS_ENUM_CLASS_VALUE(Unsignaled, GL_UNSIGNALED)
#  pragma pop_macro("Unsignaled")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Unsignaled, GL_UNSIGNALED)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

