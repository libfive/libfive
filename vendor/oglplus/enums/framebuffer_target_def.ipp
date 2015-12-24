//  File implement/oglplus/enums/framebuffer_target_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_target.txt'
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

#if defined GL_DRAW_FRAMEBUFFER
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Draw
#  pragma push_macro("Draw")
#  undef Draw
   OGLPLUS_ENUM_CLASS_VALUE(Draw, GL_DRAW_FRAMEBUFFER)
#  pragma pop_macro("Draw")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Draw, GL_DRAW_FRAMEBUFFER)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_READ_FRAMEBUFFER
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Read
#  pragma push_macro("Read")
#  undef Read
   OGLPLUS_ENUM_CLASS_VALUE(Read, GL_READ_FRAMEBUFFER)
#  pragma pop_macro("Read")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Read, GL_READ_FRAMEBUFFER)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

