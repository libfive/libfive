//  File implement/oglplus/enums/framebuffer_buffer_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_buffer.txt'
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

#if defined GL_COLOR
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Color
#  pragma push_macro("Color")
#  undef Color
   OGLPLUS_ENUM_CLASS_VALUE(Color, GL_COLOR)
#  pragma pop_macro("Color")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Color, GL_COLOR)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_DEPTH
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Depth
#  pragma push_macro("Depth")
#  undef Depth
   OGLPLUS_ENUM_CLASS_VALUE(Depth, GL_DEPTH)
#  pragma pop_macro("Depth")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Depth, GL_DEPTH)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_STENCIL
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Stencil
#  pragma push_macro("Stencil")
#  undef Stencil
   OGLPLUS_ENUM_CLASS_VALUE(Stencil, GL_STENCIL)
#  pragma pop_macro("Stencil")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Stencil, GL_STENCIL)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_DEPTH_STENCIL
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined DepthStencil
#  pragma push_macro("DepthStencil")
#  undef DepthStencil
   OGLPLUS_ENUM_CLASS_VALUE(DepthStencil, GL_DEPTH_STENCIL)
#  pragma pop_macro("DepthStencil")
# else
   OGLPLUS_ENUM_CLASS_VALUE(DepthStencil, GL_DEPTH_STENCIL)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

