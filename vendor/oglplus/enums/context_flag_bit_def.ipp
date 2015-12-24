//  File implement/oglplus/enums/context_flag_bit_def.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_flag_bit.txt'
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

#if defined GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ForwardCompatible
#  pragma push_macro("ForwardCompatible")
#  undef ForwardCompatible
   OGLPLUS_ENUM_CLASS_VALUE(ForwardCompatible, GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT)
#  pragma pop_macro("ForwardCompatible")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ForwardCompatible, GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_CONTEXT_FLAG_DEBUG_BIT
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Debug
#  pragma push_macro("Debug")
#  undef Debug
   OGLPLUS_ENUM_CLASS_VALUE(Debug, GL_CONTEXT_FLAG_DEBUG_BIT)
#  pragma pop_macro("Debug")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Debug, GL_CONTEXT_FLAG_DEBUG_BIT)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined RobustAccess
#  pragma push_macro("RobustAccess")
#  undef RobustAccess
   OGLPLUS_ENUM_CLASS_VALUE(RobustAccess, GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB)
#  pragma pop_macro("RobustAccess")
# else
   OGLPLUS_ENUM_CLASS_VALUE(RobustAccess, GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

