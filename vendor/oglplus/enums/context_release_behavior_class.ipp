//  File implement/oglplus/enums/context_release_behavior_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_release_behavior.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ContextReleaseBehavior> class Transform>
class EnumToClass<Base, ContextReleaseBehavior, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<ContextReleaseBehavior::None> None;
#  pragma pop_macro("None")
# else
	Transform<ContextReleaseBehavior::None> None;
# endif
#endif
#if defined GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
# if defined Flush
#  pragma push_macro("Flush")
#  undef Flush
	Transform<ContextReleaseBehavior::Flush> Flush;
#  pragma pop_macro("Flush")
# else
	Transform<ContextReleaseBehavior::Flush> Flush;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	 , None(_base())
#  pragma pop_macro("None")
# else
	 , None(_base())
# endif
#endif
#if defined GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH
# if defined Flush
#  pragma push_macro("Flush")
#  undef Flush
	 , Flush(_base())
#  pragma pop_macro("Flush")
# else
	 , Flush(_base())
# endif
#endif
	{ }
};

} // namespace enums

