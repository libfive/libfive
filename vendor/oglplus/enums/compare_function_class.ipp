//  File implement/oglplus/enums/compare_function_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/compare_function.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<CompareFunction> class Transform>
class EnumToClass<Base, CompareFunction, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_LEQUAL
# if defined LEqual
#  pragma push_macro("LEqual")
#  undef LEqual
	Transform<CompareFunction::LEqual> LEqual;
#  pragma pop_macro("LEqual")
# else
	Transform<CompareFunction::LEqual> LEqual;
# endif
#endif
#if defined GL_GEQUAL
# if defined GEqual
#  pragma push_macro("GEqual")
#  undef GEqual
	Transform<CompareFunction::GEqual> GEqual;
#  pragma pop_macro("GEqual")
# else
	Transform<CompareFunction::GEqual> GEqual;
# endif
#endif
#if defined GL_LESS
# if defined Less
#  pragma push_macro("Less")
#  undef Less
	Transform<CompareFunction::Less> Less;
#  pragma pop_macro("Less")
# else
	Transform<CompareFunction::Less> Less;
# endif
#endif
#if defined GL_GREATER
# if defined Greater
#  pragma push_macro("Greater")
#  undef Greater
	Transform<CompareFunction::Greater> Greater;
#  pragma pop_macro("Greater")
# else
	Transform<CompareFunction::Greater> Greater;
# endif
#endif
#if defined GL_EQUAL
# if defined Equal
#  pragma push_macro("Equal")
#  undef Equal
	Transform<CompareFunction::Equal> Equal;
#  pragma pop_macro("Equal")
# else
	Transform<CompareFunction::Equal> Equal;
# endif
#endif
#if defined GL_NOTEQUAL
# if defined NotEqual
#  pragma push_macro("NotEqual")
#  undef NotEqual
	Transform<CompareFunction::NotEqual> NotEqual;
#  pragma pop_macro("NotEqual")
# else
	Transform<CompareFunction::NotEqual> NotEqual;
# endif
#endif
#if defined GL_ALWAYS
# if defined Always
#  pragma push_macro("Always")
#  undef Always
	Transform<CompareFunction::Always> Always;
#  pragma pop_macro("Always")
# else
	Transform<CompareFunction::Always> Always;
# endif
#endif
#if defined GL_NEVER
# if defined Never
#  pragma push_macro("Never")
#  undef Never
	Transform<CompareFunction::Never> Never;
#  pragma pop_macro("Never")
# else
	Transform<CompareFunction::Never> Never;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_LEQUAL
# if defined LEqual
#  pragma push_macro("LEqual")
#  undef LEqual
	 , LEqual(_base())
#  pragma pop_macro("LEqual")
# else
	 , LEqual(_base())
# endif
#endif
#if defined GL_GEQUAL
# if defined GEqual
#  pragma push_macro("GEqual")
#  undef GEqual
	 , GEqual(_base())
#  pragma pop_macro("GEqual")
# else
	 , GEqual(_base())
# endif
#endif
#if defined GL_LESS
# if defined Less
#  pragma push_macro("Less")
#  undef Less
	 , Less(_base())
#  pragma pop_macro("Less")
# else
	 , Less(_base())
# endif
#endif
#if defined GL_GREATER
# if defined Greater
#  pragma push_macro("Greater")
#  undef Greater
	 , Greater(_base())
#  pragma pop_macro("Greater")
# else
	 , Greater(_base())
# endif
#endif
#if defined GL_EQUAL
# if defined Equal
#  pragma push_macro("Equal")
#  undef Equal
	 , Equal(_base())
#  pragma pop_macro("Equal")
# else
	 , Equal(_base())
# endif
#endif
#if defined GL_NOTEQUAL
# if defined NotEqual
#  pragma push_macro("NotEqual")
#  undef NotEqual
	 , NotEqual(_base())
#  pragma pop_macro("NotEqual")
# else
	 , NotEqual(_base())
# endif
#endif
#if defined GL_ALWAYS
# if defined Always
#  pragma push_macro("Always")
#  undef Always
	 , Always(_base())
#  pragma pop_macro("Always")
# else
	 , Always(_base())
# endif
#endif
#if defined GL_NEVER
# if defined Never
#  pragma push_macro("Never")
#  undef Never
	 , Never(_base())
#  pragma pop_macro("Never")
# else
	 , Never(_base())
# endif
#endif
	{ }
};

} // namespace enums

