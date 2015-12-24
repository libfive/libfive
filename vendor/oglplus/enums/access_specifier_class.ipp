//  File implement/oglplus/enums/access_specifier_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/access_specifier.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<AccessSpecifier> class Transform>
class EnumToClass<Base, AccessSpecifier, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_READ_ONLY
# if defined ReadOnly
#  pragma push_macro("ReadOnly")
#  undef ReadOnly
	Transform<AccessSpecifier::ReadOnly> ReadOnly;
#  pragma pop_macro("ReadOnly")
# else
	Transform<AccessSpecifier::ReadOnly> ReadOnly;
# endif
#endif
#if defined GL_WRITE_ONLY
# if defined WriteOnly
#  pragma push_macro("WriteOnly")
#  undef WriteOnly
	Transform<AccessSpecifier::WriteOnly> WriteOnly;
#  pragma pop_macro("WriteOnly")
# else
	Transform<AccessSpecifier::WriteOnly> WriteOnly;
# endif
#endif
#if defined GL_READ_WRITE
# if defined ReadWrite
#  pragma push_macro("ReadWrite")
#  undef ReadWrite
	Transform<AccessSpecifier::ReadWrite> ReadWrite;
#  pragma pop_macro("ReadWrite")
# else
	Transform<AccessSpecifier::ReadWrite> ReadWrite;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_READ_ONLY
# if defined ReadOnly
#  pragma push_macro("ReadOnly")
#  undef ReadOnly
	 , ReadOnly(_base())
#  pragma pop_macro("ReadOnly")
# else
	 , ReadOnly(_base())
# endif
#endif
#if defined GL_WRITE_ONLY
# if defined WriteOnly
#  pragma push_macro("WriteOnly")
#  undef WriteOnly
	 , WriteOnly(_base())
#  pragma pop_macro("WriteOnly")
# else
	 , WriteOnly(_base())
# endif
#endif
#if defined GL_READ_WRITE
# if defined ReadWrite
#  pragma push_macro("ReadWrite")
#  undef ReadWrite
	 , ReadWrite(_base())
#  pragma pop_macro("ReadWrite")
# else
	 , ReadWrite(_base())
# endif
#endif
	{ }
};

} // namespace enums

