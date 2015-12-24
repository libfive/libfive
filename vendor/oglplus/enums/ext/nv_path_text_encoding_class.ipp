//  File implement/oglplus/enums/ext/nv_path_text_encoding_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_text_encoding.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVTextEncoding> class Transform>
class EnumToClass<Base, PathNVTextEncoding, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_UTF8_NV
# if defined UTF8
#  pragma push_macro("UTF8")
#  undef UTF8
	Transform<PathNVTextEncoding::UTF8> UTF8;
#  pragma pop_macro("UTF8")
# else
	Transform<PathNVTextEncoding::UTF8> UTF8;
# endif
#endif
#if defined GL_UTF16_NV
# if defined UTF16
#  pragma push_macro("UTF16")
#  undef UTF16
	Transform<PathNVTextEncoding::UTF16> UTF16;
#  pragma pop_macro("UTF16")
# else
	Transform<PathNVTextEncoding::UTF16> UTF16;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_UTF8_NV
# if defined UTF8
#  pragma push_macro("UTF8")
#  undef UTF8
	 , UTF8(_base())
#  pragma pop_macro("UTF8")
# else
	 , UTF8(_base())
# endif
#endif
#if defined GL_UTF16_NV
# if defined UTF16
#  pragma push_macro("UTF16")
#  undef UTF16
	 , UTF16(_base())
#  pragma pop_macro("UTF16")
# else
	 , UTF16(_base())
# endif
#endif
	{ }
};

} // namespace enums

