//  File implement/oglplus/enums/ext/nv_path_format_class.ipp
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
namespace enums {
template <typename Base, template<PathNVFormat> class Transform>
class EnumToClass<Base, PathNVFormat, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PATH_FORMAT_SVG_NV
# if defined SVG
#  pragma push_macro("SVG")
#  undef SVG
	Transform<PathNVFormat::SVG> SVG;
#  pragma pop_macro("SVG")
# else
	Transform<PathNVFormat::SVG> SVG;
# endif
#endif
#if defined GL_PATH_FORMAT_PS_NV
# if defined PS
#  pragma push_macro("PS")
#  undef PS
	Transform<PathNVFormat::PS> PS;
#  pragma pop_macro("PS")
# else
	Transform<PathNVFormat::PS> PS;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PATH_FORMAT_SVG_NV
# if defined SVG
#  pragma push_macro("SVG")
#  undef SVG
	 , SVG(_base())
#  pragma pop_macro("SVG")
# else
	 , SVG(_base())
# endif
#endif
#if defined GL_PATH_FORMAT_PS_NV
# if defined PS
#  pragma push_macro("PS")
#  undef PS
	 , PS(_base())
#  pragma pop_macro("PS")
# else
	 , PS(_base())
# endif
#endif
	{ }
};

} // namespace enums

