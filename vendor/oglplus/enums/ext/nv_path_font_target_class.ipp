//  File implement/oglplus/enums/ext/nv_path_font_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_font_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVFontTarget> class Transform>
class EnumToClass<Base, PathNVFontTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_STANDARD_FONT_NAME_NV
# if defined Standard
#  pragma push_macro("Standard")
#  undef Standard
	Transform<PathNVFontTarget::Standard> Standard;
#  pragma pop_macro("Standard")
# else
	Transform<PathNVFontTarget::Standard> Standard;
# endif
#endif
#if defined GL_SYSTEM_FONT_NAME_NV
# if defined System
#  pragma push_macro("System")
#  undef System
	Transform<PathNVFontTarget::System> System;
#  pragma pop_macro("System")
# else
	Transform<PathNVFontTarget::System> System;
# endif
#endif
#if defined GL_FILE_NAME_NV
# if defined FileName
#  pragma push_macro("FileName")
#  undef FileName
	Transform<PathNVFontTarget::FileName> FileName;
#  pragma pop_macro("FileName")
# else
	Transform<PathNVFontTarget::FileName> FileName;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_STANDARD_FONT_NAME_NV
# if defined Standard
#  pragma push_macro("Standard")
#  undef Standard
	 , Standard(_base())
#  pragma pop_macro("Standard")
# else
	 , Standard(_base())
# endif
#endif
#if defined GL_SYSTEM_FONT_NAME_NV
# if defined System
#  pragma push_macro("System")
#  undef System
	 , System(_base())
#  pragma pop_macro("System")
# else
	 , System(_base())
# endif
#endif
#if defined GL_FILE_NAME_NV
# if defined FileName
#  pragma push_macro("FileName")
#  undef FileName
	 , FileName(_base())
#  pragma pop_macro("FileName")
# else
	 , FileName(_base())
# endif
#endif
	{ }
};

} // namespace enums

