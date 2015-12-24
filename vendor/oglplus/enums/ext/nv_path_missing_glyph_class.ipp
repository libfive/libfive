//  File implement/oglplus/enums/ext/nv_path_missing_glyph_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_missing_glyph.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVMissingGlyph> class Transform>
class EnumToClass<Base, PathNVMissingGlyph, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_SKIP_MISSING_GLYPH_NV
# if defined Skip
#  pragma push_macro("Skip")
#  undef Skip
	Transform<PathNVMissingGlyph::Skip> Skip;
#  pragma pop_macro("Skip")
# else
	Transform<PathNVMissingGlyph::Skip> Skip;
# endif
#endif
#if defined GL_USE_MISSING_GLYPH_NV
# if defined Use
#  pragma push_macro("Use")
#  undef Use
	Transform<PathNVMissingGlyph::Use> Use;
#  pragma pop_macro("Use")
# else
	Transform<PathNVMissingGlyph::Use> Use;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_SKIP_MISSING_GLYPH_NV
# if defined Skip
#  pragma push_macro("Skip")
#  undef Skip
	 , Skip(_base())
#  pragma pop_macro("Skip")
# else
	 , Skip(_base())
# endif
#endif
#if defined GL_USE_MISSING_GLYPH_NV
# if defined Use
#  pragma push_macro("Use")
#  undef Use
	 , Use(_base())
#  pragma pop_macro("Use")
# else
	 , Use(_base())
# endif
#endif
	{ }
};

} // namespace enums

