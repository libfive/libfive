//  File implement/oglplus/enums/patch_parameter_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/patch_parameter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PatchParameter> class Transform>
class EnumToClass<Base, PatchParameter, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PATCH_VERTICES
# if defined PatchVertices
#  pragma push_macro("PatchVertices")
#  undef PatchVertices
	Transform<PatchParameter::PatchVertices> PatchVertices;
#  pragma pop_macro("PatchVertices")
# else
	Transform<PatchParameter::PatchVertices> PatchVertices;
# endif
#endif
#if defined GL_PATCH_DEFAULT_OUTER_LEVEL
# if defined PatchDefaultOuterLevel
#  pragma push_macro("PatchDefaultOuterLevel")
#  undef PatchDefaultOuterLevel
	Transform<PatchParameter::PatchDefaultOuterLevel> PatchDefaultOuterLevel;
#  pragma pop_macro("PatchDefaultOuterLevel")
# else
	Transform<PatchParameter::PatchDefaultOuterLevel> PatchDefaultOuterLevel;
# endif
#endif
#if defined GL_PATCH_DEFAULT_INNER_LEVEL
# if defined PatchDefaultInnerLevel
#  pragma push_macro("PatchDefaultInnerLevel")
#  undef PatchDefaultInnerLevel
	Transform<PatchParameter::PatchDefaultInnerLevel> PatchDefaultInnerLevel;
#  pragma pop_macro("PatchDefaultInnerLevel")
# else
	Transform<PatchParameter::PatchDefaultInnerLevel> PatchDefaultInnerLevel;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PATCH_VERTICES
# if defined PatchVertices
#  pragma push_macro("PatchVertices")
#  undef PatchVertices
	 , PatchVertices(_base())
#  pragma pop_macro("PatchVertices")
# else
	 , PatchVertices(_base())
# endif
#endif
#if defined GL_PATCH_DEFAULT_OUTER_LEVEL
# if defined PatchDefaultOuterLevel
#  pragma push_macro("PatchDefaultOuterLevel")
#  undef PatchDefaultOuterLevel
	 , PatchDefaultOuterLevel(_base())
#  pragma pop_macro("PatchDefaultOuterLevel")
# else
	 , PatchDefaultOuterLevel(_base())
# endif
#endif
#if defined GL_PATCH_DEFAULT_INNER_LEVEL
# if defined PatchDefaultInnerLevel
#  pragma push_macro("PatchDefaultInnerLevel")
#  undef PatchDefaultInnerLevel
	 , PatchDefaultInnerLevel(_base())
#  pragma pop_macro("PatchDefaultInnerLevel")
# else
	 , PatchDefaultInnerLevel(_base())
# endif
#endif
	{ }
};

} // namespace enums

