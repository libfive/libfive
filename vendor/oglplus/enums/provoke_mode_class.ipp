//  File implement/oglplus/enums/provoke_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/provoke_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ProvokeMode> class Transform>
class EnumToClass<Base, ProvokeMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FIRST_VERTEX_CONVENTION
# if defined FirstVertexConvention
#  pragma push_macro("FirstVertexConvention")
#  undef FirstVertexConvention
	Transform<ProvokeMode::FirstVertexConvention> FirstVertexConvention;
#  pragma pop_macro("FirstVertexConvention")
# else
	Transform<ProvokeMode::FirstVertexConvention> FirstVertexConvention;
# endif
#endif
#if defined GL_LAST_VERTEX_CONVENTION
# if defined LastVertexConvention
#  pragma push_macro("LastVertexConvention")
#  undef LastVertexConvention
	Transform<ProvokeMode::LastVertexConvention> LastVertexConvention;
#  pragma pop_macro("LastVertexConvention")
# else
	Transform<ProvokeMode::LastVertexConvention> LastVertexConvention;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FIRST_VERTEX_CONVENTION
# if defined FirstVertexConvention
#  pragma push_macro("FirstVertexConvention")
#  undef FirstVertexConvention
	 , FirstVertexConvention(_base())
#  pragma pop_macro("FirstVertexConvention")
# else
	 , FirstVertexConvention(_base())
# endif
#endif
#if defined GL_LAST_VERTEX_CONVENTION
# if defined LastVertexConvention
#  pragma push_macro("LastVertexConvention")
#  undef LastVertexConvention
	 , LastVertexConvention(_base())
#  pragma pop_macro("LastVertexConvention")
# else
	 , LastVertexConvention(_base())
# endif
#endif
	{ }
};

} // namespace enums

