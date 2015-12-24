//  File implement/oglplus/enums/ext/compat_client_attrib_group_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_client_attrib_group.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<CompatibilityClientAttributeGroup> class Transform>
class EnumToClass<Base, CompatibilityClientAttributeGroup, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CLIENT_VERTEX_ARRAY_BIT
# if defined VertexArray
#  pragma push_macro("VertexArray")
#  undef VertexArray
	Transform<CompatibilityClientAttributeGroup::VertexArray> VertexArray;
#  pragma pop_macro("VertexArray")
# else
	Transform<CompatibilityClientAttributeGroup::VertexArray> VertexArray;
# endif
#endif
#if defined GL_CLIENT_PIXEL_STORE_BIT
# if defined PixelStore
#  pragma push_macro("PixelStore")
#  undef PixelStore
	Transform<CompatibilityClientAttributeGroup::PixelStore> PixelStore;
#  pragma pop_macro("PixelStore")
# else
	Transform<CompatibilityClientAttributeGroup::PixelStore> PixelStore;
# endif
#endif
#if defined GL_CLIENT_ALL_ATTRIB_BITS
# if defined AllAttribs
#  pragma push_macro("AllAttribs")
#  undef AllAttribs
	Transform<CompatibilityClientAttributeGroup::AllAttribs> AllAttribs;
#  pragma pop_macro("AllAttribs")
# else
	Transform<CompatibilityClientAttributeGroup::AllAttribs> AllAttribs;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CLIENT_VERTEX_ARRAY_BIT
# if defined VertexArray
#  pragma push_macro("VertexArray")
#  undef VertexArray
	 , VertexArray(_base())
#  pragma pop_macro("VertexArray")
# else
	 , VertexArray(_base())
# endif
#endif
#if defined GL_CLIENT_PIXEL_STORE_BIT
# if defined PixelStore
#  pragma push_macro("PixelStore")
#  undef PixelStore
	 , PixelStore(_base())
#  pragma pop_macro("PixelStore")
# else
	 , PixelStore(_base())
# endif
#endif
#if defined GL_CLIENT_ALL_ATTRIB_BITS
# if defined AllAttribs
#  pragma push_macro("AllAttribs")
#  undef AllAttribs
	 , AllAttribs(_base())
#  pragma pop_macro("AllAttribs")
# else
	 , AllAttribs(_base())
# endif
#endif
	{ }
};

} // namespace enums

