/**
 *  @file oglplus/shapes/vert_attr_info.hpp
 *  @brief Classes providing additional information about the shape builders
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_VERT_ATTR_INFO_1107121519_HPP
#define OGLPLUS_SHAPES_VERT_ATTR_INFO_1107121519_HPP

#include <oglplus/string/ref.hpp>

#include <tuple>

namespace oglplus {
namespace shapes {

template <class ShapeBuilder, class VertexAttributeTag>
struct VertexAttribInfo;

#define OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(ATTR_NAME, GETTER_NAME) \
struct Vertex ## GETTER_NAME ## Tag { }; \
 \
template <class ShapeBuilder> \
struct VertexAttribInfo<ShapeBuilder, Vertex ## GETTER_NAME ## Tag> \
{ \
public: \
	static const GLchar* _att_name(void) \
	{ \
		return #ATTR_NAME; \
	} \
 \
	template <typename T> \
	struct _getter_proc \
	{ \
		typedef GLuint (*type)(const ShapeBuilder&, std::vector<T>&); \
	};\
 \
	template <typename T> \
	static GLuint _getter_wrapper( \
		const ShapeBuilder& make, \
		std::vector<T>& dest \
	) \
	{ \
		return make.GETTER_NAME(dest); \
	} \
 \
	template <typename T> \
	static typename _getter_proc<T>::type _getter(TypeTag<T>) \
	{ \
		return &VertexAttribInfo::_getter_wrapper<T>; \
	} \
};

OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(Position, Positions)
OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(Normal, Normals)
OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(Tangent, Tangents)
OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(Bitangent, Bitangents)
OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(TexCoord, TexCoordinates)
OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO(Material, MaterialNumbers)

#undef OGLPLUS_SHAPES_HLPR_MAKE_VERT_ATTR_INFO

#if OGLPLUS_DOCUMENTATION_ONLY
/// Informations about a vertex attribute getter function of a @c ShapeBuilder
/**
 */
class VertexAttribsInfo<ShapeBuiler>
{
public:
	/// Returns true if @c ShapeBuilder makes vertex attributes with @a name
	bool MakesVertexAttrib(StrCRef name) const;

	/// Returns the vertex attribute getter function by its @a name
	/** This functions tries to find a vertex attribute getter function
	 *  by the name of the vertex attribute. Returns a pointer to this
	 *  function if the @c ShapeBuilder provides one, returns a null pointer
	 *  otherwise.
	 *
	 *  The returned getter function has the following signature:
	 *  @code
	 *  GLuint GetterFunction(const ShapeBuilder&, std::vector<T>&);
	 *  @endcode
	 */
	template <typename T>
	static GetterFunction VertexAttribGetter(StrCRef name);
};
#else
template <class ShapeBuilder, class VertexAttribTags, std::size_t N>
class VertexAttribsInfoBase
{
private:
	static bool _has_vertex_attrib(
		StrCRef,
		std::integral_constant<std::size_t, N>,
		std::integral_constant<std::size_t, N>
	)
	{
		return false;
	}

	template <std::size_t I>
	static bool _has_vertex_attrib(
		StrCRef name,
		std::integral_constant<std::size_t, I>,
		std::integral_constant<std::size_t, N>
	)
	{
		auto info = VertexAttribInfo<
			ShapeBuilder,
			typename std::tuple_element<I, VertexAttribTags>::type
		>();
		if(name == info._att_name()) return true;
		else return _has_vertex_attrib(
			name,
			std::integral_constant<std::size_t, I+1>(),
			std::integral_constant<std::size_t, N>()
		);
	}
protected:
	static bool _has_vertex_attrib(StrCRef name)
	{
		return _has_vertex_attrib(
			name,
			std::integral_constant<std::size_t, 0>(),
			std::integral_constant<std::size_t, N>()
		);
	}

	template <typename T>
	struct _getter_proc
	{
		typedef GLuint (*type)(const ShapeBuilder&, std::vector<T>&);
	};
private:
	template <typename T>
	static typename _getter_proc<T>::type
	_find_getter(
		TypeTag<T>,
		StrCRef,
		std::integral_constant<std::size_t, N>,
		std::integral_constant<std::size_t, N>
	)
	{
		return nullptr;
	}

	template <typename T, std::size_t I>
	static typename _getter_proc<T>::type
	_find_getter(
		TypeTag<T> selector,
		StrCRef name,
		std::integral_constant<std::size_t, I>,
		std::integral_constant<std::size_t, N>
	)
	{
		auto info = VertexAttribInfo<
			ShapeBuilder,
			typename std::tuple_element<I, VertexAttribTags>::type
		>();
		if(std::strcmp(name.c_str(), info._att_name()) == 0)
		{
			return info._getter(selector);
		}
		return _find_getter(
			selector,
			name,
			std::integral_constant<std::size_t, I+1>(),
			std::integral_constant<std::size_t, N>()
		);
	}

protected:
	template <typename T>
	static typename _getter_proc<T>::type
	_find_getter(TypeTag<T> selector, StrCRef name)
	{
		return _find_getter(
			selector,
			name,
			std::integral_constant<std::size_t, 0>(),
			std::integral_constant<std::size_t, N>()
		);
	}
};

template <class ShapeBuilder, class VertexAttribTags>
class VertexAttribsInfo
 : public VertexAttribsInfoBase<
	ShapeBuilder,
	VertexAttribTags,
	std::tuple_size<VertexAttribTags>::value
>
{
private:
	typedef VertexAttribsInfoBase<
		ShapeBuilder,
		VertexAttribTags,
		std::tuple_size<VertexAttribTags>::value
	> _base;
public:
	bool MakesVertexAttrib(StrCRef name) const
	{
		return _base::_has_vertex_attrib(name);
	}

	template <typename T>
	static typename _base::template _getter_proc<T>::type
	VertexAttribGetter(
		const std::vector<T>& /*selector*/,
		StrCRef name
	)
	{
		return _base::_find_getter(TypeTag<T>(), name);
	}
};
#endif

} // shapes
} // oglplus

#endif // include guard
