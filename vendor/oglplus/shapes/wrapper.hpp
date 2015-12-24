/**
 *  @file oglplus/shapes/wrapper.hpp
 *  @brief Wrapper managing VAOs, VBOs and instructions used to render a shape
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_WRAPPER_1202020923_HPP
#define OGLPLUS_SHAPES_WRAPPER_1202020923_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/config/basic.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/object/array.hpp>
#include <oglplus/object/optional.hpp>
#include <oglplus/vertex_array.hpp>
#include <oglplus/vertex_attrib.hpp>
#include <oglplus/buffer.hpp>
#include <oglplus/program.hpp>
#include <oglplus/context.hpp>

#include <oglplus/math/sphere.hpp>

#include <oglplus/shapes/draw.hpp>
#include <oglplus/shapes/vert_attr_info.hpp>

#include <vector>
#include <functional>
#include <iterator>
#include <cassert>

namespace oglplus {
namespace shapes {

/// Wraps instructions and VAO+VBOs used to render a shape built by a ShapeBuilder
class ShapeWrapperBase
{
protected:
	FaceOrientation _face_winding;
	// helper object encapsulating shape drawing instructions
	shapes::DrawingInstructions _shape_instr;

	// index type properties
	shapes::ElementIndexInfo _index_info;

	Context _gl;

	// A vertex array object for the rendered shape
	Optional<VertexArray> _vao;

	// VBOs for the shape's vertex attributes
	Array<Buffer> _vbos;

	// numbers of values per vertex for the individual attributes
	std::vector<GLuint> _npvs;

	// names of the individual vertex attributes
	std::vector<String> _names;

	// the origin and radius of the bounding sphere
	Spheref _bounding_sphere;

	template <class ShapeBuilder, class ShapeIndices, typename Iterator>
	void _init(
		const ShapeBuilder& builder,
		const ShapeIndices& shape_indices,
		Iterator name,
		Iterator end
	)
	{
		NoVertexArray().Bind();
		typename ShapeBuilder::VertexAttribs vert_attr_info;
		OGLPLUS_FAKE_USE(vert_attr_info);

		unsigned i = 0;
		std::vector<GLfloat> data;
		while(name != end)
		{
			auto getter = vert_attr_info.VertexAttribGetter(
				data,
				*name
			);
			if(getter != nullptr)
			{
				_vbos[i].Bind(Buffer::Target::Array);
				_npvs[i] = getter(builder, data);
				_names[i] = *name;

				Buffer::Data(Buffer::Target::Array, data);
			}
			++name;
			++i;
		}

		if(!shape_indices.empty())
		{
			assert((i+1) == _npvs.size());
			assert((i+1) == _vbos.size());

			_npvs[i] = 1;
			_vbos[i].Bind(Buffer::Target::ElementArray);
			Buffer::Data(
				Buffer::Target::ElementArray,
				shape_indices
			);
		}

		builder.BoundingSphere(_bounding_sphere);
	}
public:
	template <typename Iterator, class ShapeBuilder, class Selector>
	ShapeWrapperBase(
		Iterator names_begin,
		Iterator names_end,
		const ShapeBuilder& builder,
		Selector selector
	): _face_winding(builder.FaceWinding())
	 , _shape_instr(builder.Instructions(selector))
	 , _index_info(builder)
	 , _vbos(std::size_t(std::distance(names_begin, names_end)+1))
	 , _npvs(std::size_t(std::distance(names_begin, names_end)+1), 0)
	 , _names(std::size_t(std::distance(names_begin, names_end)))
	{
		this->_init(
			builder,
			builder.Indices(selector),
			names_begin,
			names_end
		);
	}

	ShapeWrapperBase(ShapeWrapperBase&& temp)
	 : _face_winding(temp._face_winding)
	 , _shape_instr(std::move(temp._shape_instr))
	 , _index_info(temp._index_info)
	 , _gl(std::move(temp._gl))
	 , _vao(std::move(temp._vao))
	 , _vbos(std::move(temp._vbos))
	 , _npvs(std::move(temp._npvs))
	 , _names(std::move(temp._names))
	{ }

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	ShapeWrapperBase(const ShapeWrapperBase&) = delete;
#else
private:
	ShapeWrapperBase(const ShapeWrapperBase&);
public:
#endif

	VertexArray VAOForProgram(const ProgramOps& prog) const;
	void SetupForProgram(ProgramName progName) const;

	void UseInProgram(const ProgramOps& prog)
	{
		_vao = VAOForProgram(prog);
		assert(GetGLName(_vao) != 0u);
	}

	void Use(void)
	{
		if (GetGLName(_vao) == 0u) {
			// WARNING: here should be valid shader (same as in UseInProgram call)!
			SetupForProgram(Program::Binding());
		} else {
			_vao.Bind();
		}
	}

	FaceOrientation FaceWinding(void) const
	{
		return _face_winding;
	}

	void Draw(void) const
	{
		_gl.FrontFace(_face_winding);
		_shape_instr.Draw(_index_info, 1, 0);
	}

	void Draw(GLuint inst_count) const
	{
		_gl.FrontFace(_face_winding);
		_shape_instr.Draw(_index_info, inst_count, 0);
	}

	void Draw(GLuint inst_count, GLuint base_inst) const
	{
		_gl.FrontFace(_face_winding);
		_shape_instr.Draw(_index_info, inst_count, base_inst);
	}

	void Draw(const std::function<bool (GLuint)>& drawing_driver) const
	{
		_gl.FrontFace(_face_winding);
		_shape_instr.Draw(_index_info, 1, 0, drawing_driver);
	}

	const Spheref& BoundingSphere(void) const
	{
		return _bounding_sphere;
	}
};

/// Wraps instructions and VBOs and VAO used to render a shape built by a ShapeBuilder
template <typename Selector>
class ShapeWrapperTpl
 : public ShapeWrapperBase
{
private:
	static Selector _sel(void) { return Selector(); }
public:
	ShapeWrapperTpl(ShapeWrapperTpl&& temp)
	 : ShapeWrapperBase(static_cast<ShapeWrapperBase&&>(temp))
	{ }

	template <typename StdRange, class ShapeBuilder>
	ShapeWrapperTpl(
		const StdRange& names,
		const ShapeBuilder& builder
	): ShapeWrapperBase(names.begin(), names.end(), builder, _sel())
	{ }

	template <typename StdRange, class ShapeBuilder>
	ShapeWrapperTpl(
		const StdRange& names,
		const ShapeBuilder& builder,
		const ProgramOps& prog
	): ShapeWrapperBase(names.begin(), names.end(), builder, _sel())
	{
		UseInProgram(prog);
	}

#if !OGLPLUS_NO_INITIALIZER_LISTS
	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const std::initializer_list<const GLchar*>& names,
		const ShapeBuilder& builder
	): ShapeWrapperBase(names.begin(), names.end(), builder, _sel())
	{ }

	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const std::initializer_list<const GLchar*>& names,
		const ShapeBuilder& builder,
		const ProgramOps& prog
	): ShapeWrapperBase(names.begin(), names.end(), builder, _sel())
	{
		UseInProgram(prog);
	}
#endif

	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const GLchar** names,
		unsigned name_count,
		const ShapeBuilder& builder
	): ShapeWrapperBase(names, names+name_count, builder, _sel())
	{ }

	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const GLchar** names,
		unsigned name_count,
		const ShapeBuilder& builder,
		const ProgramOps& prog
	): ShapeWrapperBase(names, names+name_count, builder, _sel())
	{
		UseInProgram(prog);
	}

	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const GLchar* name,
		const ShapeBuilder& builder
	): ShapeWrapperBase(&name, (&name)+1, builder, _sel())
	{ }

	template <class ShapeBuilder>
	ShapeWrapperTpl(
		const GLchar* name,
		const ShapeBuilder& builder,
		const ProgramOps& prog
	): ShapeWrapperBase(&name, (&name)+1, builder, _sel())
	{
		UseInProgram(prog);
	}
};

typedef ShapeWrapperTpl<DrawMode::Default> ShapeWrapper;
typedef ShapeWrapperTpl<DrawMode::WithAdjacency> ShapeWrapperWithAdjacency;

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/wrapper.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
