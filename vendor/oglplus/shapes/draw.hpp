/**
 *  @file oglplus/shapes/draw.hpp
 *  @brief Implementation of shape draw instructions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_DRAW_1107121519_HPP
#define OGLPLUS_SHAPES_DRAW_1107121519_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/primitive_type.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/utils/type_tag.hpp>

#include <vector>
#include <cassert>

namespace oglplus {
namespace shapes {

/// Helper class storing information about shape element index datatype
/**
 *  @note Do not use this class directly.
 */
class ElementIndexInfo
{
private:
	const std::size_t _sizeof_index;
	const oglplus::DataType _index_data_type;

	template <typename IT>
	static
	std::size_t _do_get_sizeof_index(TypeTag<std::vector<IT>>)
	OGLPLUS_NOEXCEPT(true)
	{
		return sizeof(IT);
	}

	template <class ShapeBuilder>
	static
	std::size_t _get_sizeof_index(const ShapeBuilder&)
	OGLPLUS_NOEXCEPT(true)
	{
		return _do_get_sizeof_index(
			TypeTag<typename ShapeBuilder::IndexArray>()
		);
	}

	template <typename IT>
	static
	oglplus::DataType _do_get_index_data_type(TypeTag<std::vector<IT>>)
	{
		return oglplus::GetDataType<IT>();
	}

	template <class ShapeBuilder>
	static
	oglplus::DataType _get_index_data_type(const ShapeBuilder&)
	{
		return _do_get_index_data_type(
			TypeTag<typename ShapeBuilder::IndexArray>()
		);
	}
public:
	template <class ShapeBuilder>
	ElementIndexInfo(const ShapeBuilder& builder)
	 : _sizeof_index(_get_sizeof_index(builder))
	 , _index_data_type(_get_index_data_type(builder))
	{ }

	/// Returns the size (in bytes) of index type used by ShapeBuilder
	size_t Size(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _sizeof_index;
	}

	/// Returns the GL datatype of index type used by ShapeBuilder
	oglplus::DataType DataType(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _index_data_type;
	}
};

} // namespace shapes

/// Enumeration of drawing methods
OGLPLUS_ENUM_CLASS_BEGIN(ShapeDrawOperationMethod, GLuint)
	OGLPLUS_ENUM_CLASS_VALUE(DrawArrays, 0)
	OGLPLUS_ENUM_CLASS_COMMA
	OGLPLUS_ENUM_CLASS_VALUE(DrawElements, 1)
	// TODO
OGLPLUS_ENUM_CLASS_END(ShapeDrawOperationMethod)

namespace shapes {

/// Structure containing information about how to draw a part of a shape
/**
 *  @note Do not use this class directly, use DrawingInstructions returned
 *  by the various shape builder classes instead.
 */
struct DrawOperation
{
	/// Enumeration of drawing methods
	typedef oglplus::ShapeDrawOperationMethod Method;

	/// The method to be used to draw
	Method method;
	/// The primitive type to be used to draw
	PrimitiveType mode;

	/// The first element
	GLuint first;

	/// Count of elements
	GLuint count;

	/// Special constant for disabling primitive restart
	static GLuint NoRestartIndex(void)
	{
		return ~GLuint(0);
	}

	/// Primitive restart index
	/**
	 *  @see NoRestartIndex
	 */
	GLuint restart_index;

	/// The phase of the drawing process
	/** The phase is a shape-builder-specific value that indicates
	 *  which part of the shape is being rendered. Together with a
	 *  "driver" function it can be used to change the shading
	 *  program parameters (like the values of uniform variables)
	 *  to switch the whole program or change GL state.
	 */
	GLuint phase;

	void Draw(
		const ElementIndexInfo& index_info,
		GLuint inst_count = 1,
		GLuint base_inst = 0
	) const
	{
		this->Draw_(
			IndexPtr_(index_info),
			index_info.DataType(),
			inst_count,
			base_inst
		);
	}

	/// Draw the part of a shape
	template <typename IT>
	void Draw(
		const std::vector<IT>& indices,
		GLuint inst_count = 1,
		GLuint base_inst = 0
	) const
	{
		this->Draw_(
			IndexPtr_(indices),
			IndexDataType_(indices),
			inst_count,
			base_inst
		);
	}
private:

	template <typename IT>
	static DataType IndexDataType_(const std::vector<IT>&)
	{
		return GetDataType<IT>();
	}

	template <typename IT>
	const void* IndexPtr_(const std::vector<IT>& indices) const
	{
		const IT* base = indices.empty() ? nullptr : &indices.front();
		return reinterpret_cast<const void*>(base + first);
	}

	const void* IndexPtr_(const ElementIndexInfo& index_info) const
	{
		return reinterpret_cast<const void*>(first * index_info.Size());
	}

	void SetupPrimitiveRestart_(void) const;
	void CleanupPrimitiveRestart_(void) const;

	void Draw_(
		const void* indices,
		DataType index_data_type,
		GLuint inst_count,
		GLuint base_inst
	) const;

	void DrawArrays_(GLuint inst_count, GLuint base_inst) const;

	void DrawElements_(
		const void* indices,
		DataType index_data_type,
		GLuint inst_count,
		GLuint base_inst
	) const;
};

class DrawingInstructionWriter;

/// Class encapsulating the instructions for drawing of a shape
/** Instances of this class are produced by various shape builder
 *  classes implemented in the @c oglplus::shapes namespace and
 *  can be used to draw a shape from the data stored in vertex
 *  attrib buffers initialized from data returned by the member
 *  functions of the shape builder classes.
 *  @note Do not instantiate this class directly, use the appropriate
 *  member functions of the shape builder classes.
 *
 *  @see Cube
 */
class DrawingInstructions
{
private:

	typedef std::vector<DrawOperation> DrawOperationSeq;
	DrawOperationSeq _ops;

	DrawingInstructions(void)
	{ }

	DrawingInstructions(DrawOperationSeq&& ops)
	 : _ops(std::move(ops))
	{ }

	friend class DrawingInstructionWriter;

	// helper functor used as DrawFun in Draw_
	template <class IndexArray>
	struct DrawFromIndices_
	{
		const IndexArray& _indices;

		DrawFromIndices_(const IndexArray& indices)
		 : _indices(indices)
		{ }

		void operator()(
			const DrawOperation& op,
			GLuint inst_count,
			GLuint base_inst
		) const
		{
			op.Draw(_indices, inst_count, base_inst);
		}
	};

	// helper functor used as DrawFun in Draw_
	struct DrawFromIndexInfo_
	{
		ElementIndexInfo _index_info;

		DrawFromIndexInfo_(const ElementIndexInfo& index_info)
		 : _index_info(index_info)
		{ }

		void operator()(
			const DrawOperation& op,
			GLuint inst_count,
			GLuint base_inst
		) const
		{
			op.Draw(_index_info, inst_count, base_inst);
		}
	};

	/// Draw the shape from data in currently bound VBOs indexed by indices
	template <typename DrawFun, typename Driver>
	void Draw_(
		const DrawFun& draw_fun,
		const GLuint inst_count,
		const GLuint base_inst,
		const Driver& driver
	) const
	{
		auto i=_ops.begin(),e=_ops.end();
		if(i != e)
		{
			bool do_draw;
			if(driver(i->phase))
			{
				do_draw = true;
				draw_fun(*i, inst_count, base_inst);
			}
			else do_draw = false;
			GLuint prev_phase = i->phase;
			++i;

			while(i!=e)
			{
				if(prev_phase != i->phase)
				{
					do_draw = driver(i->phase);
					prev_phase = i->phase;
				}
				if(do_draw) draw_fun(*i, inst_count, base_inst);
				++i;
			}
		}
	}
public:
	DrawingInstructions(DrawingInstructions&& temp)
	 : _ops(std::move(temp._ops))
	{ }

	DrawingInstructions(const DrawingInstructions& other)
	 : _ops(other._ops)
	{ }

	const std::vector<DrawOperation>& Operations(void) const
	{
		return _ops;
	}

	struct DefaultDriver
	{
		inline bool operator()(GLuint /*phase*/) const
		{
			return true;
		}
	};

	template <typename IT, typename Driver>
	void Draw(
		const std::vector<IT>& indices,
		GLuint inst_count,
		GLuint base_inst,
		Driver driver
	) const
	{
		this->Draw_(
			DrawFromIndices_<std::vector<IT>>(indices),
			inst_count,
			base_inst,
			driver
		);
	}

	template <typename IT>
	void Draw(
		const std::vector<IT>& indices,
		GLuint inst_count = 1,
		GLuint base_inst = 0
	) const
	{
		this->Draw_(
			DrawFromIndices_<std::vector<IT>>(indices),
			inst_count,
			base_inst,
			DefaultDriver()
		);
	}

	template <typename Driver>
	void Draw(
		const ElementIndexInfo& index_info,
		GLuint inst_count,
		GLuint base_inst,
		Driver driver
	) const
	{
		this->Draw_(
			DrawFromIndexInfo_(index_info),
			inst_count,
			base_inst,
			driver
		);
	}

	void Draw(
		const ElementIndexInfo& index_info,
		GLuint inst_count = 1,
		GLuint base_inst = 0
	) const
	{
		this->Draw_(
			DrawFromIndexInfo_(index_info),
			inst_count,
			base_inst,
			DefaultDriver()
		);
	}


};

// Helper base class for shape builder classes making the drawing instructions
class DrawingInstructionWriter
{
private:
	typedef DrawingInstructions::DrawOperationSeq Operations;
protected:
	static DrawingInstructions MakeInstructions(void)
	{
		return DrawingInstructions();
	}

	static void AddInstruction(
		DrawingInstructions& instr,
		const DrawOperation& operation
	)
	{
		instr._ops.push_back(operation);
	}

	static DrawingInstructions MakeInstructions(const DrawOperation& operation)
	{
		DrawingInstructions instr;
		instr._ops.push_back(operation);
		return instr;
	}

	static DrawingInstructions MakeInstructions(Operations&& ops)
	{
		return DrawingInstructions(std::forward<Operations>(ops));
	}
};

struct DrawMode
{
	struct Default { };
	struct WithAdjacency { };
	struct Quads { };
	struct Patches { };
	struct Edges { };
};


} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/draw.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
