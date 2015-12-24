/**
 *  @file oglplus/shapes/revolve.hpp
 *  @brief Shape builders revolving splines in full circles
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_REVOLVE_1107121519_HPP
#define OGLPLUS_SHAPES_REVOLVE_1107121519_HPP

#include <oglplus/shapes/draw.hpp>
#include <oglplus/face_mode.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/math/vector.hpp>
#include <oglplus/math/matrix.hpp>
#include <oglplus/math/sphere.hpp>

#include <cmath>

namespace oglplus {
namespace shapes {

/// Class providing attributes and instructions for rendering of a revolved shape
template <typename Type>
class RevolveY
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	const std::vector<Type> _sections, _section_factors;
	const std::size_t _rings;

	const std::vector<Vector<Type, 3>> _positions_0, _positions_1;
	const std::vector<Vector<Type, 3>> _normals_0, _normals_1;
	const std::vector<Vector<Type, 3>> _tex_coords_0, _tex_coords_1;

	Type _radius;

	static Vector<Type, 3> _mix(
		const Vector<Type, 3>& a,
		const Vector<Type, 3>& b,
		Type factor
	)
	{
		if(factor < Type(0)) factor = Type(0);
		if(factor > Type(1)) factor = Type(1);
		return a * (Type(1) - factor) + b * factor;
	}

	Vector<Type, 3> _get_position(
		std::size_t ring,
		std::size_t section
	) const
	{
		return _mix(
			_positions_0[ring],
			_positions_1[ring],
			_section_factors[section]
		);
	}

	Vector<Type, 3> _get_normal(
		std::size_t ring,
		std::size_t section
	) const
	{
		return _mix(
			_normals_0[ring],
			_normals_1[ring],
			_section_factors[section]
		);
	}

	Vector<Type, 3> _get_tex_coord(
		std::size_t ring,
		std::size_t section
	) const
	{
		return _mix(
			_tex_coords_0[ring],
			_tex_coords_1[ring],
			_section_factors[section]
		);
	}

	static std::vector<Type> _make_default_sections(std::size_t sections)
	{
		std::vector<Type> result(sections + 1);
		const Type s_step = Type(1) / Type(sections);
		Type s = Type(0);
		for(auto i=result.begin(), e=result.end(); i!=e; ++i, s+=s_step)
			*i = s;
		return result;
	}

	static std::vector<Vector<Type, 3>> _calculate_normals(
		const std::vector<Vector<Type, 3>>& pos,
		const std::vector<Vector<Type, 3>>& nml
	)
	{
		if(!nml.empty())
		{
			assert(pos.size() == nml.size());
			return nml;
		}
		std::vector<Vector<Type, 3>> result(pos.size());

		const std::size_t n = result.size()-1;
		const Vec3f tgnt(0.0f, 0.0f, -1.0f);

		result[0] = Normalized(Cross(tgnt, pos[1] - pos[0]));
		for(std::size_t i=1; i!=n; ++i)
		{
			result[i] = Normalized(Cross(tgnt, pos[i+1]-pos[i-1]));
		}
		result[n] = Normalized(Cross(tgnt, pos[n] - pos[n-1]));
		return result;
	}

	void _check(void)
	{
		assert(_rings > 1);
		assert(_sections.size() > 2);
		assert(_sections.size() == _section_factors.size());

		assert(_positions_0.size() == _rings);
		assert(_positions_1.size() == _rings);
		assert(_normals_0.size() == _rings);
		assert(_normals_1.size() == _rings);
		assert(_tex_coords_0.size() == _rings);
		assert(_tex_coords_1.size() == _rings);
	}

	void _calc_radius(void)
	{
		_radius = Type(0);
		Type l;
		for(unsigned i=0; i!=_rings; ++i)
		{
			l = Length(_positions_0[i]);
			if(_radius < l) _radius = l;
			l = Length(_positions_1[i]);
			if(_radius < l) _radius = l;
		}
	}
public:
	using DrawMode::Default;

	/// Creates a shape by revolving curve approximation around the y-axis
	RevolveY(
		unsigned sections,
		const std::vector<Vector<Type, 3>>& positions,
		const std::vector<Vector<Type, 3>>& normals,
		const std::vector<Vector<Type, 3>>& tex_coords
	): _sections(_make_default_sections(sections))
	 , _section_factors(_sections.size(), Type(0))
	 , _rings(positions.size())
	 , _positions_0(positions)
	 , _positions_1(_positions_0)
	 , _normals_0(_calculate_normals(_positions_0, normals))
	 , _normals_1(_normals_0)
	 , _tex_coords_0(tex_coords)
	 , _tex_coords_1(_tex_coords_0)
	{
		_check();
		_calc_radius();
	}

	/// Creates a shape by revolving curve approximation around the y-axis
	RevolveY(
		const std::vector<Type>& section_factors,
		const std::vector<Vector<Type, 3>>& positions_0,
		const std::vector<Vector<Type, 3>>& positions_1,
		const std::vector<Vector<Type, 3>>& normals_0,
		const std::vector<Vector<Type, 3>>& normals_1,
		const std::vector<Vector<Type, 3>>& tex_coords_0,
		const std::vector<Vector<Type, 3>>& tex_coords_1
	): _sections(_make_default_sections(section_factors.size()-1))
	 , _section_factors(section_factors)
	 , _rings(positions_0.size())
	 , _positions_0(positions_0)
	 , _positions_1(positions_1)
	 , _normals_0(_calculate_normals(_positions_0, normals_0))
	 , _normals_1(_calculate_normals(_positions_1, normals_1))
	 , _tex_coords_0(tex_coords_0)
	 , _tex_coords_1(tex_coords_1)
	{
		_check();
		_calc_radius();
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CW;
	}

	/// Makes vertex coordinates and returns number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		dest.resize(_rings * _sections.size() * 3);
		unsigned k = 0;
		//
		for(std::size_t si=0, sn=_sections.size(); si!=sn; ++si)
		{
			const auto angle = FullCircles(_sections[si]);
			const auto mat = ModelMatrix<Type>::RotationY(angle);

			for(std::size_t r=0; r!=_rings; ++r)
			{
				const Vector<Type, 4> in(_get_position(r, si), 1);
				const Vector<Type, 4> out = mat * in;

				dest[k++] = T(out.x());
				dest[k++] = T(out.y());
				dest[k++] = T(out.z());
			}
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes vertex normals and returns number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		dest.resize(_rings * _sections.size() * 3);
		unsigned k = 0;
		//
		for(std::size_t si=0, sn=_sections.size(); si!=sn; ++si)
		{
			const auto angle = FullCircles(_sections[si]);
			const auto mat = ModelMatrix<Type>::RotationY(angle);

			for(std::size_t r=0; r!=_rings; ++r)
			{
				const Vector<Type, 4> in(_get_normal(r, si), 0);
				const Vector<Type, 4> out = mat * in;

				dest[k++] = T(out.x());
				dest[k++] = T(out.y());
				dest[k++] = T(out.z());
			}
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes vertex tangents and returns number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		dest.resize(_rings * _sections.size() * 3);
		std::size_t k = 0;

		const Vector<Type, 4> in(0.0, 0.0, -1.0, 0.0);

		for(std::size_t si=0, sn=_sections.size(); si!=sn; ++si)
		{
			const auto angle = FullCircles(_sections[si]);
			const auto mat = ModelMatrix<Type>::RotationY(angle);
			const auto out = mat * in;

			for(std::size_t r=0; r!=_rings; ++r)
			{
				dest[k++] = T(out.x());
				dest[k++] = T(out.y());
				dest[k++] = T(out.z());
			}
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes vertex bi-tangents and returns number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		dest.resize(_rings * _sections.size() * 3);
		unsigned k = 0;

		const Vector<Type, 3> tgt(0.0, 0.0, -1.0);

		for(std::size_t si=0, sn=_sections.size(); si!=sn; ++si)
		{
			const auto angle = FullCircles(_sections[si]);
			const auto mat = ModelMatrix<Type>::RotationY(angle);

			for(std::size_t r=0; r!=_rings; ++r)
			{
				const Vector<Type, 3> nml(_get_normal(r, si));
				const Vector<Type, 4> in(Cross(nml, tgt), 0);
				const auto out = mat * in;

				dest[k++] = T(out.x());
				dest[k++] = T(out.y());
				dest[k++] = T(out.z());
			}
		}
		assert(k == dest.size());
		return 3;
	}

	/// Makes texture coordinates and returns number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		dest.resize(_rings * _sections.size() * 3);
		std::size_t k = 0;
		//
		const Vector<Type, 4> in(0.0, 0.0, -1.0, 0.0);

		for(std::size_t si=0, sn=_sections.size(); si!=sn; ++si)
		{
			const T u_mult = _sections[si];

			for(std::size_t r=0; r!=_rings; ++r)
			{
				auto tc = _get_tex_coord(r, si);
				dest[k++] = T(tc.x()*u_mult);
				dest[k++] = T(tc.y());
				dest[k++] = T(tc.z());
			}
		}
		assert(k == dest.size());
		return 3;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** Torus provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normal vectors (Normals)
	 *  - "Tangent" the vertex tangent vector (Tangents)
	 *  - "Bitangent" the vertex bi-tangent vector (Bitangents)
	 *  - "TexCoord" the ST texture coordinates (TexCoordinates)
	 */
	typedef VertexAttribsInfo<RevolveY> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		RevolveY,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag,
			VertexTangentsTag,
			VertexBitangentsTag,
			VertexTexCoordinatesTag
		>
	> VertexAttribs;
#endif

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		// TODO
		bounding_sphere = oglplus::Sphere<T>(
			T(0),
			T(0),
			T(0),
			T(_radius)
		);
	}

	/// The type of index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		const std::size_t sn = _sections.size() - 1;
#ifdef GL_PRIMITIVE_RESTART
		const std::size_t n = sn * (2 * _rings + 1);
#else
		const std::size_t n = sn * (2 * _rings + 2);
#endif
		//
		IndexArray indices(n);
		std::size_t k = 0;
		std::size_t offs = 0;
		// the triangle strips
		for(unsigned s=0; s!=sn; ++s)
		{
			for(unsigned r=0; r!=_rings; ++r)
			{
				indices[k++] = GLuint(offs + r + _rings);
				indices[k++] = GLuint(offs + r);
			}
			offs += GLuint(_rings);
			// primitive restart index
#ifdef GL_PRIMITIVE_RESTART
			indices[k++] = GLuint(n);
#else
			indices[k++] = GLuint(offs - 1);
			indices[k++] = GLuint(offs + _rings);
#endif
		}
		assert(k == indices.size());
		//
		// return the indices
		return indices;
	}

	/// Returns the instructions for rendering
	DrawingInstructions Instructions(Default = Default()) const
	{
		auto instructions = this->MakeInstructions();
		const std::size_t sn = _sections.size() - 1;
#ifdef GL_PRIMITIVE_RESTART
		const std::size_t n = sn * (2 * _rings + 1);
#else
		const std::size_t n = sn * (2 * _rings + 2);
#endif

		DrawOperation operation;
		operation.method = DrawOperation::Method::DrawElements;
		operation.mode = PrimitiveType::TriangleStrip;
		operation.first = GLuint(0);
		operation.count = GLuint(n);
#ifdef GL_PRIMITIVE_RESTART
		operation.restart_index = GLuint(n);
#else
		operation.restart_index = DrawOperation::NoRestartIndex();
#endif
		operation.phase = 0;

		this->AddInstruction(instructions, operation);

		return instructions;
	}
};

} // shapes
} // oglplus

#endif // include guard
