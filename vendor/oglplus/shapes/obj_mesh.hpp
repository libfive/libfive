/**
 *  @file oglplus/shapes/obj_mesh.hpp
 *  @brief Loader of meshes stored in .obj file format
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_OBJ_MESH_1304161247_HPP
#define OGLPLUS_SHAPES_OBJ_MESH_1304161247_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/detail/any_iter.hpp>

#include <oglplus/math/sphere.hpp>

#include <array>
#include <vector>
#include <iostream>
#include <cctype>
#include <string>

namespace oglplus {
namespace shapes {

/// Class providing attributes and instructions for drawing of mesh loaded from obj
class ObjMesh
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	struct _loading_options
	{
		bool load_normals;
		bool load_tangents;
		bool load_bitangents;
		bool load_texcoords;
		bool load_materials;

		_loading_options(bool load_all = true)
		{
			All(load_all);
		}

		_loading_options& All(bool load_all = true)
		{
			load_normals = load_all;
			load_tangents = load_all;
			load_bitangents = load_all;
			load_texcoords = load_all;
			load_materials = load_all;
			return *this;
		}

		_loading_options& Nothing(void)
		{
			return All(false);
		}

		_loading_options& Normals(bool load = true)
		{
			load_normals = load;
			return *this;
		}

		_loading_options& Tangents(bool load = true)
		{
			load_tangents = load;
			return *this;
		}

		_loading_options& Bitangents(bool load = true)
		{
			load_bitangents = load;
			return *this;
		}

		_loading_options& TexCoords(bool load = true)
		{
			load_texcoords = load;
			return *this;
		}

		_loading_options& Materials(bool load = true)
		{
			load_materials = load;
			return *this;
		}
	};

	// vertex positions
	std::vector<double> _pos_data;
	// vertex normals
	std::vector<double> _nml_data;
	// vertex tangents
	std::vector<double> _tgt_data;
	// vertex bitangents
	std::vector<double> _btg_data;
	// vertex tex coords
	std::vector<double> _tex_data;
	// material numbers
	std::vector<GLuint> _mtl_data;
	// material names
	std::vector<std::string> _mtl_names;

	struct _vert_indices
	{
		GLuint _pos;
		GLuint _nml;
		GLuint _tex;
		GLuint _mtl;

		_vert_indices(void)
		 : _pos(0)
		 , _nml(0)
		 , _tex(0)
		 , _mtl(0)
		{ }
	};

	// the vertex offsets and counts for individual meshes
	std::vector<std::string> _mesh_names;
	std::vector<GLuint> _mesh_offsets;
	std::vector<GLuint> _mesh_counts;

	bool _load_index(
		GLuint& value,
		GLuint count,
		std::string::const_iterator& i,
		std::string::const_iterator& e
	);

	bool _load_indices(
		_vert_indices& indices,
		const _vert_indices& counts,
		std::string::const_iterator& i,
		std::string::const_iterator& e
	);

	void _load_meshes(
		const _loading_options& opts,
		aux::AnyInputIter<const char*> names_begin,
		aux::AnyInputIter<const char*> names_end,
		std::istream& input
	);

	void _call_load_meshes(
		std::istream& input,
		aux::AnyInputIter<const char*> names_begin,
		aux::AnyInputIter<const char*> names_end,
		_loading_options opts
	);
public:
	typedef _loading_options LoadingOptions;

	ObjMesh(
		std::istream& input,
		LoadingOptions opts = LoadingOptions()
	)
	{
		const char** p = nullptr;
		_call_load_meshes(input, p, p, opts);
	}

	template <typename NameStr, std::size_t NN>
	ObjMesh(
		std::istream& input,
		const std::array<NameStr, NN>& names,
		LoadingOptions opts = LoadingOptions()
	)
	{
		_call_load_meshes(
			input,
			names.begin(),
			names.end(),
			opts
		);
	}

	/// Returns the winding direction of faces
	FaceOrientation FaceWinding(void) const
	{
		return FaceOrientation::CCW;
	}

	typedef GLuint (ObjMesh::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	/// Makes the vertex positions and returns the number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _pos_data.begin(), _pos_data.end());
		return 3;
	}

	/// Makes the vertex normals and returns the number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _nml_data.begin(), _nml_data.end());
		return 3;
	}

	/// Makes the vertex tangents and returns the number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _tgt_data.begin(), _tgt_data.end());
		return 3;
	}

	/// Makes the vertex bi-tangents and returns the number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _btg_data.begin(), _btg_data.end());
		return 3;
	}

	/// Makes the texture coordinates returns the number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _tex_data.begin(), _tex_data.end());
		return 3;
	}

	/// Makes the material numbers returns the number of values per vertex
	template <typename T>
	GLuint MaterialNumbers(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.begin(), _mtl_data.begin(), _mtl_data.end());
		return 1;
	}

	/// Returns the name of the i-th material
	const std::string& MaterialName(GLuint mat_num) const
	{
		return _mtl_names[mat_num];
	}

	/// Queries the index of the mesh with the specified name
	bool QueryMeshIndex(const std::string& name, GLuint& index) const;

	/// Gets the index of the mesh with the specified name, throws on error
	GLuint GetMeshIndex(const std::string& name) const;

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** ObjMesh provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions
	 *  - "Normal" the vertex normals
	 *  - "Tangent" the vertex tangents
	 *  - "TexCoords" the vertex texture coordinates
	 *  - "Material" the vertex material numbers
	 */
	typedef VertexAttribsInfo<ObjMesh> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		ObjMesh,
		std::tuple<
			VertexPositionsTag,
			VertexNormalsTag,
			VertexTangentsTag,
			VertexBitangentsTag,
			VertexTexCoordinatesTag,
			VertexMaterialNumbersTag
		>
	> VertexAttribs;
#endif

	Spheref MakeBoundingSphere(void) const;

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		bounding_sphere = oglplus::Sphere<T>(MakeBoundingSphere());
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return IndexArray();
	}

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(PrimitiveType primitive) const;

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const
	{
		return Instructions(PrimitiveType::Triangles);
	}
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/obj_mesh.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
