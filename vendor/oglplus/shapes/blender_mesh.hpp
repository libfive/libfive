/**
 *  @file oglplus/shapes/blender_mesh.hpp
 *  @brief Blender mesh loader
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHAPES_BLENDER_MESH_1206011111_HPP
#define OGLPLUS_SHAPES_BLENDER_MESH_1206011111_HPP

#include <oglplus/face_mode.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/matrix.hpp>
#include <oglplus/math/sphere.hpp>

#include <oglplus/shapes/draw.hpp>

#include <oglplus/shapes/vert_attr_info.hpp>

#include <oglplus/imports/blend_file.hpp>

#include <oglplus/detail/any_iter.hpp>

#include <vector>
#include <array>
#include <stdexcept>
#include <cassert>

namespace oglplus {
namespace shapes {

/// Class providing vertex attributes and instructions for drawing of a mesh
class BlenderMesh
 : public DrawingInstructionWriter
 , public DrawMode
{
private:
	struct _loading_options
	{
		const char* scene_name;
		bool load_normals;
		bool load_tangents;
		bool load_bitangents;
		bool load_texcoords;
		bool load_materials;

		_loading_options(bool load_all = true)
		 : scene_name(nullptr)
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
	std::vector<GLfloat> _pos_data;
	// vertex normals
	std::vector<GLfloat> _nml_data;
	// vertex tangentials
	std::vector<GLfloat> _tgt_data;
	// vertex bitangentials
	std::vector<GLfloat> _btg_data;
	// vertex tex coords
	std::vector<GLfloat> _uvc_data;
	// material numbers
	std::vector<GLshort> _mtl_data;

	// vectors with vertex indices
	std::vector<GLuint> _idx_data;

	// the index offsets and counts of individual meshes
	std::vector<GLuint> _mesh_offsets;
	std::vector<GLuint> _mesh_n_elems;

	// find the scene by name or the default scene
	imports::BlendFileFlatStructBlockData _find_scene(
		const _loading_options& /*opts*/,
		imports::BlendFile& blend_file,
		imports::BlendFileStructGlobBlock& glob_block
	)
	{
		// TODO: find the scene by name
		return  blend_file[glob_block.curscene];
	}

	// load a single mesh from a scene
	void _load_mesh(
		const _loading_options& opts,
		imports::BlendFile& blend_file,
		imports::BlendFileFlatStructBlockData& object_mesh_data,
		const Mat4f& mesh_matrix,
		GLuint& index_offset
	);

	// load a single object from a scene
	void _load_object(
		const _loading_options& opts,
		aux::AnyInputIter<const char*> names_begin,
		aux::AnyInputIter<const char*> names_end,
		imports::BlendFile& blend_file,
		imports::BlendFileFlatStructBlockData& object_data,
		imports::BlendFilePointer object_data_ptr,
		GLuint& index_offset
	);

	void _load_meshes(
		const _loading_options& opts,
		aux::AnyInputIter<const char*> names_begin,
		aux::AnyInputIter<const char*> names_end,
		imports::BlendFile& blend_file
	);

	void _call_load_meshes(
		imports::BlendFile& blend_file,
		const char* scene_name,
		aux::AnyInputIter<const char*> names_begin,
		aux::AnyInputIter<const char*> names_end,
		_loading_options opts
	);
public:
	typedef _loading_options LoadingOptions;

	BlenderMesh(imports::BlendFile& blend_file)
	{
		_call_load_meshes(
			blend_file,
			nullptr,
			static_cast<const char**>(nullptr),
			static_cast<const char**>(nullptr),
			LoadingOptions()
		);
	}

	template <typename NameStr, std::size_t NN>
	BlenderMesh(
		imports::BlendFile& blend_file,
		const std::array<NameStr, NN>& names,
		LoadingOptions opts = LoadingOptions()
	)
	{
		_call_load_meshes(
			blend_file,
			nullptr,
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

	typedef GLuint (BlenderMesh::*VertexAttribFunc)(std::vector<GLfloat>&) const;

	/// Makes the vertex positions and returns the number of values per vertex
	template <typename T>
	GLuint Positions(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _pos_data.begin(), _pos_data.end());
		return 3;
	}

	/// Makes the vertex normals and returns the number of values per vertex
	template <typename T>
	GLuint Normals(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _nml_data.begin(), _nml_data.end());
		return 3;
	}

	/// Makes the vertex tangents and returns the number of values per vertex
	template <typename T>
	GLuint Tangents(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _tgt_data.begin(), _tgt_data.end());
		return 3;
	}

	/// Makes the vertex bitangents and returns the number of values per vertex
	template <typename T>
	GLuint Bitangents(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _btg_data.begin(), _btg_data.end());
		return 3;
	}

	/// Makes the texture coordinates and returns the number of values per vertex
	template <typename T>
	GLuint TexCoordinates(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _uvc_data.begin(), _uvc_data.end());
		return 2;
	}

	template <typename T>
	GLuint MaterialNumbers(std::vector<T>& dest) const
	{
		dest.clear();
		dest.insert(dest.end(), _mtl_data.begin(), _mtl_data.end());
		return 1;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Vertex attribute information for this shape builder
	/** BlenderMesh provides build functions for the following named
	 *  vertex attributes:
	 *  - "Position" the vertex positions (Positions)
	 *  - "Normal" the vertex normals (Normals)
	 */
	typedef VertexAttribsInfo<BlenderMesh> VertexAttribs;
#else
	typedef VertexAttribsInfo<
		BlenderMesh,
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

	Spheref GetBoundingSphere(void) const;

	/// Queries the bounding sphere coordinates and dimensions
	template <typename T>
	void BoundingSphere(oglplus::Sphere<T>& bounding_sphere) const
	{
		bounding_sphere = oglplus::Sphere<T>(GetBoundingSphere());
	}

	/// The type of the index container returned by Indices()
	typedef std::vector<GLuint> IndexArray;

	/// Returns element indices that are used with the drawing instructions
	IndexArray Indices(Default = Default()) const
	{
		return _idx_data;
	}

	/// Returns the instructions for rendering of faces
	DrawingInstructions Instructions(Default = Default()) const;
};

} // shapes
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shapes/blender_mesh.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
