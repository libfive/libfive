/**
 *  @file oglplus/shapes/obj_mesh.ipp
 *  @brief Implementation of shapes::ObjMesh
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <cassert>

namespace oglplus {
namespace shapes {

OGLPLUS_LIB_FUNC
bool ObjMesh::_load_index(
	GLuint& value,
	GLuint n_verts,
	std::string::const_iterator& i,
	std::string::const_iterator& e
)
{
	bool neg = false;
	if((i != e) && (*i == '-'))
	{
		neg = true;
		++i;
		while((i != e) && (std::isspace(*i))) ++i;
	}
	if((i != e) && (*i >= '0') && (*i <= '9'))
	{
		value = 0;
		while((i != e) && (*i >= '0') && (*i <= '9'))
		{
			value *= 10;
			value += GLuint(*i-'0');
			++i;
		}
		if(neg)
		{
			assert(n_verts > value);
			value = n_verts - value;
		}
		return true;
	}
	return false;
}

OGLPLUS_LIB_FUNC
bool ObjMesh::_load_indices(
	_vert_indices& indices,
	const _vert_indices& counts,
	std::string::const_iterator& i,
	std::string::const_iterator& e
)
{
	indices = _vert_indices();

	while((i != e) && (std::isspace(*i))) ++i;
	if(_load_index(indices._pos, counts._pos, i, e))
	{
		if(i == e) return true;
		if(std::isspace(*i)) return true;
		if(*i == '/')
		{
			++i;
			if(i == e) return false;
			if(*i != '/')
			{
				if(!_load_index(indices._tex,counts._tex, i, e))
				{
					return false;
				}
			}
			if(*i == '/')
			{
				++i;
				if(i == e) return false;
				if(std::isspace(*i)) return false;
				if(!_load_index(indices._nml,counts._nml, i, e))
				{
					return false;
				}
			}
			return (i == e) || std::isspace(*i);
		}
	}
	return false;
}

OGLPLUS_LIB_FUNC
void ObjMesh::_load_meshes(
	const _loading_options& opts, //TODO
	aux::AnyInputIter<const char*> names_begin,
	aux::AnyInputIter<const char*> names_end,
	std::istream& input
)
{
	if(!input.good())
	{
		throw std::runtime_error("Obj file loader: Unable to read input.");
	}

	const double unused[3] = {0.0, 0.0, 0.0};
	// unused position
	std::vector<double> pos_data(unused, unused+3);
	// unused normal
	std::vector<double> nml_data(unused, unused+3);
	// unused tex. coord.
	std::vector<double> tex_data(unused, unused+3);
	// unused material
	std::vector<double> mtl_data(1, 0);
	// vertex attrib tuple counts
	_vert_indices n_attr;
	n_attr._pos = 1;
	n_attr._nml = 1;
	n_attr._tex = 1;
	n_attr._mtl = 1;
	// unused index
	std::vector<_vert_indices> idx_data(1, _vert_indices());
	_mtl_names.push_back(std::string());
	// the current count of vertices

	std::vector<std::string> mesh_names;
	std::vector<GLuint> mesh_offsets;
	std::vector<GLuint> mesh_counts;

	GLuint curr_mtl = 0;
	std::string mtllib;

	const std::string vert_tags(" tnp");
 	std::string line;
	while(std::getline(input, line))
	{
		std::string::const_iterator b = line.begin(), i = b, e = line.end();
		// rtrim \r
		while((b < e) && e[-1] == '\r') --e;
		// ltrim
		while((i != e) && std::isspace(*i)) ++i;
		// skip empty lines
		if(i == e) continue;
		// skip comments
		if(*i == '#') continue;
		//
		// if it is a material library statement
		if(*i == 'm')
		{
			const char* s = "mtllib";
			if(std::find_end(i, e, s, s+6) != i)
			{
				throw std::runtime_error(
					"Obj file loader: Unknown tag at line: "+
					line
				);
			}
			i += 6;
			while((i != e) && std::isspace(*i)) ++i;
			std::string::const_iterator f = i;
			while((f != e) && !std::isspace(*f)) ++f;
			mtllib = std::string(i, f);
		}
		// if it is a use material statement
		else if(*i == 'u')
		{
			const char* s = "usemtl";
			if(std::find_end(i, e, s, s+6) != i)
			{
				throw std::runtime_error(
					"Obj file loader: Unknown tag at line: "+
					line
				);
			}
			i += 6;
			while((i != e) && std::isspace(*i)) ++i;
			std::string::const_iterator f = i;
			while((f != e) && !std::isspace(*f)) ++f;

			std::string material;
			if(!mtllib.empty()) material = mtllib + '#';
			material.append(std::string(i, f));

			curr_mtl = GLuint(_mtl_names.size());
			_mtl_names.push_back(material);
		}
		// if the line contains vertex data
		else if(*i == 'v')
		{
			++i;
			if(i == e)
			{
				throw std::runtime_error(
					"Obj file loader: Unexpected end of line: "+
					line
				);
			}
			char t = *i;
			++i;
			std::stringstream str(line.c_str()+std::distance(b, i));
			// if it is a known tag
			if(vert_tags.find(t) != std::string::npos)
			{
				double v[3] = {0.0, 0.0, 0.0};
				str >> v[0];
				str >> v[1];
				str >> v[2];
				if(t == ' ')
				{
					pos_data.insert(pos_data.end(), v, v+3);
					++n_attr._pos;
				}
				if(t == 'n')
				{
					nml_data.insert(nml_data.end(), v, v+3);
					++n_attr._nml;
				}
				if(t == 't')
				{
					tex_data.insert(tex_data.end(), v, v+3);
					++n_attr._tex;
				}
			}
		}
		else if(*i == 'f')
		{
			++i;
			while((i != e) && std::isspace(*i)) ++i;
			_vert_indices vi1[3];
			for(std::size_t n=0; n!=3; ++n)
			{
				if(!_load_indices(vi1[n], n_attr, i, e))
				{
					throw std::runtime_error(
						"Obj file loader: Error reading indices: "+
						line
					);
				}
				vi1[n]._mtl = curr_mtl;
			}
			idx_data.insert(idx_data.end(), vi1, vi1+3);
			_vert_indices vi2[3] = {vi1[0], vi1[2], _vert_indices()};
			while(_load_indices(vi2[2], n_attr, i, e))
			{
				vi2[2]._mtl = curr_mtl;
				idx_data.insert(idx_data.end(), vi2, vi2+3);
				vi2[1] = vi2[2];
			}
		}
		else if(*i == 'o')
		{
			++i;
			while((i != e) && std::isspace(*i)) ++i;
			if(!mesh_offsets.empty())
			{
				mesh_counts.push_back(
					GLuint(idx_data.size())-
					mesh_offsets.back()
				);
			}
			mesh_names.push_back(std::string(i, e));
			mesh_offsets.push_back(GLuint(idx_data.size()));
		}
	}
	// the last mesh element count
	if(mesh_offsets.empty())
	{
		if(!idx_data.empty())
		{
			mesh_offsets.push_back(1);
			mesh_counts.push_back(GLuint(idx_data.size()-1));
		}
	}
	else
	{
		mesh_counts.push_back(GLuint(idx_data.size()-mesh_offsets.back()));
	}

	if(mesh_names.empty())
		mesh_names.push_back(std::string());
	assert(mesh_names.size() == mesh_offsets.size());
	assert(mesh_names.size() == mesh_counts.size());

	std::size_t ni = idx_data.size()-1;
	std::size_t mo = 0;

	_pos_data.resize(ni*3);
	_nml_data.resize(ni*3);
	_tex_data.resize(ni*3);
	_mtl_data.resize(ni*1);

	std::vector<std::size_t> meshes_to_load;

	if(names_begin == names_end)
	{
		meshes_to_load.resize(mesh_names.size());
		_mesh_names.resize(mesh_names.size());
		for(std::size_t m = 0; m!=mesh_names.size(); ++m)
		{
			meshes_to_load[m] = m;
			_mesh_names[m] = std::move(mesh_names[m]);
		}
	}
	else
	{
		while(names_begin != names_end)
		{
			for(std::size_t m=0; m!=mesh_names.size(); ++m)
			{
				if(*names_begin == mesh_names[m])
				{
					meshes_to_load.push_back(m);
					_mesh_names.push_back(mesh_names[m]);
					break;
				}
			}
			++names_begin;
		}
	}

	for(std::size_t l = 0; l!=meshes_to_load.size(); ++l)
	{
		std::size_t m = meshes_to_load[l];
		std::size_t ii = mesh_offsets[m];
		std::size_t mc = mesh_counts[m];
		ni = ii + mc;
		while(ii != ni)
		{
			for(std::size_t c=0; c!=3; ++c)
			{
				std::size_t oi = (ii-1)*3+c;
				_pos_data[oi] = pos_data[idx_data[ii]._pos*3+c];
				_nml_data[oi] = nml_data[idx_data[ii]._nml*3+c];
				_tex_data[oi] = tex_data[idx_data[ii]._tex*3+c];
			}
			_mtl_data[ii-1] = idx_data[ii]._mtl;
			++ii;
		}
		_mesh_offsets.push_back(GLuint(mo));
		_mesh_counts.push_back(GLuint(mc));
		mo += mc;
	}

	assert(_pos_data.size() % 9 == 0);
	assert(_pos_data.size() == _tex_data.size());

	if(opts.load_tangents)
	{
		if(opts.load_tangents)
		{
			_tgt_data.resize(_pos_data.size());
		}
		if(opts.load_bitangents)
		{
			_btg_data.resize(_pos_data.size());
		}

		for(std::size_t f=0, nf = _pos_data.size()/9; f != nf; ++f)
		{
			for(std::size_t v=0; v!=3; ++v)
			{
				std::size_t j[3] = {
					v,
					(v+1)%3,
					(v+2)%3
				};

				Vec3d p[3];
				Vec2d uv[3];
				for(std::size_t k=0; k<3; ++k)
				{
					p[k] = Vec3d(
						_pos_data[f*9+j[k]*3+0],
						_pos_data[f*9+j[k]*3+1],
						_pos_data[f*9+j[k]*3+2]
					);
					uv[k] = Vec2d(
						_tex_data[f*9+j[k]*3+0],
						_tex_data[f*9+j[k]*3+1]
					);
				}

				Vec3d v0 = p[1] - p[0];
				Vec3d v1 = p[2] - p[0];

				Vec2d duv0 = uv[1] - uv[0];
				Vec2d duv1 = uv[2] - uv[0];

				double d = duv0.x()*duv1.y()-duv0.y()*duv1.x();
				if(d != 0.0f) d = 1.0f/d;

				if(opts.load_tangents)
				{
					Vec3f t = (duv1.y()*v0 - duv0.y()*v1)*d;
					Vec3f nt = Normalized(t);

					for(std::size_t tv=0; tv!=3; ++tv)
					{
						_tgt_data[f*9+v*3+0] = nt.x();
						_tgt_data[f*9+v*3+1] = nt.y();
						_tgt_data[f*9+v*3+2] = nt.z();
					}
				}

				if(opts.load_bitangents)
				{
					Vec3f b = (duv0.x()*v1 - duv1.x()*v0)*d;
					Vec3f nb = Normalized(b);

					for(std::size_t tv=0; tv!=3; ++tv)
					{
						_btg_data[f*9+v*3+0] = nb.x();
						_btg_data[f*9+v*3+1] = nb.y();
						_btg_data[f*9+v*3+2] = nb.z();
					}
				}
			}
		}
	}
}

OGLPLUS_LIB_FUNC
void ObjMesh::_call_load_meshes(
	std::istream& input,
	aux::AnyInputIter<const char*> names_begin,
	aux::AnyInputIter<const char*> names_end,
	_loading_options opts
)
{
	opts.load_tangents |= opts.load_bitangents;
	opts.load_bitangents |= opts.load_tangents;
	opts.load_texcoords |= opts.load_tangents;

	_load_meshes(opts, names_begin, names_end, input);
}

OGLPLUS_LIB_FUNC
bool ObjMesh::QueryMeshIndex(const std::string& name, GLuint& index) const
{
	auto p = std::find(_mesh_names.begin(), _mesh_names.end(), name);
	if(p == _mesh_names.end()) return false;
	index = GLuint(std::distance(_mesh_names.begin(), p));
	return true;
}

OGLPLUS_LIB_FUNC
GLuint ObjMesh::GetMeshIndex(const std::string& name) const
{
	GLuint result = 0;
	if(!QueryMeshIndex(name, result))
	{
		throw std::runtime_error(
			"ObjMesh: Unable to find index of mesh '"+
			name +
			"'"
		);
	}
	return result;
}

OGLPLUS_LIB_FUNC
Spheref ObjMesh::MakeBoundingSphere(void) const
{
	GLdouble min_x = _pos_data[3], max_x = _pos_data[3];
	GLdouble min_y = _pos_data[4], max_y = _pos_data[4];
	GLdouble min_z = _pos_data[5], max_z = _pos_data[5];
	for(std::size_t v=0, vn=_pos_data.size()/3; v!=vn; ++v)
	{
		GLdouble x = _pos_data[v*3+0];
		GLdouble y = _pos_data[v*3+1];
		GLdouble z = _pos_data[v*3+2];

		if(min_x > x) min_x = x;
		if(min_y > y) min_y = y;
		if(min_z > z) min_z = z;
		if(max_x < x) max_x = x;
		if(max_y < y) max_y = y;
		if(max_z < z) max_z = z;
	}

	Vec3d c(
		(min_x + max_x) * 0.5,
		(min_y + max_y) * 0.5,
		(min_z + max_z) * 0.5
	);

	return Spheref(
		GLfloat(c.x()),
		GLfloat(c.y()),
		GLfloat(c.z()),
		GLfloat(Distance(c, Vec3d(min_x, min_y, min_z)))
	);
}

OGLPLUS_LIB_FUNC
DrawingInstructions ObjMesh::Instructions(PrimitiveType primitive) const
{
	DrawingInstructions instr = this->MakeInstructions();
	for(std::size_t m=0; m!=_mesh_offsets.size(); ++m)
	{
		DrawOperation operation;
		operation.method = DrawOperation::Method::DrawArrays;
		operation.mode = primitive;
		operation.first = _mesh_offsets[m];
		operation.count = _mesh_counts[m];
		operation.restart_index = DrawOperation::NoRestartIndex();
		operation.phase = GLuint(m);
		this->AddInstruction(instr, operation);
	}
	return std::move(instr);
}

} // shapes
} // oglplus

