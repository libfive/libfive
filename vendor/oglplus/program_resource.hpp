/**
 *  @file oglplus/program_resource.hpp
 *  @brief OpenGL program resource wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROGRAM_RESOURCE_1208301144_HPP
#define OGLPLUS_PROGRAM_RESOURCE_1208301144_HPP

#include <oglplus/error/object.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/shader_type.hpp>
#include <oglplus/program_interface.hpp>
#include <oglplus/detail/program.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
/// Information about a single active program resource
/**
 *  @see Program
 *  @see Program::ActiveResources()
 *  @see ProgramInterface
 *
 *  @code
 *  Program prog;
 *  ...
 *  ProgramInterface intf = ProgramInterface::ProgramInput;
 *  for(auto range=prog.ActiveResources(intf); !range.Empty(); range.Next())
 *  {
 *      auto res = range.Front();
 *      std::cout << res.Name() << std::endl;
 *      std::cout << EnumValueName(res.Type()) << std::endl;
 *      if(res.IsPerPatch())
 *          std::cout << "Per-patch" << std::endl;
 *      else std::cout << "Not per-patch << std::endl;
 *  }
 *  @endcode
 */
class ProgramResource
{
private:
	GLuint _prog_name;
	GLenum _interface;
	GLuint _index;
	String _res_name;

	void QueryParams(
		GLenum property,
		GLsizei bufsize,
		GLsizei* written,
		GLint* params
	) const
	{
		OGLPLUS_GLFUNC(GetProgramResourceiv)(
			_prog_name,
			_interface,
			_index,
			1, &property,
			bufsize,
			written,
			params
		);
	}

	GLint GetParam(GLenum property) const
	{
		GLint res;
		QueryParams(property, 1, nullptr, &res);
		OGLPLUS_VERIFY(
			GetProgramResourceiv,
			Error,
			EnumParam(property)
		);
		return res;
	}

	bool HasProp(GLenum property) const
	{
		GLint res;
		QueryParams(GLenum(property), 1, nullptr, &res);
		return OGLPLUS_GLFUNC(GetError)() == GL_NO_ERROR;
	}
public:
	ProgramResource(
		aux::ProgramInterfaceContext& context,
		GLuint index
	);

	/// Gets the value of a single property (as an GLint)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 */
	GLint GetIntParam(ProgramResourceProperty property) const
	{
		return GetParam(GLenum(property));
	}

	/// Gets the value of a single property (as a boolean value)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 */
	Boolean GetBoolParam(ProgramResourceProperty property) const
	{
		return Boolean(
			GetParam(GLenum(property)),
			std::nothrow
		);
	}

	/// Checks if this resource has the specified property
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 */
	bool Has(ProgramResourceProperty property) const
	{
		return HasProp(GLenum(property));
	}

	/// Returns the interface of the resource
	ProgramInterface Interface(void) const
	{
		return ProgramInterface(_interface);
	}

	/// Returns the name of the resource
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResourceName}
	 */
	const String& Name(void) const
	{
		return _res_name;
	}

	/// Returns the index of the resource
	GLuint Index(void) const
	{
		return _index;
	}

	/// Returns true if the resource has a type
	bool HasType(void) const
	{
		return HasProp(GL_TYPE);
	}

	/// Returns the data type of the resource (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{TYPE}
	 */
	SLDataType Type(void) const
	{
		return SLDataType(GetParam(GL_TYPE));
	}

	/// Returns the program resource location (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{LOCATION}
	 */
	GLint Location(void) const
	{
		return GetParam(GL_LOCATION);
	}

	/// Returns the program resource location index (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{LOCATION_INDEX}
	 */
	GLint LocationIndex(void) const
	{
		return GetParam(GL_LOCATION_INDEX);
	}

	/// Returns the array size of the resource (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{ARRAY_SIZE}
	 */
	GLint ArraySize(void) const
	{
		return GetParam(GL_ARRAY_SIZE);
	}

	GLenum ReferencedByProperty(ShaderType type) const;

	/// Returns true if the resource is_referenced by shader (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{REFERENCED_BY_VERTEX_SHADER}
	 *  @gldefref{REFERENCED_BY_TESS_CONTROL_SHADER}
	 *  @gldefref{REFERENCED_BY_TESS_EVALUATION_SHADER}
	 *  @gldefref{REFERENCED_BY_GEOMETRY_SHADER}
	 *  @gldefref{REFERENCED_BY_FRAGMENT_SHADER}
	 *  @gldefref{REFERENCED_BY_CONTROL_SHADER}
	 */
	Boolean ReferencedBy(ShaderType shader_type) const
	{
		return Boolean(
			GetParam(ReferencedByProperty(shader_type)),
			std::nothrow
		);
	}

	/// Returns true if the resource is per-patch (if applicable)
	/**
	 *  @glsymbols
	 *  @glfunref{GetProgramResource}
	 *  @gldefref{IS_PER_PATCH}
	 */
	Boolean IsPerPatch(void) const
	{
		return Boolean(
			GetParam(GL_IS_PER_PATCH),
			std::nothrow
		);
	}

	// TODO: finish this
};

#endif // GL_VERSION_4_3

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/program_resource.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard
