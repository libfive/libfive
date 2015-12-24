/**
 *  @file oglplus/ext/ARB_compatibility.hpp
 *  @brief Wrapper for the ARB_compatibility extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_ARB_COMPATIBILITY_1203031902_HPP
#define OGLPLUS_EXT_ARB_COMPATIBILITY_1203031902_HPP

#include <oglplus/extension.hpp>
#include <oglplus/texture_unit.hpp>
#include <oglplus/ext/ARB_compatibility/primitive_type.hpp>
#include <oglplus/ext/ARB_compatibility/matrix_mode.hpp>
#include <oglplus/ext/ARB_compatibility/attrib_group.hpp>

#include <oglplus/math/matrix.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_compatibility
/// Wrapper for the ARB_compatibility extension
/**
 *  @glsymbols
 *  @glextref{ARB,compatibility}
 *
 *  @ingroup gl_extensions
 */
class ARB_compatibility
{
public:
	OGLPLUS_EXTENSION_CLASS(ARB, compatibility)

	/// Begins geometric object specification
	/**
	 *  @glsymbols
	 *  @glfunref{Begin}
	 */
	static void Begin(CompatibilityPrimitiveType mode)
	{
		OGLPLUS_GLFUNC(Begin)(GLenum(mode));
	}

	/// Ends geometric object specification
	/**
	 *  @glsymbols
	 *  @glfunref{End}
	 */
	static void End(void)
	{
		OGLPLUS_GLFUNC(End)();
		OGLPLUS_VERIFY_SIMPLE(End);
	}

	/// Pushes specified server attribute group variables on the stack
	/**
	 *  @glsymbols
	 *  @glfunref{PushAttrib}
	 */
	static void PushAttrib(Bitfield<CompatibilityAttributeGroup> attr_groups)
	{
		OGLPLUS_GLFUNC(PushAttrib)(GLbitfield(attr_groups));
		OGLPLUS_VERIFY_SIMPLE(PushAttrib);
	}

	/// Pushes specified client attribute group variables on the stack
	/**
	 *  @glsymbols
	 *  @glfunref{PushClientAttrib}
	 */
	static void PushClientAttrib(
		Bitfield<CompatibilityClientAttributeGroup> attrib_groups
	)
	{
		OGLPLUS_GLFUNC(PushClientAttrib)(GLbitfield(attrib_groups));
		OGLPLUS_VERIFY_SIMPLE(PushClientAttrib);
	}

	/// Pop previously pushed server attribute group variables from the stack
	/**
	 *  @glsymbols
	 *  @glfunref{PopAttrib}
	 */
	static void PopAttrib(void)
	{
		OGLPLUS_GLFUNC(PopAttrib)();
		OGLPLUS_VERIFY_SIMPLE(PopAttrib);
	}

	/// Pop previously pushed client attribute group variables from the stack
	/**
	 *  @glsymbols
	 *  @glfunref{PopClientAttrib}
	 */
	static void PopClientAttrib(void)
	{
		OGLPLUS_GLFUNC(PopClientAttrib)();
		OGLPLUS_VERIFY_SIMPLE(PopClientAttrib);
	}

	/// Sets the matrix mode for the subsequent commands
	/**
	 *  @glsymbols
	 *  @glfunref{MatrixMode}
	 */
	static void MatrixMode(CompatibilityMatrixMode mode)
	{
		OGLPLUS_GLFUNC(MatrixMode)(GLenum(mode));
		OGLPLUS_VERIFY_SIMPLE(MatrixMode);
	}

	/// Loads a identity matrix
	/**
	 *  @glsymbols
	 *  @glfunref{LoadIdentity}
	 */
	static void LoadIdentity(void)
	{
		OGLPLUS_GLFUNC(LoadIdentity)();
		OGLPLUS_VERIFY_SIMPLE(LoadIdentity);
	}

	/// Loads the specified @p matrix
	/**
	 *  @glsymbols
	 *  @glfunref{LoadMatrix}
	 */
	static void LoadMatrix(const Mat4f& matrix)
	{
		OGLPLUS_GLFUNC(LoadMatrixf)(Data(Transposed(matrix)));
		OGLPLUS_VERIFY_SIMPLE(LoadMatrixf);
	}

	/// Loads the specified @p matrix
	/**
	 *  @glsymbols
	 *  @glfunref{LoadMatrix}
	 */
	static void LoadMatrix(const Mat4d& matrix)
	{
		OGLPLUS_GLFUNC(LoadMatrixd)(Data(Transposed(matrix)));
		OGLPLUS_VERIFY_SIMPLE(LoadMatrixd);
	}

private:
	static void Vertex_(GLint x, GLint y)
	{
		OGLPLUS_GLFUNC(Vertex2i)(x, y);
	}

	static void Vertex_(GLint x, GLint y, GLint z)
	{
		OGLPLUS_GLFUNC(Vertex3i)(x, y, z);
	}

	static void Vertex_(GLint x, GLint y, GLint z, GLint w)
	{
		OGLPLUS_GLFUNC(Vertex4i)(x, y, z, w);
	}

	static void Vertex_(GLfloat x, GLfloat y)
	{
		OGLPLUS_GLFUNC(Vertex2f)(x, y);
	}

	static void Vertex_(GLfloat x, GLfloat y, GLfloat z)
	{
		OGLPLUS_GLFUNC(Vertex3f)(x, y, z);
	}

	static void Vertex_(GLfloat x, GLfloat y, GLfloat z, GLfloat w)
	{
		OGLPLUS_GLFUNC(Vertex4f)(x, y, z, w);
	}

	static void Vertex_(GLdouble x, GLdouble y)
	{
		OGLPLUS_GLFUNC(Vertex2d)(x, y);
	}

	static void Vertex_(GLdouble x, GLdouble y, GLdouble z)
	{
		OGLPLUS_GLFUNC(Vertex3d)(x, y, z);
	}

	static void Vertex_(GLdouble x, GLdouble y, GLdouble z, GLdouble w)
	{
		OGLPLUS_GLFUNC(Vertex4d)(x, y, z, w);
	}
public:
	/// Specifies vertex position x,y coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{Vertex}
	 */
	template <typename Type>
	static void Vertex(Type x, Type y)
	{
		Vertex_(x, y);
	}

	/// Specifies vertex position x,y,z coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{Vertex}
	 */
	template <typename Type>
	static void Vertex(Type x, Type y, Type z)
	{
		Vertex_(x, y, z);
	}

	/// Specifies vertex position x,y,z,w coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{Vertex}
	 */
	template <typename Type>
	static void Vertex(Type x, Type y, Type z, Type w)
	{
		Vertex_(x, y, z, w);
	}

private:
	static void TexCoord_(GLint s)
	{
		OGLPLUS_GLFUNC(TexCoord1i)(s);
	}

	static void TexCoord_(GLint s, GLint t)
	{
		OGLPLUS_GLFUNC(TexCoord2i)(s, t);
	}

	static void TexCoord_(GLint s, GLint t, GLint r)
	{
		OGLPLUS_GLFUNC(TexCoord3i)(s, t, r);
	}

	static void TexCoord_(GLint s, GLint t, GLint r, GLint q)
	{
		OGLPLUS_GLFUNC(TexCoord4i)(s, t, r, q);
	}

	static void TexCoord_(GLfloat s)
	{
		OGLPLUS_GLFUNC(TexCoord1f)(s);
	}

	static void TexCoord_(GLfloat s, GLfloat t)
	{
		OGLPLUS_GLFUNC(TexCoord2f)(s, t);
	}

	static void TexCoord_(GLfloat s, GLfloat t, GLfloat r)
	{
		OGLPLUS_GLFUNC(TexCoord3f)(s, t, r);
	}

	static void TexCoord_(GLfloat s, GLfloat t, GLfloat r, GLfloat q)
	{
		OGLPLUS_GLFUNC(TexCoord4f)(s, t, r, q);
	}

	static void TexCoord_(GLdouble s)
	{
		OGLPLUS_GLFUNC(TexCoord1d)(s);
	}

	static void TexCoord_(GLdouble s, GLdouble t)
	{
		OGLPLUS_GLFUNC(TexCoord2d)(s, t);
	}

	static void TexCoord_(GLdouble s, GLdouble t, GLdouble r)
	{
		OGLPLUS_GLFUNC(TexCoord3d)(s, t, r);
	}

	static void TexCoord_(GLdouble s, GLdouble t, GLdouble r, GLdouble q)
	{
		OGLPLUS_GLFUNC(TexCoord4d)(s, t, r, q);
	}
public:
	/// Specifies vertex position s coordinate
	/**
	 *  @glsymbols
	 *  @glfunref{TexCoord}
	 */
	template <typename Type>
	static void TexCoord(Type s)
	{
		TexCoord_(s);
	}

	/// Specifies vertex position s,t coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{TexCoord}
	 */
	template <typename Type>
	static void TexCoord(Type s, Type t)
	{
		TexCoord_(s, t);
	}

	/// Specifies vertex position s,t,r coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{TexCoord}
	 */
	template <typename Type>
	static void TexCoord(Type s, Type t, Type r)
	{
		TexCoord_(s, t, r);
	}

	/// Specifies vertex position s,t,r,q coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{TexCoord}
	 */
	template <typename Type>
	static void TexCoord(Type s, Type t, Type r, Type q)
	{
		TexCoord_(s, t, r, q);
	}

private:
	static void MultiTexCoord_(GLenum tex_unit, GLint s)
	{
		OGLPLUS_GLFUNC(MultiTexCoord1i)(tex_unit, s);
	}

	static void MultiTexCoord_(GLenum tex_unit, GLint s, GLint t)
	{
		OGLPLUS_GLFUNC(MultiTexCoord2i)(tex_unit, s, t);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLint s,
		GLint t,
		GLint r
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord3i)(tex_unit, s, t, r);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLint s,
		GLint t,
		GLint r,
		GLint q
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord4i)(tex_unit, s, t, r, q);
	}

	static void MultiTexCoord_(GLenum tex_unit, GLfloat s)
	{
		OGLPLUS_GLFUNC(MultiTexCoord1f)(tex_unit, s);
	}

	static void MultiTexCoord_(GLenum tex_unit, GLfloat s, GLfloat t)
	{
		OGLPLUS_GLFUNC(MultiTexCoord2f)(tex_unit, s, t);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLfloat s,
		GLfloat t,
		GLfloat r
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord3f)(tex_unit, s, t, r);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLfloat s,
		GLfloat t,
		GLfloat r,
		GLfloat q
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord4f)(tex_unit, s, t, r, q);
	}

	static void MultiTexCoord_(GLenum tex_unit, GLdouble s)
	{
		OGLPLUS_GLFUNC(MultiTexCoord1d)(tex_unit, s);
	}

	static void MultiTexCoord_(GLenum tex_unit, GLdouble s, GLdouble t)
	{
		OGLPLUS_GLFUNC(MultiTexCoord2d)(tex_unit, s, t);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLdouble s,
		GLdouble t,
		GLdouble r
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord3d)(tex_unit, s, t, r);
	}

	static void MultiTexCoord_(
		GLenum tex_unit,
		GLdouble s,
		GLdouble t,
		GLdouble r,
		GLdouble q
	)
	{
		OGLPLUS_GLFUNC(MultiTexCoord4d)(tex_unit, s, t, r, q);
	}
public:
	/// Specifies vertex position s coordinate
	/**
	 *  @glsymbols
	 *  @glfunref{MultiTexCoord}
	 */
	template <typename Type>
	static void MultiTexCoord(
		TextureUnitSelector tex_unit,
		Type s
	)
	{
		MultiTexCoord_(
			GLenum(GL_TEXTURE0 + GLuint(tex_unit)),
			s
		);
	}

	/// Specifies vertex position s,t coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{MultiTexCoord}
	 */
	template <typename Type>
	static void MultiTexCoord(
		TextureUnitSelector tex_unit,
		Type s,
		Type t
	)
	{
		MultiTexCoord_(
			GLenum(GL_TEXTURE0 + GLuint(tex_unit)),
			s, t
		);
	}

	/// Specifies vertex position s,t,r coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{MultiTexCoord}
	 */
	template <typename Type>
	static void MultiTexCoord(TextureUnitSelector tex_unit,
		Type s,
		Type t,
		Type r
	)
	{
		MultiTexCoord_(
			GLenum(GL_TEXTURE0 + GLuint(tex_unit)),
			s, t, r
		);
	}

	/// Specifies vertex position s,t,r,q coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{MultiTexCoord}
	 */
	template <typename Type>
	static void MultiTexCoord(
		TextureUnitSelector tex_unit,
		Type s,
		Type t,
		Type r,
		Type q
	)
	{
		MultiTexCoord_(
			GLenum(GL_TEXTURE0 + GLuint(tex_unit)),
			s, t, r, q
		);
	}

private:
	static void Normal_(GLint x, GLint y, GLint z)
	{
		OGLPLUS_GLFUNC(Normal3i)(x, y, z);
	}

	static void Normal_(GLfloat x, GLfloat y, GLfloat z)
	{
		OGLPLUS_GLFUNC(Normal3f)(x, y, z);
	}

	static void Normal_(GLdouble x, GLdouble y, GLdouble z)
	{
		OGLPLUS_GLFUNC(Normal3d)(x, y, z);
	}
public:
	/// Specifies vertex x,y,z normal components
	/**
	 *  @glsymbols
	 *  @glfunref{Normal}
	 */
	template <typename Type>
	static void Normal(Type x, Type y, Type z)
	{
		Normal_(x, y, z);
	}

private:
	static void FogCoord_(GLfloat c)
	{
		OGLPLUS_GLFUNC(FogCoordf)(c);
	}

	static void FogCoord_(GLdouble c)
	{
		OGLPLUS_GLFUNC(FogCoordd)(c);
	}
public:
	/// Specifies vertex coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{FogCoord}
	 */
	template <typename Type>
	static void FogCoord(Type c)
	{
		FogCoord_(c);
	}

private:
	static void Color_(GLubyte r, GLubyte g, GLubyte b)
	{
		OGLPLUS_GLFUNC(Color3ub)(r, g, b);
	}

	static void Color_(GLubyte r, GLubyte g, GLubyte b, GLubyte a)
	{
		OGLPLUS_GLFUNC(Color4ub)(r, g, b, a);
	}

	static void Color_(GLint r, GLint g, GLint b)
	{
		OGLPLUS_GLFUNC(Color3i)(r, g, b);
	}

	static void Color_(GLint r, GLint g, GLint b, GLint a)
	{
		OGLPLUS_GLFUNC(Color4i)(r, g, b, a);
	}

	static void Color_(GLfloat r, GLfloat g, GLfloat b)
	{
		OGLPLUS_GLFUNC(Color3f)(r, g, b);
	}

	static void Color_(GLfloat r, GLfloat g, GLfloat b, GLfloat a)
	{
		OGLPLUS_GLFUNC(Color4f)(r, g, b, a);
	}

	static void Color_(GLdouble r, GLdouble g, GLdouble b)
	{
		OGLPLUS_GLFUNC(Color3d)(r, g, b);
	}

	static void Color_(GLdouble r, GLdouble g, GLdouble b, GLdouble a)
	{
		OGLPLUS_GLFUNC(Color4d)(r, g, b, a);
	}
public:
	/// Specifies vertex r,g,b color components
	/**
	 *  @glsymbols
	 *  @glfunref{Color}
	 */
	template <typename Type>
	static void Color(Type r, Type g, Type b)
	{
		Color_(r, g, b);
	}

	/// Specifies vertex r,g,b,a color components
	/**
	 *  @glsymbols
	 *  @glfunref{Color}
	 */
	template <typename Type>
	static void Color(Type r, Type g, Type b, Type a)
	{
		Color_(r, g, b, a);
	}

private:
	static void SColor_(GLubyte r, GLubyte g, GLubyte b)
	{
		OGLPLUS_GLFUNC(SecondaryColor3ub)(r, g, b);
	}

	static void SColor_(GLint r, GLint g, GLint b)
	{
		OGLPLUS_GLFUNC(SecondaryColor3i)(r, g, b);
	}

	static void SColor_(GLfloat r, GLfloat g, GLfloat b)
	{
		OGLPLUS_GLFUNC(SecondaryColor3f)(r, g, b);
	}

	static void SColor_(GLdouble r, GLdouble g, GLdouble b)
	{
		OGLPLUS_GLFUNC(SecondaryColor3d)(r, g, b);
	}
public:
	/// Specifies vertex r,g,b secondary color components
	/**
	 *  @glsymbols
	 *  @glfunref{SecondaryColor}
	 */
	template <typename Type>
	static void SecondaryColor(Type r, Type g, Type b)
	{
		SColor_(r, g, b);
	}

private:
	static void Index_(GLint i)
	{
		OGLPLUS_GLFUNC(Indexi)(i);
	}

	static void Index_(GLfloat i)
	{
		OGLPLUS_GLFUNC(Indexf)(i);
	}

	static void Index_(GLdouble i)
	{
		OGLPLUS_GLFUNC(Indexd)(i);
	}
public:
	/// Specifies vertex coordinates
	/**
	 *  @glsymbols
	 *  @glfunref{Index}
	 */
	template <typename Type>
	static void Index(Type i)
	{
		Index_(i);
	}
};
#endif

} // namespace oglplus

#endif // include guard
