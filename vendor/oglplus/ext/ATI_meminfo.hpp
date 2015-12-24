/**
 *  @file oglplus/ext/ATI_meminfo.hpp
 *  @brief Wrapper for the ATI_meminfo extension
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_EXT_ATI_MEMINFO_1203031902_HPP
#define OGLPLUS_EXT_ATI_MEMINFO_1203031902_HPP

#include <oglplus/extension.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ATI_meminfo
/// Wrapper for the ATI_meminfo extension
/**
 *  @glsymbols
 *  @glextref{ATI,meminfo}
 *
 *  @ingroup gl_extensions
 */
class ATI_meminfo
{
public:
	/// Structure storing information about available memory
	struct AvailableMemory
	{
		// private implementation detail. Do NOT use
		GLint _v[4];

		/// Total free memory in kB
		GLint TotalFree(void) const
		{
			return _v[0];
		}

		/// Largest free memory block in kB
		GLint LargestFreeBlock(void) const
		{
			return _v[1];
		}

		/// Total free auxiliary memory in kB
		GLint TotalAuxFree(void) const
		{
			return _v[2];
		}

		/// Largest free block in auxiliary memory in kB
		GLint LargestAuxFreeBlock(void) const
		{
			return _v[3];
		}
	};

	OGLPLUS_EXTENSION_CLASS(ATI, meminfo)

	/// Returns information about free memory usable for VBOs
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{VBO_FREE_MEMORY_ATI}
	 */
	static AvailableMemory VBOFreeMemory(void)
	{
		AvailableMemory result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_VBO_FREE_MEMORY_ATI,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	/// Returns information about free memory usable for textures
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{TEXTURE_FREE_MEMORY_ATI}
	 */
	static AvailableMemory TextureFreeMemory(void)
	{
		AvailableMemory result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_TEXTURE_FREE_MEMORY_ATI,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	/// Returns information about free memory usable for renderbuffers
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{RENDERBUFFER_FREE_MEMORY_ATI}
	 */
	static AvailableMemory RenderbufferFreeMemory(void)
	{
		AvailableMemory result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_RENDERBUFFER_FREE_MEMORY_ATI,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}
};
#endif

} // namespace oglplus

#endif // include guard
