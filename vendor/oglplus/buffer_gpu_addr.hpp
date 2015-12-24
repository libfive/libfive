/**
 *  @file oglplus/buffer_gpu_addr.hpp
 *  @brief Object representing Buffer's GPU address.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BUFFER_GPU_ADDR_1310102147_HPP
#define OGLPLUS_BUFFER_GPU_ADDR_1310102147_HPP

#if GL_NV_shader_buffer_load

#include <oglplus/fwd.hpp>
#include <oglplus/prog_var/type_ops.hpp>

namespace oglplus {

class BufferGPUAddress;
GLuint64EXT GetGLAddress(BufferGPUAddress);

/// Class encapsulating buffer object GPU address
class BufferGPUAddress
{
private:
	friend GLuint64EXT GetGLAddress(BufferGPUAddress);

	GLuint64EXT _addr;
public:
	BufferGPUAddress(GLuint64EXT addr)
	 : _addr(addr)
	{ }
};

inline GLuint64EXT GetGLAddress(BufferGPUAddress bga)
{
	return bga._addr;
}

template <>
struct AdjustProgVar<BufferGPUAddress>
{
	typedef GLuint64 BaseType;
	typedef BufferGPUAddress ValueType;

	inline static BaseType Adjust(ValueType value)
	{
		return GetGLAddress(value);
	}
};

} // namespace oglplus

#endif // GL_NV_shader_buffer_load

#endif // include guard
