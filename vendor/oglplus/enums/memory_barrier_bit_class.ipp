//  File implement/oglplus/enums/memory_barrier_bit_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/memory_barrier_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<MemoryBarrierBit> class Transform>
class EnumToClass<Base, MemoryBarrierBit, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
# if defined VertexAttribArray
#  pragma push_macro("VertexAttribArray")
#  undef VertexAttribArray
	Transform<MemoryBarrierBit::VertexAttribArray> VertexAttribArray;
#  pragma pop_macro("VertexAttribArray")
# else
	Transform<MemoryBarrierBit::VertexAttribArray> VertexAttribArray;
# endif
#endif
#if defined GL_ELEMENT_ARRAY_BARRIER_BIT
# if defined ElementArray
#  pragma push_macro("ElementArray")
#  undef ElementArray
	Transform<MemoryBarrierBit::ElementArray> ElementArray;
#  pragma pop_macro("ElementArray")
# else
	Transform<MemoryBarrierBit::ElementArray> ElementArray;
# endif
#endif
#if defined GL_UNIFORM_BARRIER_BIT
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	Transform<MemoryBarrierBit::Uniform> Uniform;
#  pragma pop_macro("Uniform")
# else
	Transform<MemoryBarrierBit::Uniform> Uniform;
# endif
#endif
#if defined GL_TEXTURE_FETCH_BARRIER_BIT
# if defined TextureFetch
#  pragma push_macro("TextureFetch")
#  undef TextureFetch
	Transform<MemoryBarrierBit::TextureFetch> TextureFetch;
#  pragma pop_macro("TextureFetch")
# else
	Transform<MemoryBarrierBit::TextureFetch> TextureFetch;
# endif
#endif
#if defined GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
# if defined ShaderImageAccess
#  pragma push_macro("ShaderImageAccess")
#  undef ShaderImageAccess
	Transform<MemoryBarrierBit::ShaderImageAccess> ShaderImageAccess;
#  pragma pop_macro("ShaderImageAccess")
# else
	Transform<MemoryBarrierBit::ShaderImageAccess> ShaderImageAccess;
# endif
#endif
#if defined GL_COMMAND_BARRIER_BIT
# if defined Command
#  pragma push_macro("Command")
#  undef Command
	Transform<MemoryBarrierBit::Command> Command;
#  pragma pop_macro("Command")
# else
	Transform<MemoryBarrierBit::Command> Command;
# endif
#endif
#if defined GL_PIXEL_BUFFER_BARRIER_BIT
# if defined PixelBuffer
#  pragma push_macro("PixelBuffer")
#  undef PixelBuffer
	Transform<MemoryBarrierBit::PixelBuffer> PixelBuffer;
#  pragma pop_macro("PixelBuffer")
# else
	Transform<MemoryBarrierBit::PixelBuffer> PixelBuffer;
# endif
#endif
#if defined GL_TEXTURE_UPDATE_BARRIER_BIT
# if defined TextureUpdate
#  pragma push_macro("TextureUpdate")
#  undef TextureUpdate
	Transform<MemoryBarrierBit::TextureUpdate> TextureUpdate;
#  pragma pop_macro("TextureUpdate")
# else
	Transform<MemoryBarrierBit::TextureUpdate> TextureUpdate;
# endif
#endif
#if defined GL_BUFFER_UPDATE_BARRIER_BIT
# if defined BufferUpdate
#  pragma push_macro("BufferUpdate")
#  undef BufferUpdate
	Transform<MemoryBarrierBit::BufferUpdate> BufferUpdate;
#  pragma pop_macro("BufferUpdate")
# else
	Transform<MemoryBarrierBit::BufferUpdate> BufferUpdate;
# endif
#endif
#if defined GL_FRAMEBUFFER_BARRIER_BIT
# if defined Framebuffer
#  pragma push_macro("Framebuffer")
#  undef Framebuffer
	Transform<MemoryBarrierBit::Framebuffer> Framebuffer;
#  pragma pop_macro("Framebuffer")
# else
	Transform<MemoryBarrierBit::Framebuffer> Framebuffer;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BARRIER_BIT
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	Transform<MemoryBarrierBit::TransformFeedback> TransformFeedback;
#  pragma pop_macro("TransformFeedback")
# else
	Transform<MemoryBarrierBit::TransformFeedback> TransformFeedback;
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BARRIER_BIT
# if defined AtomicCounter
#  pragma push_macro("AtomicCounter")
#  undef AtomicCounter
	Transform<MemoryBarrierBit::AtomicCounter> AtomicCounter;
#  pragma pop_macro("AtomicCounter")
# else
	Transform<MemoryBarrierBit::AtomicCounter> AtomicCounter;
# endif
#endif
#if defined GL_SHADER_STORAGE_BARRIER_BIT
# if defined ShaderStorage
#  pragma push_macro("ShaderStorage")
#  undef ShaderStorage
	Transform<MemoryBarrierBit::ShaderStorage> ShaderStorage;
#  pragma pop_macro("ShaderStorage")
# else
	Transform<MemoryBarrierBit::ShaderStorage> ShaderStorage;
# endif
#endif
#if defined GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
# if defined ClientMappedBuffer
#  pragma push_macro("ClientMappedBuffer")
#  undef ClientMappedBuffer
	Transform<MemoryBarrierBit::ClientMappedBuffer> ClientMappedBuffer;
#  pragma pop_macro("ClientMappedBuffer")
# else
	Transform<MemoryBarrierBit::ClientMappedBuffer> ClientMappedBuffer;
# endif
#endif
#if defined GL_ALL_BARRIER_BITS
# if defined All
#  pragma push_macro("All")
#  undef All
	Transform<MemoryBarrierBit::All> All;
#  pragma pop_macro("All")
# else
	Transform<MemoryBarrierBit::All> All;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
# if defined VertexAttribArray
#  pragma push_macro("VertexAttribArray")
#  undef VertexAttribArray
	 , VertexAttribArray(_base())
#  pragma pop_macro("VertexAttribArray")
# else
	 , VertexAttribArray(_base())
# endif
#endif
#if defined GL_ELEMENT_ARRAY_BARRIER_BIT
# if defined ElementArray
#  pragma push_macro("ElementArray")
#  undef ElementArray
	 , ElementArray(_base())
#  pragma pop_macro("ElementArray")
# else
	 , ElementArray(_base())
# endif
#endif
#if defined GL_UNIFORM_BARRIER_BIT
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	 , Uniform(_base())
#  pragma pop_macro("Uniform")
# else
	 , Uniform(_base())
# endif
#endif
#if defined GL_TEXTURE_FETCH_BARRIER_BIT
# if defined TextureFetch
#  pragma push_macro("TextureFetch")
#  undef TextureFetch
	 , TextureFetch(_base())
#  pragma pop_macro("TextureFetch")
# else
	 , TextureFetch(_base())
# endif
#endif
#if defined GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
# if defined ShaderImageAccess
#  pragma push_macro("ShaderImageAccess")
#  undef ShaderImageAccess
	 , ShaderImageAccess(_base())
#  pragma pop_macro("ShaderImageAccess")
# else
	 , ShaderImageAccess(_base())
# endif
#endif
#if defined GL_COMMAND_BARRIER_BIT
# if defined Command
#  pragma push_macro("Command")
#  undef Command
	 , Command(_base())
#  pragma pop_macro("Command")
# else
	 , Command(_base())
# endif
#endif
#if defined GL_PIXEL_BUFFER_BARRIER_BIT
# if defined PixelBuffer
#  pragma push_macro("PixelBuffer")
#  undef PixelBuffer
	 , PixelBuffer(_base())
#  pragma pop_macro("PixelBuffer")
# else
	 , PixelBuffer(_base())
# endif
#endif
#if defined GL_TEXTURE_UPDATE_BARRIER_BIT
# if defined TextureUpdate
#  pragma push_macro("TextureUpdate")
#  undef TextureUpdate
	 , TextureUpdate(_base())
#  pragma pop_macro("TextureUpdate")
# else
	 , TextureUpdate(_base())
# endif
#endif
#if defined GL_BUFFER_UPDATE_BARRIER_BIT
# if defined BufferUpdate
#  pragma push_macro("BufferUpdate")
#  undef BufferUpdate
	 , BufferUpdate(_base())
#  pragma pop_macro("BufferUpdate")
# else
	 , BufferUpdate(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_BARRIER_BIT
# if defined Framebuffer
#  pragma push_macro("Framebuffer")
#  undef Framebuffer
	 , Framebuffer(_base())
#  pragma pop_macro("Framebuffer")
# else
	 , Framebuffer(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BARRIER_BIT
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	 , TransformFeedback(_base())
#  pragma pop_macro("TransformFeedback")
# else
	 , TransformFeedback(_base())
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BARRIER_BIT
# if defined AtomicCounter
#  pragma push_macro("AtomicCounter")
#  undef AtomicCounter
	 , AtomicCounter(_base())
#  pragma pop_macro("AtomicCounter")
# else
	 , AtomicCounter(_base())
# endif
#endif
#if defined GL_SHADER_STORAGE_BARRIER_BIT
# if defined ShaderStorage
#  pragma push_macro("ShaderStorage")
#  undef ShaderStorage
	 , ShaderStorage(_base())
#  pragma pop_macro("ShaderStorage")
# else
	 , ShaderStorage(_base())
# endif
#endif
#if defined GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
# if defined ClientMappedBuffer
#  pragma push_macro("ClientMappedBuffer")
#  undef ClientMappedBuffer
	 , ClientMappedBuffer(_base())
#  pragma pop_macro("ClientMappedBuffer")
# else
	 , ClientMappedBuffer(_base())
# endif
#endif
#if defined GL_ALL_BARRIER_BITS
# if defined All
#  pragma push_macro("All")
#  undef All
	 , All(_base())
#  pragma pop_macro("All")
# else
	 , All(_base())
# endif
#endif
	{ }
};

} // namespace enums

