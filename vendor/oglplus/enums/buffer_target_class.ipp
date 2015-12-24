//  File implement/oglplus/enums/buffer_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BufferTarget> class Transform>
class EnumToClass<Base, BufferTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ARRAY_BUFFER
# if defined Array
#  pragma push_macro("Array")
#  undef Array
	Transform<BufferTarget::Array> Array;
#  pragma pop_macro("Array")
# else
	Transform<BufferTarget::Array> Array;
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
# if defined AtomicCounter
#  pragma push_macro("AtomicCounter")
#  undef AtomicCounter
	Transform<BufferTarget::AtomicCounter> AtomicCounter;
#  pragma pop_macro("AtomicCounter")
# else
	Transform<BufferTarget::AtomicCounter> AtomicCounter;
# endif
#endif
#if defined GL_COPY_READ_BUFFER
# if defined CopyRead
#  pragma push_macro("CopyRead")
#  undef CopyRead
	Transform<BufferTarget::CopyRead> CopyRead;
#  pragma pop_macro("CopyRead")
# else
	Transform<BufferTarget::CopyRead> CopyRead;
# endif
#endif
#if defined GL_COPY_WRITE_BUFFER
# if defined CopyWrite
#  pragma push_macro("CopyWrite")
#  undef CopyWrite
	Transform<BufferTarget::CopyWrite> CopyWrite;
#  pragma pop_macro("CopyWrite")
# else
	Transform<BufferTarget::CopyWrite> CopyWrite;
# endif
#endif
#if defined GL_DISPATCH_INDIRECT_BUFFER
# if defined DispatchIndirect
#  pragma push_macro("DispatchIndirect")
#  undef DispatchIndirect
	Transform<BufferTarget::DispatchIndirect> DispatchIndirect;
#  pragma pop_macro("DispatchIndirect")
# else
	Transform<BufferTarget::DispatchIndirect> DispatchIndirect;
# endif
#endif
#if defined GL_DRAW_INDIRECT_BUFFER
# if defined DrawIndirect
#  pragma push_macro("DrawIndirect")
#  undef DrawIndirect
	Transform<BufferTarget::DrawIndirect> DrawIndirect;
#  pragma pop_macro("DrawIndirect")
# else
	Transform<BufferTarget::DrawIndirect> DrawIndirect;
# endif
#endif
#if defined GL_ELEMENT_ARRAY_BUFFER
# if defined ElementArray
#  pragma push_macro("ElementArray")
#  undef ElementArray
	Transform<BufferTarget::ElementArray> ElementArray;
#  pragma pop_macro("ElementArray")
# else
	Transform<BufferTarget::ElementArray> ElementArray;
# endif
#endif
#if defined GL_PIXEL_PACK_BUFFER
# if defined PixelPack
#  pragma push_macro("PixelPack")
#  undef PixelPack
	Transform<BufferTarget::PixelPack> PixelPack;
#  pragma pop_macro("PixelPack")
# else
	Transform<BufferTarget::PixelPack> PixelPack;
# endif
#endif
#if defined GL_PIXEL_UNPACK_BUFFER
# if defined PixelUnpack
#  pragma push_macro("PixelUnpack")
#  undef PixelUnpack
	Transform<BufferTarget::PixelUnpack> PixelUnpack;
#  pragma pop_macro("PixelUnpack")
# else
	Transform<BufferTarget::PixelUnpack> PixelUnpack;
# endif
#endif
#if defined GL_SHADER_STORAGE_BUFFER
# if defined ShaderStorage
#  pragma push_macro("ShaderStorage")
#  undef ShaderStorage
	Transform<BufferTarget::ShaderStorage> ShaderStorage;
#  pragma pop_macro("ShaderStorage")
# else
	Transform<BufferTarget::ShaderStorage> ShaderStorage;
# endif
#endif
#if defined GL_TEXTURE_BUFFER
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	Transform<BufferTarget::Texture> Texture;
#  pragma pop_macro("Texture")
# else
	Transform<BufferTarget::Texture> Texture;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	Transform<BufferTarget::TransformFeedback> TransformFeedback;
#  pragma pop_macro("TransformFeedback")
# else
	Transform<BufferTarget::TransformFeedback> TransformFeedback;
# endif
#endif
#if defined GL_UNIFORM_BUFFER
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	Transform<BufferTarget::Uniform> Uniform;
#  pragma pop_macro("Uniform")
# else
	Transform<BufferTarget::Uniform> Uniform;
# endif
#endif
#if defined GL_QUERY_BUFFER
# if defined Query
#  pragma push_macro("Query")
#  undef Query
	Transform<BufferTarget::Query> Query;
#  pragma pop_macro("Query")
# else
	Transform<BufferTarget::Query> Query;
# endif
#endif
#if defined GL_PARAMETER_BUFFER_ARB
# if defined Parameter
#  pragma push_macro("Parameter")
#  undef Parameter
	Transform<BufferTarget::Parameter> Parameter;
#  pragma pop_macro("Parameter")
# else
	Transform<BufferTarget::Parameter> Parameter;
# endif
#endif
#if defined GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD
# if defined ExternalVirtualMemory
#  pragma push_macro("ExternalVirtualMemory")
#  undef ExternalVirtualMemory
	Transform<BufferTarget::ExternalVirtualMemory> ExternalVirtualMemory;
#  pragma pop_macro("ExternalVirtualMemory")
# else
	Transform<BufferTarget::ExternalVirtualMemory> ExternalVirtualMemory;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_ARRAY_BUFFER
# if defined Array
#  pragma push_macro("Array")
#  undef Array
	 , Array(_base())
#  pragma pop_macro("Array")
# else
	 , Array(_base())
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
# if defined AtomicCounter
#  pragma push_macro("AtomicCounter")
#  undef AtomicCounter
	 , AtomicCounter(_base())
#  pragma pop_macro("AtomicCounter")
# else
	 , AtomicCounter(_base())
# endif
#endif
#if defined GL_COPY_READ_BUFFER
# if defined CopyRead
#  pragma push_macro("CopyRead")
#  undef CopyRead
	 , CopyRead(_base())
#  pragma pop_macro("CopyRead")
# else
	 , CopyRead(_base())
# endif
#endif
#if defined GL_COPY_WRITE_BUFFER
# if defined CopyWrite
#  pragma push_macro("CopyWrite")
#  undef CopyWrite
	 , CopyWrite(_base())
#  pragma pop_macro("CopyWrite")
# else
	 , CopyWrite(_base())
# endif
#endif
#if defined GL_DISPATCH_INDIRECT_BUFFER
# if defined DispatchIndirect
#  pragma push_macro("DispatchIndirect")
#  undef DispatchIndirect
	 , DispatchIndirect(_base())
#  pragma pop_macro("DispatchIndirect")
# else
	 , DispatchIndirect(_base())
# endif
#endif
#if defined GL_DRAW_INDIRECT_BUFFER
# if defined DrawIndirect
#  pragma push_macro("DrawIndirect")
#  undef DrawIndirect
	 , DrawIndirect(_base())
#  pragma pop_macro("DrawIndirect")
# else
	 , DrawIndirect(_base())
# endif
#endif
#if defined GL_ELEMENT_ARRAY_BUFFER
# if defined ElementArray
#  pragma push_macro("ElementArray")
#  undef ElementArray
	 , ElementArray(_base())
#  pragma pop_macro("ElementArray")
# else
	 , ElementArray(_base())
# endif
#endif
#if defined GL_PIXEL_PACK_BUFFER
# if defined PixelPack
#  pragma push_macro("PixelPack")
#  undef PixelPack
	 , PixelPack(_base())
#  pragma pop_macro("PixelPack")
# else
	 , PixelPack(_base())
# endif
#endif
#if defined GL_PIXEL_UNPACK_BUFFER
# if defined PixelUnpack
#  pragma push_macro("PixelUnpack")
#  undef PixelUnpack
	 , PixelUnpack(_base())
#  pragma pop_macro("PixelUnpack")
# else
	 , PixelUnpack(_base())
# endif
#endif
#if defined GL_SHADER_STORAGE_BUFFER
# if defined ShaderStorage
#  pragma push_macro("ShaderStorage")
#  undef ShaderStorage
	 , ShaderStorage(_base())
#  pragma pop_macro("ShaderStorage")
# else
	 , ShaderStorage(_base())
# endif
#endif
#if defined GL_TEXTURE_BUFFER
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	 , Texture(_base())
#  pragma pop_macro("Texture")
# else
	 , Texture(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	 , TransformFeedback(_base())
#  pragma pop_macro("TransformFeedback")
# else
	 , TransformFeedback(_base())
# endif
#endif
#if defined GL_UNIFORM_BUFFER
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	 , Uniform(_base())
#  pragma pop_macro("Uniform")
# else
	 , Uniform(_base())
# endif
#endif
#if defined GL_QUERY_BUFFER
# if defined Query
#  pragma push_macro("Query")
#  undef Query
	 , Query(_base())
#  pragma pop_macro("Query")
# else
	 , Query(_base())
# endif
#endif
#if defined GL_PARAMETER_BUFFER_ARB
# if defined Parameter
#  pragma push_macro("Parameter")
#  undef Parameter
	 , Parameter(_base())
#  pragma pop_macro("Parameter")
# else
	 , Parameter(_base())
# endif
#endif
#if defined GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD
# if defined ExternalVirtualMemory
#  pragma push_macro("ExternalVirtualMemory")
#  undef ExternalVirtualMemory
	 , ExternalVirtualMemory(_base())
#  pragma pop_macro("ExternalVirtualMemory")
# else
	 , ExternalVirtualMemory(_base())
# endif
#endif
	{ }
};

} // namespace enums

