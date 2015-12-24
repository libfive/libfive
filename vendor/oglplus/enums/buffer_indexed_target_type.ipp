//  File implement/oglplus/enums/buffer_indexed_target_type.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_indexed_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//

namespace enums {
#if defined GL_ATOMIC_COUNTER_BUFFER
template <>
struct EnumAssocType<BufferIndexedTarget, BufferIndexedTarget::AtomicCounter>
{ typedef AtomicCounterBufferBindingPoint Type; };
#endif
#if defined GL_SHADER_STORAGE_BUFFER
template <>
struct EnumAssocType<BufferIndexedTarget, BufferIndexedTarget::ShaderStorage>
{ typedef ShaderStorageBufferBindingPoint Type; };
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
template <>
struct EnumAssocType<BufferIndexedTarget, BufferIndexedTarget::TransformFeedback>
{ typedef TransformFeedbackBufferBindingPoint Type; };
#endif
#if defined GL_UNIFORM_BUFFER
template <>
struct EnumAssocType<BufferIndexedTarget, BufferIndexedTarget::Uniform>
{ typedef UniformBufferBindingPoint Type; };
#endif
} // namespace enums
