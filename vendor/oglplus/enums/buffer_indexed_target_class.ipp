//  File implement/oglplus/enums/buffer_indexed_target_class.ipp
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
template <typename Base, template<BufferIndexedTarget> class Transform>
class EnumToClass<Base, BufferIndexedTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ATOMIC_COUNTER_BUFFER
# if defined AtomicCounter
#  pragma push_macro("AtomicCounter")
#  undef AtomicCounter
	Transform<BufferIndexedTarget::AtomicCounter> AtomicCounter;
#  pragma pop_macro("AtomicCounter")
# else
	Transform<BufferIndexedTarget::AtomicCounter> AtomicCounter;
# endif
#endif
#if defined GL_SHADER_STORAGE_BUFFER
# if defined ShaderStorage
#  pragma push_macro("ShaderStorage")
#  undef ShaderStorage
	Transform<BufferIndexedTarget::ShaderStorage> ShaderStorage;
#  pragma pop_macro("ShaderStorage")
# else
	Transform<BufferIndexedTarget::ShaderStorage> ShaderStorage;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	Transform<BufferIndexedTarget::TransformFeedback> TransformFeedback;
#  pragma pop_macro("TransformFeedback")
# else
	Transform<BufferIndexedTarget::TransformFeedback> TransformFeedback;
# endif
#endif
#if defined GL_UNIFORM_BUFFER
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	Transform<BufferIndexedTarget::Uniform> Uniform;
#  pragma pop_macro("Uniform")
# else
	Transform<BufferIndexedTarget::Uniform> Uniform;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
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
	{ }
};

} // namespace enums

