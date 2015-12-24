//  File implement/oglplus/enums/object_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/object_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ObjectType> class Transform>
class EnumToClass<Base, ObjectType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_BUFFER
# if defined Buffer
#  pragma push_macro("Buffer")
#  undef Buffer
	Transform<ObjectType::Buffer> Buffer;
#  pragma pop_macro("Buffer")
# else
	Transform<ObjectType::Buffer> Buffer;
# endif
#endif
#if defined GL_FRAMEBUFFER
# if defined Framebuffer
#  pragma push_macro("Framebuffer")
#  undef Framebuffer
	Transform<ObjectType::Framebuffer> Framebuffer;
#  pragma pop_macro("Framebuffer")
# else
	Transform<ObjectType::Framebuffer> Framebuffer;
# endif
#endif
#if defined GL_PROGRAM_PIPELINE
# if defined ProgramPipeline
#  pragma push_macro("ProgramPipeline")
#  undef ProgramPipeline
	Transform<ObjectType::ProgramPipeline> ProgramPipeline;
#  pragma pop_macro("ProgramPipeline")
# else
	Transform<ObjectType::ProgramPipeline> ProgramPipeline;
# endif
#endif
#if defined GL_PROGRAM
# if defined Program
#  pragma push_macro("Program")
#  undef Program
	Transform<ObjectType::Program> Program;
#  pragma pop_macro("Program")
# else
	Transform<ObjectType::Program> Program;
# endif
#endif
#if defined GL_QUERY
# if defined Query
#  pragma push_macro("Query")
#  undef Query
	Transform<ObjectType::Query> Query;
#  pragma pop_macro("Query")
# else
	Transform<ObjectType::Query> Query;
# endif
#endif
#if defined GL_RENDERBUFFER
# if defined Renderbuffer
#  pragma push_macro("Renderbuffer")
#  undef Renderbuffer
	Transform<ObjectType::Renderbuffer> Renderbuffer;
#  pragma pop_macro("Renderbuffer")
# else
	Transform<ObjectType::Renderbuffer> Renderbuffer;
# endif
#endif
#if defined GL_SAMPLER
# if defined Sampler
#  pragma push_macro("Sampler")
#  undef Sampler
	Transform<ObjectType::Sampler> Sampler;
#  pragma pop_macro("Sampler")
# else
	Transform<ObjectType::Sampler> Sampler;
# endif
#endif
#if defined GL_SHADER
# if defined Shader
#  pragma push_macro("Shader")
#  undef Shader
	Transform<ObjectType::Shader> Shader;
#  pragma pop_macro("Shader")
# else
	Transform<ObjectType::Shader> Shader;
# endif
#endif
#if defined GL_TEXTURE
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	Transform<ObjectType::Texture> Texture;
#  pragma pop_macro("Texture")
# else
	Transform<ObjectType::Texture> Texture;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	Transform<ObjectType::TransformFeedback> TransformFeedback;
#  pragma pop_macro("TransformFeedback")
# else
	Transform<ObjectType::TransformFeedback> TransformFeedback;
# endif
#endif
#if defined GL_VERTEX_ARRAY
# if defined VertexArray
#  pragma push_macro("VertexArray")
#  undef VertexArray
	Transform<ObjectType::VertexArray> VertexArray;
#  pragma pop_macro("VertexArray")
# else
	Transform<ObjectType::VertexArray> VertexArray;
# endif
#endif
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<ObjectType::None> None;
#  pragma pop_macro("None")
# else
	Transform<ObjectType::None> None;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_BUFFER
# if defined Buffer
#  pragma push_macro("Buffer")
#  undef Buffer
	 , Buffer(_base())
#  pragma pop_macro("Buffer")
# else
	 , Buffer(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER
# if defined Framebuffer
#  pragma push_macro("Framebuffer")
#  undef Framebuffer
	 , Framebuffer(_base())
#  pragma pop_macro("Framebuffer")
# else
	 , Framebuffer(_base())
# endif
#endif
#if defined GL_PROGRAM_PIPELINE
# if defined ProgramPipeline
#  pragma push_macro("ProgramPipeline")
#  undef ProgramPipeline
	 , ProgramPipeline(_base())
#  pragma pop_macro("ProgramPipeline")
# else
	 , ProgramPipeline(_base())
# endif
#endif
#if defined GL_PROGRAM
# if defined Program
#  pragma push_macro("Program")
#  undef Program
	 , Program(_base())
#  pragma pop_macro("Program")
# else
	 , Program(_base())
# endif
#endif
#if defined GL_QUERY
# if defined Query
#  pragma push_macro("Query")
#  undef Query
	 , Query(_base())
#  pragma pop_macro("Query")
# else
	 , Query(_base())
# endif
#endif
#if defined GL_RENDERBUFFER
# if defined Renderbuffer
#  pragma push_macro("Renderbuffer")
#  undef Renderbuffer
	 , Renderbuffer(_base())
#  pragma pop_macro("Renderbuffer")
# else
	 , Renderbuffer(_base())
# endif
#endif
#if defined GL_SAMPLER
# if defined Sampler
#  pragma push_macro("Sampler")
#  undef Sampler
	 , Sampler(_base())
#  pragma pop_macro("Sampler")
# else
	 , Sampler(_base())
# endif
#endif
#if defined GL_SHADER
# if defined Shader
#  pragma push_macro("Shader")
#  undef Shader
	 , Shader(_base())
#  pragma pop_macro("Shader")
# else
	 , Shader(_base())
# endif
#endif
#if defined GL_TEXTURE
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	 , Texture(_base())
#  pragma pop_macro("Texture")
# else
	 , Texture(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK
# if defined TransformFeedback
#  pragma push_macro("TransformFeedback")
#  undef TransformFeedback
	 , TransformFeedback(_base())
#  pragma pop_macro("TransformFeedback")
# else
	 , TransformFeedback(_base())
# endif
#endif
#if defined GL_VERTEX_ARRAY
# if defined VertexArray
#  pragma push_macro("VertexArray")
#  undef VertexArray
	 , VertexArray(_base())
#  pragma pop_macro("VertexArray")
# else
	 , VertexArray(_base())
# endif
#endif
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	 , None(_base())
#  pragma pop_macro("None")
# else
	 , None(_base())
# endif
#endif
	{ }
};

} // namespace enums

