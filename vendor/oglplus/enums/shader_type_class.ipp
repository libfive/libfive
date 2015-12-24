//  File implement/oglplus/enums/shader_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/shader_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ShaderType> class Transform>
class EnumToClass<Base, ShaderType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_VERTEX_SHADER
# if defined Vertex
#  pragma push_macro("Vertex")
#  undef Vertex
	Transform<ShaderType::Vertex> Vertex;
#  pragma pop_macro("Vertex")
# else
	Transform<ShaderType::Vertex> Vertex;
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER
# if defined TessControl
#  pragma push_macro("TessControl")
#  undef TessControl
	Transform<ShaderType::TessControl> TessControl;
#  pragma pop_macro("TessControl")
# else
	Transform<ShaderType::TessControl> TessControl;
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER
# if defined TessEvaluation
#  pragma push_macro("TessEvaluation")
#  undef TessEvaluation
	Transform<ShaderType::TessEvaluation> TessEvaluation;
#  pragma pop_macro("TessEvaluation")
# else
	Transform<ShaderType::TessEvaluation> TessEvaluation;
# endif
#endif
#if defined GL_GEOMETRY_SHADER
# if defined Geometry
#  pragma push_macro("Geometry")
#  undef Geometry
	Transform<ShaderType::Geometry> Geometry;
#  pragma pop_macro("Geometry")
# else
	Transform<ShaderType::Geometry> Geometry;
# endif
#endif
#if defined GL_FRAGMENT_SHADER
# if defined Fragment
#  pragma push_macro("Fragment")
#  undef Fragment
	Transform<ShaderType::Fragment> Fragment;
#  pragma pop_macro("Fragment")
# else
	Transform<ShaderType::Fragment> Fragment;
# endif
#endif
#if defined GL_COMPUTE_SHADER
# if defined Compute
#  pragma push_macro("Compute")
#  undef Compute
	Transform<ShaderType::Compute> Compute;
#  pragma pop_macro("Compute")
# else
	Transform<ShaderType::Compute> Compute;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_VERTEX_SHADER
# if defined Vertex
#  pragma push_macro("Vertex")
#  undef Vertex
	 , Vertex(_base())
#  pragma pop_macro("Vertex")
# else
	 , Vertex(_base())
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER
# if defined TessControl
#  pragma push_macro("TessControl")
#  undef TessControl
	 , TessControl(_base())
#  pragma pop_macro("TessControl")
# else
	 , TessControl(_base())
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER
# if defined TessEvaluation
#  pragma push_macro("TessEvaluation")
#  undef TessEvaluation
	 , TessEvaluation(_base())
#  pragma pop_macro("TessEvaluation")
# else
	 , TessEvaluation(_base())
# endif
#endif
#if defined GL_GEOMETRY_SHADER
# if defined Geometry
#  pragma push_macro("Geometry")
#  undef Geometry
	 , Geometry(_base())
#  pragma pop_macro("Geometry")
# else
	 , Geometry(_base())
# endif
#endif
#if defined GL_FRAGMENT_SHADER
# if defined Fragment
#  pragma push_macro("Fragment")
#  undef Fragment
	 , Fragment(_base())
#  pragma pop_macro("Fragment")
# else
	 , Fragment(_base())
# endif
#endif
#if defined GL_COMPUTE_SHADER
# if defined Compute
#  pragma push_macro("Compute")
#  undef Compute
	 , Compute(_base())
#  pragma pop_macro("Compute")
# else
	 , Compute(_base())
# endif
#endif
	{ }
};

} // namespace enums

