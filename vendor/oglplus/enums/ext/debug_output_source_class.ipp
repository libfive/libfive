//  File implement/oglplus/enums/ext/debug_output_source_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_output_source.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<DebugOutputARBSource> class Transform>
class EnumToClass<Base, DebugOutputARBSource, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DEBUG_SOURCE_API_ARB
# if defined API
#  pragma push_macro("API")
#  undef API
	Transform<DebugOutputARBSource::API> API;
#  pragma pop_macro("API")
# else
	Transform<DebugOutputARBSource::API> API;
# endif
#endif
#if defined GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB
# if defined WindowSystem
#  pragma push_macro("WindowSystem")
#  undef WindowSystem
	Transform<DebugOutputARBSource::WindowSystem> WindowSystem;
#  pragma pop_macro("WindowSystem")
# else
	Transform<DebugOutputARBSource::WindowSystem> WindowSystem;
# endif
#endif
#if defined GL_DEBUG_SOURCE_SHADER_COMPILER_ARB
# if defined ShaderCompiler
#  pragma push_macro("ShaderCompiler")
#  undef ShaderCompiler
	Transform<DebugOutputARBSource::ShaderCompiler> ShaderCompiler;
#  pragma pop_macro("ShaderCompiler")
# else
	Transform<DebugOutputARBSource::ShaderCompiler> ShaderCompiler;
# endif
#endif
#if defined GL_DEBUG_SOURCE_THIRD_PARTY_ARB
# if defined ThirdParty
#  pragma push_macro("ThirdParty")
#  undef ThirdParty
	Transform<DebugOutputARBSource::ThirdParty> ThirdParty;
#  pragma pop_macro("ThirdParty")
# else
	Transform<DebugOutputARBSource::ThirdParty> ThirdParty;
# endif
#endif
#if defined GL_DEBUG_SOURCE_APPLICATION_ARB
# if defined Application
#  pragma push_macro("Application")
#  undef Application
	Transform<DebugOutputARBSource::Application> Application;
#  pragma pop_macro("Application")
# else
	Transform<DebugOutputARBSource::Application> Application;
# endif
#endif
#if defined GL_DEBUG_SOURCE_OTHER_ARB
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	Transform<DebugOutputARBSource::Other> Other;
#  pragma pop_macro("Other")
# else
	Transform<DebugOutputARBSource::Other> Other;
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	Transform<DebugOutputARBSource::DontCare> DontCare;
#  pragma pop_macro("DontCare")
# else
	Transform<DebugOutputARBSource::DontCare> DontCare;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DEBUG_SOURCE_API_ARB
# if defined API
#  pragma push_macro("API")
#  undef API
	 , API(_base())
#  pragma pop_macro("API")
# else
	 , API(_base())
# endif
#endif
#if defined GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB
# if defined WindowSystem
#  pragma push_macro("WindowSystem")
#  undef WindowSystem
	 , WindowSystem(_base())
#  pragma pop_macro("WindowSystem")
# else
	 , WindowSystem(_base())
# endif
#endif
#if defined GL_DEBUG_SOURCE_SHADER_COMPILER_ARB
# if defined ShaderCompiler
#  pragma push_macro("ShaderCompiler")
#  undef ShaderCompiler
	 , ShaderCompiler(_base())
#  pragma pop_macro("ShaderCompiler")
# else
	 , ShaderCompiler(_base())
# endif
#endif
#if defined GL_DEBUG_SOURCE_THIRD_PARTY_ARB
# if defined ThirdParty
#  pragma push_macro("ThirdParty")
#  undef ThirdParty
	 , ThirdParty(_base())
#  pragma pop_macro("ThirdParty")
# else
	 , ThirdParty(_base())
# endif
#endif
#if defined GL_DEBUG_SOURCE_APPLICATION_ARB
# if defined Application
#  pragma push_macro("Application")
#  undef Application
	 , Application(_base())
#  pragma pop_macro("Application")
# else
	 , Application(_base())
# endif
#endif
#if defined GL_DEBUG_SOURCE_OTHER_ARB
# if defined Other
#  pragma push_macro("Other")
#  undef Other
	 , Other(_base())
#  pragma pop_macro("Other")
# else
	 , Other(_base())
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	 , DontCare(_base())
#  pragma pop_macro("DontCare")
# else
	 , DontCare(_base())
# endif
#endif
	{ }
};

} // namespace enums

