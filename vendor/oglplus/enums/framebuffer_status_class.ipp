//  File implement/oglplus/enums/framebuffer_status_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<FramebufferStatus> class Transform>
class EnumToClass<Base, FramebufferStatus, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FRAMEBUFFER_COMPLETE
# if defined Complete
#  pragma push_macro("Complete")
#  undef Complete
	Transform<FramebufferStatus::Complete> Complete;
#  pragma pop_macro("Complete")
# else
	Transform<FramebufferStatus::Complete> Complete;
# endif
#endif
#if defined GL_FRAMEBUFFER_UNDEFINED
# if defined Undefined
#  pragma push_macro("Undefined")
#  undef Undefined
	Transform<FramebufferStatus::Undefined> Undefined;
#  pragma pop_macro("Undefined")
# else
	Transform<FramebufferStatus::Undefined> Undefined;
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
# if defined IncompleteAttachment
#  pragma push_macro("IncompleteAttachment")
#  undef IncompleteAttachment
	Transform<FramebufferStatus::IncompleteAttachment> IncompleteAttachment;
#  pragma pop_macro("IncompleteAttachment")
# else
	Transform<FramebufferStatus::IncompleteAttachment> IncompleteAttachment;
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
# if defined IncompleteMissingAttachment
#  pragma push_macro("IncompleteMissingAttachment")
#  undef IncompleteMissingAttachment
	Transform<FramebufferStatus::IncompleteMissingAttachment> IncompleteMissingAttachment;
#  pragma pop_macro("IncompleteMissingAttachment")
# else
	Transform<FramebufferStatus::IncompleteMissingAttachment> IncompleteMissingAttachment;
# endif
#endif
#if defined GL_FRAMEBUFFER_UNSUPPORTED
# if defined Unsupported
#  pragma push_macro("Unsupported")
#  undef Unsupported
	Transform<FramebufferStatus::Unsupported> Unsupported;
#  pragma pop_macro("Unsupported")
# else
	Transform<FramebufferStatus::Unsupported> Unsupported;
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
# if defined IncompleteMultisample
#  pragma push_macro("IncompleteMultisample")
#  undef IncompleteMultisample
	Transform<FramebufferStatus::IncompleteMultisample> IncompleteMultisample;
#  pragma pop_macro("IncompleteMultisample")
# else
	Transform<FramebufferStatus::IncompleteMultisample> IncompleteMultisample;
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
# if defined IncompleteLayerTargets
#  pragma push_macro("IncompleteLayerTargets")
#  undef IncompleteLayerTargets
	Transform<FramebufferStatus::IncompleteLayerTargets> IncompleteLayerTargets;
#  pragma pop_macro("IncompleteLayerTargets")
# else
	Transform<FramebufferStatus::IncompleteLayerTargets> IncompleteLayerTargets;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FRAMEBUFFER_COMPLETE
# if defined Complete
#  pragma push_macro("Complete")
#  undef Complete
	 , Complete(_base())
#  pragma pop_macro("Complete")
# else
	 , Complete(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_UNDEFINED
# if defined Undefined
#  pragma push_macro("Undefined")
#  undef Undefined
	 , Undefined(_base())
#  pragma pop_macro("Undefined")
# else
	 , Undefined(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
# if defined IncompleteAttachment
#  pragma push_macro("IncompleteAttachment")
#  undef IncompleteAttachment
	 , IncompleteAttachment(_base())
#  pragma pop_macro("IncompleteAttachment")
# else
	 , IncompleteAttachment(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
# if defined IncompleteMissingAttachment
#  pragma push_macro("IncompleteMissingAttachment")
#  undef IncompleteMissingAttachment
	 , IncompleteMissingAttachment(_base())
#  pragma pop_macro("IncompleteMissingAttachment")
# else
	 , IncompleteMissingAttachment(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_UNSUPPORTED
# if defined Unsupported
#  pragma push_macro("Unsupported")
#  undef Unsupported
	 , Unsupported(_base())
#  pragma pop_macro("Unsupported")
# else
	 , Unsupported(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
# if defined IncompleteMultisample
#  pragma push_macro("IncompleteMultisample")
#  undef IncompleteMultisample
	 , IncompleteMultisample(_base())
#  pragma pop_macro("IncompleteMultisample")
# else
	 , IncompleteMultisample(_base())
# endif
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
# if defined IncompleteLayerTargets
#  pragma push_macro("IncompleteLayerTargets")
#  undef IncompleteLayerTargets
	 , IncompleteLayerTargets(_base())
#  pragma pop_macro("IncompleteLayerTargets")
# else
	 , IncompleteLayerTargets(_base())
# endif
#endif
	{ }
};

} // namespace enums

