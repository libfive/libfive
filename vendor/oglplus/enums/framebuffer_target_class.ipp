//  File implement/oglplus/enums/framebuffer_target_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<FramebufferTarget> class Transform>
class EnumToClass<Base, FramebufferTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_DRAW_FRAMEBUFFER
# if defined Draw
#  pragma push_macro("Draw")
#  undef Draw
	Transform<FramebufferTarget::Draw> Draw;
#  pragma pop_macro("Draw")
# else
	Transform<FramebufferTarget::Draw> Draw;
# endif
#endif
#if defined GL_READ_FRAMEBUFFER
# if defined Read
#  pragma push_macro("Read")
#  undef Read
	Transform<FramebufferTarget::Read> Read;
#  pragma pop_macro("Read")
# else
	Transform<FramebufferTarget::Read> Read;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_DRAW_FRAMEBUFFER
# if defined Draw
#  pragma push_macro("Draw")
#  undef Draw
	 , Draw(_base())
#  pragma pop_macro("Draw")
# else
	 , Draw(_base())
# endif
#endif
#if defined GL_READ_FRAMEBUFFER
# if defined Read
#  pragma push_macro("Read")
#  undef Read
	 , Read(_base())
#  pragma pop_macro("Read")
# else
	 , Read(_base())
# endif
#endif
	{ }
};

} // namespace enums

