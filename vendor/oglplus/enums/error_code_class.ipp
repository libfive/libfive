//  File implement/oglplus/enums/error_code_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/error_code.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ErrorCode> class Transform>
class EnumToClass<Base, ErrorCode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NO_ERROR
# if defined NoError
#  pragma push_macro("NoError")
#  undef NoError
	Transform<ErrorCode::NoError> NoError;
#  pragma pop_macro("NoError")
# else
	Transform<ErrorCode::NoError> NoError;
# endif
#endif
#if defined GL_OUT_OF_MEMORY
# if defined OutOfMemory
#  pragma push_macro("OutOfMemory")
#  undef OutOfMemory
	Transform<ErrorCode::OutOfMemory> OutOfMemory;
#  pragma pop_macro("OutOfMemory")
# else
	Transform<ErrorCode::OutOfMemory> OutOfMemory;
# endif
#endif
#if defined GL_INVALID_ENUM
# if defined InvalidEnum
#  pragma push_macro("InvalidEnum")
#  undef InvalidEnum
	Transform<ErrorCode::InvalidEnum> InvalidEnum;
#  pragma pop_macro("InvalidEnum")
# else
	Transform<ErrorCode::InvalidEnum> InvalidEnum;
# endif
#endif
#if defined GL_INVALID_VALUE
# if defined InvalidValue
#  pragma push_macro("InvalidValue")
#  undef InvalidValue
	Transform<ErrorCode::InvalidValue> InvalidValue;
#  pragma pop_macro("InvalidValue")
# else
	Transform<ErrorCode::InvalidValue> InvalidValue;
# endif
#endif
#if defined GL_INVALID_OPERATION
# if defined InvalidOperation
#  pragma push_macro("InvalidOperation")
#  undef InvalidOperation
	Transform<ErrorCode::InvalidOperation> InvalidOperation;
#  pragma pop_macro("InvalidOperation")
# else
	Transform<ErrorCode::InvalidOperation> InvalidOperation;
# endif
#endif
#if defined GL_INVALID_FRAMEBUFFER_OPERATION
# if defined InvalidFramebufferOperation
#  pragma push_macro("InvalidFramebufferOperation")
#  undef InvalidFramebufferOperation
	Transform<ErrorCode::InvalidFramebufferOperation> InvalidFramebufferOperation;
#  pragma pop_macro("InvalidFramebufferOperation")
# else
	Transform<ErrorCode::InvalidFramebufferOperation> InvalidFramebufferOperation;
# endif
#endif
#if defined GL_STACK_OVERFLOW
# if defined StackOverflow
#  pragma push_macro("StackOverflow")
#  undef StackOverflow
	Transform<ErrorCode::StackOverflow> StackOverflow;
#  pragma pop_macro("StackOverflow")
# else
	Transform<ErrorCode::StackOverflow> StackOverflow;
# endif
#endif
#if defined GL_STACK_UNDERFLOW
# if defined StackUnderflow
#  pragma push_macro("StackUnderflow")
#  undef StackUnderflow
	Transform<ErrorCode::StackUnderflow> StackUnderflow;
#  pragma pop_macro("StackUnderflow")
# else
	Transform<ErrorCode::StackUnderflow> StackUnderflow;
# endif
#endif
#if defined GL_TABLE_TOO_LARGE
# if defined TableTooLarge
#  pragma push_macro("TableTooLarge")
#  undef TableTooLarge
	Transform<ErrorCode::TableTooLarge> TableTooLarge;
#  pragma pop_macro("TableTooLarge")
# else
	Transform<ErrorCode::TableTooLarge> TableTooLarge;
# endif
#endif
#if defined GL_CONTEXT_LOST
# if defined ContextLost
#  pragma push_macro("ContextLost")
#  undef ContextLost
	Transform<ErrorCode::ContextLost> ContextLost;
#  pragma pop_macro("ContextLost")
# else
	Transform<ErrorCode::ContextLost> ContextLost;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NO_ERROR
# if defined NoError
#  pragma push_macro("NoError")
#  undef NoError
	 , NoError(_base())
#  pragma pop_macro("NoError")
# else
	 , NoError(_base())
# endif
#endif
#if defined GL_OUT_OF_MEMORY
# if defined OutOfMemory
#  pragma push_macro("OutOfMemory")
#  undef OutOfMemory
	 , OutOfMemory(_base())
#  pragma pop_macro("OutOfMemory")
# else
	 , OutOfMemory(_base())
# endif
#endif
#if defined GL_INVALID_ENUM
# if defined InvalidEnum
#  pragma push_macro("InvalidEnum")
#  undef InvalidEnum
	 , InvalidEnum(_base())
#  pragma pop_macro("InvalidEnum")
# else
	 , InvalidEnum(_base())
# endif
#endif
#if defined GL_INVALID_VALUE
# if defined InvalidValue
#  pragma push_macro("InvalidValue")
#  undef InvalidValue
	 , InvalidValue(_base())
#  pragma pop_macro("InvalidValue")
# else
	 , InvalidValue(_base())
# endif
#endif
#if defined GL_INVALID_OPERATION
# if defined InvalidOperation
#  pragma push_macro("InvalidOperation")
#  undef InvalidOperation
	 , InvalidOperation(_base())
#  pragma pop_macro("InvalidOperation")
# else
	 , InvalidOperation(_base())
# endif
#endif
#if defined GL_INVALID_FRAMEBUFFER_OPERATION
# if defined InvalidFramebufferOperation
#  pragma push_macro("InvalidFramebufferOperation")
#  undef InvalidFramebufferOperation
	 , InvalidFramebufferOperation(_base())
#  pragma pop_macro("InvalidFramebufferOperation")
# else
	 , InvalidFramebufferOperation(_base())
# endif
#endif
#if defined GL_STACK_OVERFLOW
# if defined StackOverflow
#  pragma push_macro("StackOverflow")
#  undef StackOverflow
	 , StackOverflow(_base())
#  pragma pop_macro("StackOverflow")
# else
	 , StackOverflow(_base())
# endif
#endif
#if defined GL_STACK_UNDERFLOW
# if defined StackUnderflow
#  pragma push_macro("StackUnderflow")
#  undef StackUnderflow
	 , StackUnderflow(_base())
#  pragma pop_macro("StackUnderflow")
# else
	 , StackUnderflow(_base())
# endif
#endif
#if defined GL_TABLE_TOO_LARGE
# if defined TableTooLarge
#  pragma push_macro("TableTooLarge")
#  undef TableTooLarge
	 , TableTooLarge(_base())
#  pragma pop_macro("TableTooLarge")
# else
	 , TableTooLarge(_base())
# endif
#endif
#if defined GL_CONTEXT_LOST
# if defined ContextLost
#  pragma push_macro("ContextLost")
#  undef ContextLost
	 , ContextLost(_base())
#  pragma pop_macro("ContextLost")
# else
	 , ContextLost(_base())
# endif
#endif
	{ }
};

} // namespace enums

