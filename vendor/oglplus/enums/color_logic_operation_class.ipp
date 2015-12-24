//  File implement/oglplus/enums/color_logic_operation_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_logic_operation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ColorLogicOperation> class Transform>
class EnumToClass<Base, ColorLogicOperation, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CLEAR
# if defined Clear
#  pragma push_macro("Clear")
#  undef Clear
	Transform<ColorLogicOperation::Clear> Clear;
#  pragma pop_macro("Clear")
# else
	Transform<ColorLogicOperation::Clear> Clear;
# endif
#endif
#if defined GL_AND
# if defined And
#  pragma push_macro("And")
#  undef And
	Transform<ColorLogicOperation::And> And;
#  pragma pop_macro("And")
# else
	Transform<ColorLogicOperation::And> And;
# endif
#endif
#if defined GL_AND_REVERSE
# if defined AndReverse
#  pragma push_macro("AndReverse")
#  undef AndReverse
	Transform<ColorLogicOperation::AndReverse> AndReverse;
#  pragma pop_macro("AndReverse")
# else
	Transform<ColorLogicOperation::AndReverse> AndReverse;
# endif
#endif
#if defined GL_COPY
# if defined Copy
#  pragma push_macro("Copy")
#  undef Copy
	Transform<ColorLogicOperation::Copy> Copy;
#  pragma pop_macro("Copy")
# else
	Transform<ColorLogicOperation::Copy> Copy;
# endif
#endif
#if defined GL_AND_INVERTED
# if defined AndInverted
#  pragma push_macro("AndInverted")
#  undef AndInverted
	Transform<ColorLogicOperation::AndInverted> AndInverted;
#  pragma pop_macro("AndInverted")
# else
	Transform<ColorLogicOperation::AndInverted> AndInverted;
# endif
#endif
#if defined GL_NOOP
# if defined Noop
#  pragma push_macro("Noop")
#  undef Noop
	Transform<ColorLogicOperation::Noop> Noop;
#  pragma pop_macro("Noop")
# else
	Transform<ColorLogicOperation::Noop> Noop;
# endif
#endif
#if defined GL_XOR
# if defined Xor
#  pragma push_macro("Xor")
#  undef Xor
	Transform<ColorLogicOperation::Xor> Xor;
#  pragma pop_macro("Xor")
# else
	Transform<ColorLogicOperation::Xor> Xor;
# endif
#endif
#if defined GL_OR
# if defined Or
#  pragma push_macro("Or")
#  undef Or
	Transform<ColorLogicOperation::Or> Or;
#  pragma pop_macro("Or")
# else
	Transform<ColorLogicOperation::Or> Or;
# endif
#endif
#if defined GL_NOR
# if defined Nor
#  pragma push_macro("Nor")
#  undef Nor
	Transform<ColorLogicOperation::Nor> Nor;
#  pragma pop_macro("Nor")
# else
	Transform<ColorLogicOperation::Nor> Nor;
# endif
#endif
#if defined GL_EQUIV
# if defined Equiv
#  pragma push_macro("Equiv")
#  undef Equiv
	Transform<ColorLogicOperation::Equiv> Equiv;
#  pragma pop_macro("Equiv")
# else
	Transform<ColorLogicOperation::Equiv> Equiv;
# endif
#endif
#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	Transform<ColorLogicOperation::Invert> Invert;
#  pragma pop_macro("Invert")
# else
	Transform<ColorLogicOperation::Invert> Invert;
# endif
#endif
#if defined GL_OR_REVERSE
# if defined OrReverse
#  pragma push_macro("OrReverse")
#  undef OrReverse
	Transform<ColorLogicOperation::OrReverse> OrReverse;
#  pragma pop_macro("OrReverse")
# else
	Transform<ColorLogicOperation::OrReverse> OrReverse;
# endif
#endif
#if defined GL_COPY_INVERTED
# if defined CopyInverted
#  pragma push_macro("CopyInverted")
#  undef CopyInverted
	Transform<ColorLogicOperation::CopyInverted> CopyInverted;
#  pragma pop_macro("CopyInverted")
# else
	Transform<ColorLogicOperation::CopyInverted> CopyInverted;
# endif
#endif
#if defined GL_OR_INVERTED
# if defined OrInverted
#  pragma push_macro("OrInverted")
#  undef OrInverted
	Transform<ColorLogicOperation::OrInverted> OrInverted;
#  pragma pop_macro("OrInverted")
# else
	Transform<ColorLogicOperation::OrInverted> OrInverted;
# endif
#endif
#if defined GL_NAND
# if defined Nand
#  pragma push_macro("Nand")
#  undef Nand
	Transform<ColorLogicOperation::Nand> Nand;
#  pragma pop_macro("Nand")
# else
	Transform<ColorLogicOperation::Nand> Nand;
# endif
#endif
#if defined GL_SET
# if defined Set
#  pragma push_macro("Set")
#  undef Set
	Transform<ColorLogicOperation::Set> Set;
#  pragma pop_macro("Set")
# else
	Transform<ColorLogicOperation::Set> Set;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CLEAR
# if defined Clear
#  pragma push_macro("Clear")
#  undef Clear
	 , Clear(_base())
#  pragma pop_macro("Clear")
# else
	 , Clear(_base())
# endif
#endif
#if defined GL_AND
# if defined And
#  pragma push_macro("And")
#  undef And
	 , And(_base())
#  pragma pop_macro("And")
# else
	 , And(_base())
# endif
#endif
#if defined GL_AND_REVERSE
# if defined AndReverse
#  pragma push_macro("AndReverse")
#  undef AndReverse
	 , AndReverse(_base())
#  pragma pop_macro("AndReverse")
# else
	 , AndReverse(_base())
# endif
#endif
#if defined GL_COPY
# if defined Copy
#  pragma push_macro("Copy")
#  undef Copy
	 , Copy(_base())
#  pragma pop_macro("Copy")
# else
	 , Copy(_base())
# endif
#endif
#if defined GL_AND_INVERTED
# if defined AndInverted
#  pragma push_macro("AndInverted")
#  undef AndInverted
	 , AndInverted(_base())
#  pragma pop_macro("AndInverted")
# else
	 , AndInverted(_base())
# endif
#endif
#if defined GL_NOOP
# if defined Noop
#  pragma push_macro("Noop")
#  undef Noop
	 , Noop(_base())
#  pragma pop_macro("Noop")
# else
	 , Noop(_base())
# endif
#endif
#if defined GL_XOR
# if defined Xor
#  pragma push_macro("Xor")
#  undef Xor
	 , Xor(_base())
#  pragma pop_macro("Xor")
# else
	 , Xor(_base())
# endif
#endif
#if defined GL_OR
# if defined Or
#  pragma push_macro("Or")
#  undef Or
	 , Or(_base())
#  pragma pop_macro("Or")
# else
	 , Or(_base())
# endif
#endif
#if defined GL_NOR
# if defined Nor
#  pragma push_macro("Nor")
#  undef Nor
	 , Nor(_base())
#  pragma pop_macro("Nor")
# else
	 , Nor(_base())
# endif
#endif
#if defined GL_EQUIV
# if defined Equiv
#  pragma push_macro("Equiv")
#  undef Equiv
	 , Equiv(_base())
#  pragma pop_macro("Equiv")
# else
	 , Equiv(_base())
# endif
#endif
#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	 , Invert(_base())
#  pragma pop_macro("Invert")
# else
	 , Invert(_base())
# endif
#endif
#if defined GL_OR_REVERSE
# if defined OrReverse
#  pragma push_macro("OrReverse")
#  undef OrReverse
	 , OrReverse(_base())
#  pragma pop_macro("OrReverse")
# else
	 , OrReverse(_base())
# endif
#endif
#if defined GL_COPY_INVERTED
# if defined CopyInverted
#  pragma push_macro("CopyInverted")
#  undef CopyInverted
	 , CopyInverted(_base())
#  pragma pop_macro("CopyInverted")
# else
	 , CopyInverted(_base())
# endif
#endif
#if defined GL_OR_INVERTED
# if defined OrInverted
#  pragma push_macro("OrInverted")
#  undef OrInverted
	 , OrInverted(_base())
#  pragma pop_macro("OrInverted")
# else
	 , OrInverted(_base())
# endif
#endif
#if defined GL_NAND
# if defined Nand
#  pragma push_macro("Nand")
#  undef Nand
	 , Nand(_base())
#  pragma pop_macro("Nand")
# else
	 , Nand(_base())
# endif
#endif
#if defined GL_SET
# if defined Set
#  pragma push_macro("Set")
#  undef Set
	 , Set(_base())
#  pragma pop_macro("Set")
# else
	 , Set(_base())
# endif
#endif
	{ }
};

} // namespace enums

