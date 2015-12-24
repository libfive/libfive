//  File implement/oglplus/enums/precision_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/precision_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PrecisionType> class Transform>
class EnumToClass<Base, PrecisionType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_LOW_FLOAT
# if defined LowFloat
#  pragma push_macro("LowFloat")
#  undef LowFloat
	Transform<PrecisionType::LowFloat> LowFloat;
#  pragma pop_macro("LowFloat")
# else
	Transform<PrecisionType::LowFloat> LowFloat;
# endif
#endif
#if defined GL_MEDIUM_FLOAT
# if defined MediumFloat
#  pragma push_macro("MediumFloat")
#  undef MediumFloat
	Transform<PrecisionType::MediumFloat> MediumFloat;
#  pragma pop_macro("MediumFloat")
# else
	Transform<PrecisionType::MediumFloat> MediumFloat;
# endif
#endif
#if defined GL_HIGH_FLOAT
# if defined HighFloat
#  pragma push_macro("HighFloat")
#  undef HighFloat
	Transform<PrecisionType::HighFloat> HighFloat;
#  pragma pop_macro("HighFloat")
# else
	Transform<PrecisionType::HighFloat> HighFloat;
# endif
#endif
#if defined GL_LOW_INT
# if defined LowInt
#  pragma push_macro("LowInt")
#  undef LowInt
	Transform<PrecisionType::LowInt> LowInt;
#  pragma pop_macro("LowInt")
# else
	Transform<PrecisionType::LowInt> LowInt;
# endif
#endif
#if defined GL_MEDIUM_INT
# if defined MediumInt
#  pragma push_macro("MediumInt")
#  undef MediumInt
	Transform<PrecisionType::MediumInt> MediumInt;
#  pragma pop_macro("MediumInt")
# else
	Transform<PrecisionType::MediumInt> MediumInt;
# endif
#endif
#if defined GL_HIGH_INT
# if defined HighInt
#  pragma push_macro("HighInt")
#  undef HighInt
	Transform<PrecisionType::HighInt> HighInt;
#  pragma pop_macro("HighInt")
# else
	Transform<PrecisionType::HighInt> HighInt;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_LOW_FLOAT
# if defined LowFloat
#  pragma push_macro("LowFloat")
#  undef LowFloat
	 , LowFloat(_base())
#  pragma pop_macro("LowFloat")
# else
	 , LowFloat(_base())
# endif
#endif
#if defined GL_MEDIUM_FLOAT
# if defined MediumFloat
#  pragma push_macro("MediumFloat")
#  undef MediumFloat
	 , MediumFloat(_base())
#  pragma pop_macro("MediumFloat")
# else
	 , MediumFloat(_base())
# endif
#endif
#if defined GL_HIGH_FLOAT
# if defined HighFloat
#  pragma push_macro("HighFloat")
#  undef HighFloat
	 , HighFloat(_base())
#  pragma pop_macro("HighFloat")
# else
	 , HighFloat(_base())
# endif
#endif
#if defined GL_LOW_INT
# if defined LowInt
#  pragma push_macro("LowInt")
#  undef LowInt
	 , LowInt(_base())
#  pragma pop_macro("LowInt")
# else
	 , LowInt(_base())
# endif
#endif
#if defined GL_MEDIUM_INT
# if defined MediumInt
#  pragma push_macro("MediumInt")
#  undef MediumInt
	 , MediumInt(_base())
#  pragma pop_macro("MediumInt")
# else
	 , MediumInt(_base())
# endif
#endif
#if defined GL_HIGH_INT
# if defined HighInt
#  pragma push_macro("HighInt")
#  undef HighInt
	 , HighInt(_base())
#  pragma pop_macro("HighInt")
# else
	 , HighInt(_base())
# endif
#endif
	{ }
};

} // namespace enums

