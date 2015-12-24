//  File implement/oglplus/enums/ext/amd_perf_monitor_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/amd_perf_monitor_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PerfMonitorAMDType> class Transform>
class EnumToClass<Base, PerfMonitorAMDType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	Transform<PerfMonitorAMDType::UnsignedInt> UnsignedInt;
#  pragma pop_macro("UnsignedInt")
# else
	Transform<PerfMonitorAMDType::UnsignedInt> UnsignedInt;
# endif
#endif
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	Transform<PerfMonitorAMDType::Float> Float;
#  pragma pop_macro("Float")
# else
	Transform<PerfMonitorAMDType::Float> Float;
# endif
#endif
#if defined GL_UNSIGNED_INT64_AMD
# if defined UnsignedInt64
#  pragma push_macro("UnsignedInt64")
#  undef UnsignedInt64
	Transform<PerfMonitorAMDType::UnsignedInt64> UnsignedInt64;
#  pragma pop_macro("UnsignedInt64")
# else
	Transform<PerfMonitorAMDType::UnsignedInt64> UnsignedInt64;
# endif
#endif
#if defined GL_PERCENTAGE_AMD
# if defined Percentage
#  pragma push_macro("Percentage")
#  undef Percentage
	Transform<PerfMonitorAMDType::Percentage> Percentage;
#  pragma pop_macro("Percentage")
# else
	Transform<PerfMonitorAMDType::Percentage> Percentage;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	 , UnsignedInt(_base())
#  pragma pop_macro("UnsignedInt")
# else
	 , UnsignedInt(_base())
# endif
#endif
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	 , Float(_base())
#  pragma pop_macro("Float")
# else
	 , Float(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT64_AMD
# if defined UnsignedInt64
#  pragma push_macro("UnsignedInt64")
#  undef UnsignedInt64
	 , UnsignedInt64(_base())
#  pragma pop_macro("UnsignedInt64")
# else
	 , UnsignedInt64(_base())
# endif
#endif
#if defined GL_PERCENTAGE_AMD
# if defined Percentage
#  pragma push_macro("Percentage")
#  undef Percentage
	 , Percentage(_base())
#  pragma pop_macro("Percentage")
# else
	 , Percentage(_base())
# endif
#endif
	{ }
};

} // namespace enums

