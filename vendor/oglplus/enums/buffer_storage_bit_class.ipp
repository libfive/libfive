//  File implement/oglplus/enums/buffer_storage_bit_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_storage_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BufferStorageBit> class Transform>
class EnumToClass<Base, BufferStorageBit, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_MAP_READ_BIT
# if defined MapRead
#  pragma push_macro("MapRead")
#  undef MapRead
	Transform<BufferStorageBit::MapRead> MapRead;
#  pragma pop_macro("MapRead")
# else
	Transform<BufferStorageBit::MapRead> MapRead;
# endif
#endif
#if defined GL_MAP_WRITE_BIT
# if defined MapWrite
#  pragma push_macro("MapWrite")
#  undef MapWrite
	Transform<BufferStorageBit::MapWrite> MapWrite;
#  pragma pop_macro("MapWrite")
# else
	Transform<BufferStorageBit::MapWrite> MapWrite;
# endif
#endif
#if defined GL_MAP_PERSISTENT_BIT
# if defined MapPersistent
#  pragma push_macro("MapPersistent")
#  undef MapPersistent
	Transform<BufferStorageBit::MapPersistent> MapPersistent;
#  pragma pop_macro("MapPersistent")
# else
	Transform<BufferStorageBit::MapPersistent> MapPersistent;
# endif
#endif
#if defined GL_MAP_COHERENT_BIT
# if defined MapCoherent
#  pragma push_macro("MapCoherent")
#  undef MapCoherent
	Transform<BufferStorageBit::MapCoherent> MapCoherent;
#  pragma pop_macro("MapCoherent")
# else
	Transform<BufferStorageBit::MapCoherent> MapCoherent;
# endif
#endif
#if defined GL_DYNAMIC_STORAGE_BIT
# if defined DynamicStorage
#  pragma push_macro("DynamicStorage")
#  undef DynamicStorage
	Transform<BufferStorageBit::DynamicStorage> DynamicStorage;
#  pragma pop_macro("DynamicStorage")
# else
	Transform<BufferStorageBit::DynamicStorage> DynamicStorage;
# endif
#endif
#if defined GL_CLIENT_STORAGE_BIT
# if defined ClientStorage
#  pragma push_macro("ClientStorage")
#  undef ClientStorage
	Transform<BufferStorageBit::ClientStorage> ClientStorage;
#  pragma pop_macro("ClientStorage")
# else
	Transform<BufferStorageBit::ClientStorage> ClientStorage;
# endif
#endif
#if defined GL_SPARSE_STORAGE_BIT_ARB
# if defined SparseStorage
#  pragma push_macro("SparseStorage")
#  undef SparseStorage
	Transform<BufferStorageBit::SparseStorage> SparseStorage;
#  pragma pop_macro("SparseStorage")
# else
	Transform<BufferStorageBit::SparseStorage> SparseStorage;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_MAP_READ_BIT
# if defined MapRead
#  pragma push_macro("MapRead")
#  undef MapRead
	 , MapRead(_base())
#  pragma pop_macro("MapRead")
# else
	 , MapRead(_base())
# endif
#endif
#if defined GL_MAP_WRITE_BIT
# if defined MapWrite
#  pragma push_macro("MapWrite")
#  undef MapWrite
	 , MapWrite(_base())
#  pragma pop_macro("MapWrite")
# else
	 , MapWrite(_base())
# endif
#endif
#if defined GL_MAP_PERSISTENT_BIT
# if defined MapPersistent
#  pragma push_macro("MapPersistent")
#  undef MapPersistent
	 , MapPersistent(_base())
#  pragma pop_macro("MapPersistent")
# else
	 , MapPersistent(_base())
# endif
#endif
#if defined GL_MAP_COHERENT_BIT
# if defined MapCoherent
#  pragma push_macro("MapCoherent")
#  undef MapCoherent
	 , MapCoherent(_base())
#  pragma pop_macro("MapCoherent")
# else
	 , MapCoherent(_base())
# endif
#endif
#if defined GL_DYNAMIC_STORAGE_BIT
# if defined DynamicStorage
#  pragma push_macro("DynamicStorage")
#  undef DynamicStorage
	 , DynamicStorage(_base())
#  pragma pop_macro("DynamicStorage")
# else
	 , DynamicStorage(_base())
# endif
#endif
#if defined GL_CLIENT_STORAGE_BIT
# if defined ClientStorage
#  pragma push_macro("ClientStorage")
#  undef ClientStorage
	 , ClientStorage(_base())
#  pragma pop_macro("ClientStorage")
# else
	 , ClientStorage(_base())
# endif
#endif
#if defined GL_SPARSE_STORAGE_BIT_ARB
# if defined SparseStorage
#  pragma push_macro("SparseStorage")
#  undef SparseStorage
	 , SparseStorage(_base())
#  pragma pop_macro("SparseStorage")
# else
	 , SparseStorage(_base())
# endif
#endif
	{ }
};

} // namespace enums

