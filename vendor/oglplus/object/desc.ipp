/**
 *  @file oglplus/object/desc.ipp
 *  @brief Implementation of Object string description
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace aux {

#if !OGLPLUS_NO_OBJECT_DESC

OGLPLUS_LIB_FUNC
::std::map<unsigned, std::string>& ObjectDescRegistryStorage(int id)
{
	static ::std::map<int, ::std::map<unsigned, std::string> > _maps;
	return _maps[id];
}

OGLPLUS_LIB_FUNC
void ObjectDescRegistryBase::_do_register_desc(
	_desc_map& storage,
	unsigned name,
	ObjectDesc&& desc
)
{
	assert(storage.find(name) == storage.end());
	storage.insert(
		_desc_map::value_type(
			name,
			desc.Release()
		)
	);
}

OGLPLUS_LIB_FUNC
void ObjectDescRegistryBase::_do_unregister_desc(
	_desc_map& storage,
	unsigned name
)
{
	if(!std::uncaught_exception())
	{
		auto pos = storage.find(name);
		if(pos != storage.end())
		{
			storage.erase(pos);
		}
	}
}

OGLPLUS_LIB_FUNC
const std::string& ObjectDescRegistryBase::_do_get_desc(
	_desc_map& storage,
	unsigned name
)
{
	auto pos = storage.find(name);
	if(pos != storage.end()) return pos->second;
	return EmptyStdString();
}

#endif // OGLPLUS_NO_OBJECT_DESC

} // namespace aux
} // namespace oglplus

