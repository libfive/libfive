/**
 *  @file oglplus/ext/KHR_debug.ipp
 *  @brief Implementation of the wrapper for KHR_debug
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/utils/xml.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

#if GL_KHR_debug

OGLPLUS_LIB_FUNC
void KHR_debug_UniqueEssence::
Call(const KHR_debug::CallbackData& data)
{
	if(GLsizei(buffer.capacity()) < data.length)
	{
		buffer.resize(data.length);
	}
	buffer.assign(data.message, data.length);
	if(already_done.find(buffer) == already_done.end())
	{
		already_done.insert(buffer);
		_callback(data);
	}
}

OGLPLUS_LIB_FUNC
KHR_debug_TreeEssence::
KHR_debug_TreeEssence(std::ostream& out)
 : dbgout(out)
{
	dbgout << "-+-[Begin]" << std::endl;
}

OGLPLUS_LIB_FUNC
KHR_debug_TreeEssence::
~KHR_debug_TreeEssence(void)
{
	dbgout << " `-[Done]" << std::endl;
}


OGLPLUS_LIB_FUNC
void KHR_debug_TreeEssence::
Call(const KHR_debug::CallbackData& data)
{
	dbgout << " |" << std::endl;
	dbgout << " +-+-[" << data.id << "] '" <<
		data.message << "'" << std::endl;
	dbgout << " | +---[source]   '" <<
		EnumValueName(data.source).c_str()  << "'" << std::endl;
	dbgout << " | +---[type]     '" <<
		EnumValueName(data.type).c_str()  << "'" << std::endl;
	dbgout << " | `---[severity] '" <<
		EnumValueName(data.severity).c_str()  << "'" << std::endl;
}

OGLPLUS_LIB_FUNC
KHR_debug_ToXMLEssence::
KHR_debug_ToXMLEssence(std::ostream& out)
 : dbgout(out)
{
	dbgout << "<?xml version='1.0' encoding='UTF-8'?>" << std::endl;
	dbgout << "<KHR_debug>" << std::endl;
}

OGLPLUS_LIB_FUNC
KHR_debug_ToXMLEssence::
~KHR_debug_ToXMLEssence(void)
{
	dbgout << "</KHR_debug>" << std::endl;
}


OGLPLUS_LIB_FUNC
void KHR_debug_ToXMLEssence::
Call(const KHR_debug::CallbackData& data)
{
	dbgout << "<entry>" << std::endl;
	dbgout << "<id>" << data.id << "</id>" << std::endl;
	dbgout
		<< "<message>";
	aux::xml_text_to_stream(data.message, data.length, dbgout)
		<< "</message>"
		<< std::endl;
	dbgout
		<< "<source>"
		<< EnumValueName(data.source).c_str()
		<< "</source>"
		<< std::endl;
	dbgout
		<< "<type>"
		<< EnumValueName(data.type).c_str()
		<< "</type>"
		<< std::endl;
	dbgout
		<< "<severity>"
		<< EnumValueName(data.severity).c_str()
		<< "</severity>"
		<< std::endl;

	dbgout << "</entry>" << std::endl;
}

#endif // KHR_debug

} // namespace oglplus

