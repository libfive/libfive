/**
 *  .file oglplus/utils/xml.hpp
 *  .brief Simple XML output utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifndef __OGLPLUS_UTILS_XML_1310290816_HPP
#define __OGLPLUS_UTILS_XML_1310290816_HPP

#include <iosfwd>
#include <cstddef>

namespace oglplus {
namespace aux {

std::ostream& xml_text_to_stream(
	const char* text,
	std::size_t length,
	std::ostream& output
);

} // namespace aux
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/utils/xml.ipp>
#endif // OGLPLUS_LINK_LIB

#endif
