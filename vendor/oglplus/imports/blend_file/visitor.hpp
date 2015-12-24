/**
 *  @file oglplus/imports/blend_file/utils.hpp
 *  @brief Base class for value visitor
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_VISITOR_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_VISITOR_1107121519_HPP

#include <oglplus/imports/blend_file/pointer.hpp>
#include <cstddef>
#include <string>

namespace oglplus {
namespace imports {

struct BlendFileVisitor
{
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	BlendFileVisitor(void) = default;
	BlendFileVisitor(const BlendFileVisitor&) = default;
	BlendFileVisitor(BlendFileVisitor&&) = default;
#else
	BlendFileVisitor(void)
	{ }
	BlendFileVisitor(const BlendFileVisitor&)
	{ }
	BlendFileVisitor(BlendFileVisitor&&)
	{ }
#endif

	virtual ~BlendFileVisitor(void)
	{ }

	virtual void VisitStr(const std::string&) = 0;

	virtual void VisitPtr(BlendFilePointer) = 0;
	virtual void VisitPPtr(BlendFilePointerToPointer) = 0;

	virtual void VisitDbl(double) = 0;
	virtual void VisitFlt(float v){ VisitDbl(v); }

	virtual void VisitU64(uint64_t) = 0;
	virtual void VisitU32(uint32_t v){ VisitU64(v); }
	virtual void VisitU16(uint16_t v){ VisitU64(v); }
	virtual void VisitU8(uint8_t v){ VisitU64(v); }

	virtual void VisitI64(int64_t) = 0;
	virtual void VisitI32(int32_t v){ VisitI64(v); }
	virtual void VisitI16(int16_t v){ VisitI64(v); }
	virtual void VisitI8(int8_t v){ VisitI64(v); }

	virtual void VisitChr(char) = 0;

	virtual void VisitRaw(const char*, std::size_t) = 0;
};

} // imports
} // oglplus

#endif // include guard
