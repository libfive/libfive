/**
 *  .file oglplus/string/utf8.hpp
 *  .brief Helper utf8-related tools
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifndef OGLPLUS_STRING_UTF8_1102101236_HPP
#define OGLPLUS_STRING_UTF8_1102101236_HPP

#include <oglplus/config/compiler.hpp>
#include <cstddef>
#include <cassert>
#include <vector>

namespace oglplus {
namespace aux {

typedef char32_t UnicodeCP;

std::size_t UTF8BytesRequired(const UnicodeCP* cp_str, std::size_t len);

void ConvertCodePointToUTF8(UnicodeCP cp, char* str, std::size_t& len);

void ConvertCodePointsToUTF8(
	const UnicodeCP* cps,
	std::size_t len,
	std::vector<char>& result
);

std::size_t CodePointsRequired(const char* str, std::size_t len);

UnicodeCP ConvertUTF8ToCodePoint(
	const char* str,
	std::size_t len,
	std::size_t& cp_len
);

void ConvertUTF8ToCodePoints(
	const char* str,
	std::size_t len,
	std::vector<UnicodeCP>& result
);

class UTF8Validator
{
protected:
	static bool _is_valid_ptr(const char* _s);
	static unsigned char byte(const char* _i);

	template <int N>
	static void _check_seq_tail(const char* _s)
	{
		for(int i=1; i!=N; ++i)
		{
			assert(byte(_s) != 0x00);
			assert(byte(_s) != 0x80);
			++_s;
		}
	}

	// Validates the utf8 string, returns _end or nullptr
	static const char* _validate(const char* _s, const char* _end);
public:
	bool operator()(const char* begin, const char* end) const
	{
		return _validate(begin, end) == end;
	}
};

inline bool ValidUTF8(const char* begin, const char* end)
{
#if !defined(OGLPLUS_NO_UTF8_CHECKS) || !OGLPLUS_NO_UTF8_CHECKS
	UTF8Validator valid_utf8;
	return valid_utf8(begin, end);
#else
	OGLPLUS_FAKE_USE(begin);
	OGLPLUS_FAKE_USE(end);
	return true;
#endif
}

} // namespace aux
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/string/utf8.ipp>
#endif // OGLPLUS_LINK_LIB

#endif
