/**
 *  .file oglplus/string/utf8.ipp
 *  .brief Helper utf8-related tools
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>
#include <oglplus/assert.hpp>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
std::size_t UTF8BytesRequired(const UnicodeCP* cp_str, std::size_t len)
{
	std::size_t result = 0;
	for(std::size_t i=0; i!=len; ++i)
	{
		UnicodeCP cp = *cp_str++;
		if((cp & ~0x0000007Fu) == 0)
			result += 1;
		else if((cp & ~0x000007FFu) == 0)
			result += 2;
		else if((cp & ~0x0000FFFFu) == 0)
			result += 3;
		else if((cp & ~0x001FFFFFu) == 0)
			result += 4;
		else if((cp & ~0x03FFFFFFu) == 0)
			result += 5;
		else if((cp & ~0x7FFFFFFFu) == 0)
			result += 6;
		else OGLPLUS_ABORT("Invalid code point");
	}
	return result;
}

OGLPLUS_LIB_FUNC
void ConvertCodePointToUTF8(UnicodeCP cp, char* str, std::size_t& len)
{
	// 7-bits -> one byte
	if((cp & ~0x0000007Fu) == 0)
	{
		str[0] = char(cp);
		len = 1;
	}
	// 11-bits -> two bytes
	else if((cp & ~0x000007FFu) == 0)
	{
		str[0] = char(((cp & 0x000007C0u) >>  6) | 0xC0);
		str[1] = char(((cp & 0x0000003Fu) >>  0) | 0x80);
		len = 2;
	}
	// 16-bits -> three bytes
	else if((cp & ~0x0000FFFFu) == 0)
	{
		str[0] = char(((cp & 0x0000F000u) >> 12) | 0xE0);
		str[1] = char(((cp & 0x00000FC0u) >>  6) | 0x80);
		str[2] = char(((cp & 0x0000003Fu) >>  0) | 0x80);
		len = 3;
	}
	// 21-bits -> four bytes
	else if((cp & ~0x001FFFFFu) == 0)
	{
		str[0] = char(((cp & 0x001C0000u) >> 18) | 0xF0);
		str[1] = char(((cp & 0x0003F000u) >> 12) | 0x80);
		str[2] = char(((cp & 0x00000FC0u) >>  6) | 0x80);
		str[3] = char(((cp & 0x0000003Fu) >>  0) | 0x80);
		len = 4;
	}
	// 26-bits -> five bytes
	else if((cp & ~0x03FFFFFFu) == 0)
	{
		str[0] = char(((cp & 0x03000000u) >> 24) | 0xF8);
		str[1] = char(((cp & 0x00FC0000u) >> 18) | 0x80);
		str[2] = char(((cp & 0x0003F000u) >> 12) | 0x80);
		str[3] = char(((cp & 0x00000FC0u) >>  6) | 0x80);
		str[4] = char(((cp & 0x0000003Fu) >>  0) | 0x80);
		len = 5;
	}
	// 31-bits -> six bytes
	else if((cp & ~0x7FFFFFFFu) == 0)
	{
		str[0] = char(((cp & 0x40000000u) >> 30) | 0xFC);
		str[1] = char(((cp & 0x3F000000u) >> 24) | 0x80);
		str[2] = char(((cp & 0x00FC0000u) >> 18) | 0x80);
		str[3] = char(((cp & 0x0003F000u) >> 12) | 0x80);
		str[4] = char(((cp & 0x00000FC0u) >>  6) | 0x80);
		str[5] = char(((cp & 0x0000003Fu) >>  0) | 0x80);
		len = 6;
	}
	else OGLPLUS_ABORT("Invalid code point");
}

OGLPLUS_LIB_FUNC
void ConvertCodePointsToUTF8(
	const UnicodeCP* cps,
	std::size_t len,
	std::vector<char>& result
)
{
	std::size_t u8len = UTF8BytesRequired(cps, len);
	result.resize(u8len);
	char* cptr = result.data();
	std::size_t clen = 0;
	while(len)
	{
		assert(u8len >= clen);
		ConvertCodePointToUTF8(*cps, cptr, clen);
		assert(clen > 0);
		u8len -= clen;
		cptr += clen;
		len -= 1;
		cps += 1;
	}
	assert(u8len == 0);
}

OGLPLUS_LIB_FUNC
std::size_t CodePointsRequired(const char* str, std::size_t len)
{
	assert(sizeof(char) == sizeof(unsigned char));
	const unsigned char* pb=reinterpret_cast<const unsigned char*>(str);

	std::size_t result = 0;

	while(len)
	{
		std::size_t skip = 0;
		if(((*pb) & 0x80) == 0x00)
			skip = 1;
		else if(((*pb) & 0xE0) == 0xC0)
			skip = 2;
		else if(((*pb) & 0xF0) == 0xE0)
			skip = 3;
		else if(((*pb) & 0xF8) == 0xF0)
			skip = 4;
		else if(((*pb) & 0xFC) == 0xF8)
			skip = 5;
		else if(((*pb) & 0xFE) == 0xFC)
			skip = 6;
		else OGLPLUS_ABORT("Invalid UTF8 sequence");
		assert(len >= skip);
		len -= skip;
		pb += skip;
		++result;
	}
	return result;
}

OGLPLUS_LIB_FUNC
UnicodeCP ConvertUTF8ToCodePoint(const char* str, std::size_t len, std::size_t& cp_len)
{
	assert(sizeof(char) == sizeof(unsigned char));
	const unsigned char* bytes=reinterpret_cast<const unsigned char*>(str);
	// 0xxxxxxx
	if((bytes[0] & 0x80) == 0x00)
	{
		assert(len >= 1);
		cp_len = 1;
		return UnicodeCP(bytes[0]);
	}
	// 110xxxxx
	else if((bytes[0] & 0xE0) == 0xC0)
	{
		// but not 11000000
		assert(bytes[0] != 0xC0);
		assert(len >= 2);
		cp_len = 2;
		return UnicodeCP(
			(((bytes[0] & ~0xE0u) <<  6) & 0x00000FC0u)|
			(((bytes[1] & ~0xC0u) <<  0) & 0x0000003Fu)
		);
	}
	// 1110xxxx
	else if((bytes[0] & 0xF0) == 0xE0)
	{
		// but not 11100000
		assert(bytes[0] != 0xE0);
		assert(len >= 3);
		cp_len = 3;
		return UnicodeCP(
			(((bytes[0] & ~0xF0u) << 12) & 0x0003F000u)|
			(((bytes[1] & ~0xC0u) <<  6) & 0x00000FC0u)|
			(((bytes[2] & ~0xC0u) <<  0) & 0x0000003Fu)
		);
	}
	// 11110xxx
	else if((bytes[0] & 0xF8) == 0xF0)
	{
		// but not 11110000
		assert(bytes[0] != 0xF0);
		assert(len >= 4);
		cp_len = 4;
		return UnicodeCP(
			(((bytes[0] & ~0xF8u) << 18) & 0x00FC0000u)|
			(((bytes[1] & ~0xC0u) << 12) & 0x0003F000u)|
			(((bytes[2] & ~0xC0u) <<  6) & 0x00000FC0u)|
			(((bytes[3] & ~0xC0u) <<  0) & 0x0000003Fu)
		);
	}
	// 111110xx
	else if((bytes[0] & 0xFC) == 0xF8)
	{
		// but not 11111000
		assert(bytes[0] != 0xF8);
		assert(len >= 5);
		cp_len = 5;
		return UnicodeCP(
			(((bytes[0] & ~0xFCu) << 24) & 0x3F000000u)|
			(((bytes[1] & ~0xC0u) << 18) & 0x00FC0000u)|
			(((bytes[2] & ~0xC0u) << 12) & 0x0003F000u)|
			(((bytes[3] & ~0xC0u) <<  6) & 0x00000FC0u)|
			(((bytes[4] & ~0xC0u) <<  0) & 0x0000003Fu)
		);
	}
	// 1111110x
	else if((bytes[0] & 0xFE) == 0xFC)
	{
		// but not 11111100
		assert(bytes[0] != 0xFC);
		assert(len >= 6);
		cp_len = 6;
		return UnicodeCP(
			(((bytes[0] & ~0xFEu) << 30) & 0xC0000000u)|
			(((bytes[1] & ~0xC0u) << 24) & 0x3F000000u)|
			(((bytes[2] & ~0xC0u) << 18) & 0x00FC0000u)|
			(((bytes[3] & ~0xC0u) << 12) & 0x0003F000u)|
			(((bytes[4] & ~0xC0u) <<  6) & 0x00000FC0u)|
			(((bytes[5] & ~0xC0u) <<  0) & 0x0000003Fu)
		);
	}
	OGLPLUS_ABORT("Invalid UTF8 sequence");
	return UnicodeCP();
}

OGLPLUS_LIB_FUNC
void ConvertUTF8ToCodePoints(
	const char* str,
	std::size_t len,
	std::vector<UnicodeCP>& result
)
{
	std::size_t ulen = CodePointsRequired(str, len);
	result.resize(ulen);
	UnicodeCP* cpptr = result.data();
	std::size_t cplen = 0;
	while(len)
	{
		*cpptr = ConvertUTF8ToCodePoint(str, len, cplen);
		++cpptr;
		assert(cplen > 0);
		assert(len >= cplen);
		len -= cplen;
		str += cplen;
		ulen -= 1;
	}
	assert(ulen == 0);
}

OGLPLUS_LIB_FUNC
bool UTF8Validator::_is_valid_ptr(const char* _s)
{
	return _s != nullptr;
}

OGLPLUS_LIB_FUNC
unsigned char UTF8Validator::byte(const char* _i)
{
	assert(_is_valid_ptr(_i));
	return static_cast<unsigned char>(*_i);
}

OGLPLUS_LIB_FUNC
const char* UTF8Validator::_validate(const char* _s, const char* _end)
{
	unsigned short bytes = 0;
	assert(_is_valid_ptr(_s));
	while((_s != _end) && (byte(_s) != 0x00))
	{
		// there are remaining bytes in the sequence
		if(bytes)
		{
			// the byte must be 10xxxxxx
			if((byte(_s) & 0xC0) != 0x80)
				return nullptr;
			--bytes;
		}
		// this is _a beginning of a new sequence
		else if(byte(_s) & 0x80)
		{
			// 110xxxxx
			if((byte(_s) & 0xE0) == 0xC0)
			{
				// but not 11000000
				if(byte(_s) == 0xC0)
					return nullptr;
				bytes = 1;
			}
			// 1110xxxx
			else if((byte(_s) & 0xF0) == 0xE0)
			{
				// but not 11100000
				if(byte(_s) == 0xE0)
					return nullptr;
				bytes = 2;
			}
			// 11110xxx
			else if((byte(_s) & 0xF8) == 0xF0)
			{
				// but not 11110000
				if(byte(_s) == 0xF0)
					return nullptr;
				bytes = 3;
			}
			// 111110xx
			else if((byte(_s) & 0xFC) == 0xF8)
			{
				// but not 11111000
				if(byte(_s) == 0xF8)
					return nullptr;
				bytes = 4;
			}
			// 1111110x
			else if((byte(_s) & 0xFE) == 0xFC)
			{
				// but not 11111100
				if(byte(_s) == 0xFC)
					return nullptr;
				bytes = 5;
			}
		}
		++_s;
	}
	return _s;
}

} // namespace aux
} // namespace oglplus

