/**
 *  .file oglplus/detail/glsl_source.ipp
 *  .brief Implementation of GLSLSource helpers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <stdexcept>
#include <cassert>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
StrCRefsGLSLSrcWrap::StrCRefsGLSLSrcWrap(
	AnyInputIter<StrCRef>&& i,
	AnyInputIter<StrCRef>&& e
)
{
	auto d = distance(i, e);

	assert(!(d < 0));

	_ptrs.resize(std::size_t(d));
	_sizes.resize(std::size_t(d));

	auto pptr = _ptrs.begin();
	auto psize = _sizes.begin();
	while(i != e)
	{
		assert(pptr != _ptrs.end());
		assert(psize != _sizes.end());
		*pptr = i->begin();
		*psize = int(i->size());
		++i;
		++pptr;
		++psize;
	}
	assert(_ptrs.size() == _sizes.size());
}

OGLPLUS_LIB_FUNC
void StrsGLSLSrcWrap::_init(void)
{
	for(std::size_t i=0, n=_storage.size(); i!=n; ++i)
	{
		_ptrs[i] = _storage[i].c_str();
		_sizes[i] = GLint(_storage[i].size());
	}
}

OGLPLUS_LIB_FUNC
StrsGLSLSrcWrap::StrsGLSLSrcWrap(
	AnyInputIter<String>&& i,
	AnyInputIter<String>&& e
): _storage(i, e)
 , _ptrs(_storage.size(), nullptr)
 , _sizes(_storage.size(), 0)
{
	_init();
}

OGLPLUS_LIB_FUNC
StrsGLSLSrcWrap::StrsGLSLSrcWrap(std::vector<String>&& storage)
 : _storage(std::move(storage))
 , _ptrs(_storage.size(), nullptr)
 , _sizes(_storage.size(), 0)
{
	_init();
}

OGLPLUS_LIB_FUNC
std::size_t InputStreamGLSLSrcWrap::_check_and_get_size(std::istream& in)
{
	static const std::size_t default_size = 1023;
	if(!in.good())
	{
		std::string msg("Failed to read GLSL input stream.");
		throw std::runtime_error(msg);
	}

	in.exceptions(std::ios::badbit);
	std::streampos begin = in.tellg();
	in.seekg(0, std::ios::end);

	if(in.good())
	{
		std::streampos end = in.tellg();
		if(in.good())
		{
			in.seekg(0, std::ios::beg);
			if(in.good())
			{
				assert(!(end < begin));
				return std::size_t(end - begin);
			}
		}
	}
	else
	{
		in.clear();
		return default_size;
	}

	in.clear();
	in.seekg(0, std::ios::beg);

	if(in.good())
	{
		return default_size;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
std::vector<GLchar> InputStreamGLSLSrcWrap::_read_data(
	std::istream& input,
	std::size_t size
)
{
	std::vector<GLchar> data;
	if(size > 0)
	{
		data.reserve(size+1);
		typedef std::istreambuf_iterator<GLchar> _iit;
		data.assign(_iit(input), _iit());
		data.push_back('\0');
	}
	return data;
}

OGLPLUS_LIB_FUNC
InputStreamGLSLSrcWrap::InputStreamGLSLSrcWrap(std::istream& input)
 : _storage(_read_data(input, _check_and_get_size(input)))
 , _pdata(_storage.data())
 , _size(GLint(_storage.size()))
{ }

OGLPLUS_LIB_FUNC
FileGLSLSrcWrapOpener::FileGLSLSrcWrapOpener(const char* path)
 : _file(path, std::ios::in)
{
	if(!_file.good())
	{
		std::string msg("Failed to open file '");
		msg.append(path);
		msg.append("' for reading.");
		throw std::runtime_error(msg);
	}
}

OGLPLUS_LIB_FUNC
FileGLSLSrcWrap::FileGLSLSrcWrap(const char* path)
 : FileGLSLSrcWrapOpener(path)
 , InputStreamGLSLSrcWrap(_file)
{
	_file.close();
}

} // namespace aux
} // namespace oglplus

