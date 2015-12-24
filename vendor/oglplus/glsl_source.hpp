/**
 *  @file oglplus/glsl_source.hpp
 *  @brief Helper class storing source code in GLSL
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_GLSL_SOURCE_1207111232_HPP
#define OGLPLUS_GLSL_SOURCE_1207111232_HPP

#include <oglplus/glsl_string.hpp>
#include <oglplus/detail/glsl_source.hpp>
#include <memory>

namespace oglplus {

/// Class storing source code in GLSL
class GLSLSource
{
private:
	std::unique_ptr<aux::GLSLSourceWrapper> _impl;

	template <typename Impl, typename P1>
	static aux::GLSLSourceWrapper* make_impl(P1&& p1)
	{
		return new Impl(std::forward<P1>(p1));
	}

	template <typename Impl, typename P1, typename P2>
	static aux::GLSLSourceWrapper* make_impl(P1&& p1, P2&& p2)
	{
		return new Impl(
			std::forward<P1>(p1),
			std::forward<P2>(p2)
		);
	}

	GLSLSource(const GLSLSource&);
public:
	GLSLSource(GLSLSource&& tmp)
	 : _impl(std::move(tmp._impl))
	{ }

	explicit GLSLSource(const StrCRef& source)
	 : _impl(make_impl<aux::StrCRefGLSLSrcWrap>(source))
	{ }

	GLSLSource(const std::vector<StrCRef>& lits)
	 : _impl(make_impl<aux::StrCRefsGLSLSrcWrap>(
		lits.begin(),
		lits.end()
	))
	{ }

	template <size_t N>
	GLSLSource(const StrCRef (&lits)[N])
	 : _impl(make_impl<aux::StrCRefsGLSLSrcWrap>(
		lits,
		lits+N
	))
	{ }

	GLSLSource(const std::vector<String>& strs)
	 : _impl(make_impl<aux::StrsGLSLSrcWrap>(
		strs.begin(),
		strs.end()
	))
	{ }

#if !OGLPLUS_NO_INITIALIZER_LISTS
	GLSLSource(std::initializer_list<StrCRef> lits)
	 : _impl(make_impl<aux::StrCRefsGLSLSrcWrap>(
		lits.begin(),
		lits.end()
	))
	{ }

	GLSLSource(std::initializer_list<String> strs)
	 : _impl(make_impl<aux::StrsGLSLSrcWrap>(
		strs.begin(),
		strs.end()
	))
	{ }
#endif

	template <typename Head, typename Tail>
	GLSLSource(const StrCRefChainTpl<GLchar, Head, Tail>& source)
	 : _impl(make_impl<aux::StrGLSLSrcWrap>(source.str()))
	{ }

	explicit GLSLSource(const String& source)
	 : _impl(make_impl<aux::StrGLSLSrcWrap>(source))
	{ }

	explicit GLSLSource(String&& source)
	 : _impl(make_impl<aux::StrGLSLSrcWrap>(std::move(source)))
	{ }

	struct FromStream_ { };

	GLSLSource(std::istream& input, FromStream_)
	 : _impl(make_impl<aux::InputStreamGLSLSrcWrap>(input))
	{ }

	static GLSLSource FromStream(std::istream& input)
	{
		return GLSLSource(input, FromStream_());
	}

	struct FromFile_ { };

	GLSLSource(const char* path, FromFile_)
	 : _impl(make_impl<aux::FileGLSLSrcWrap>(path))
	{ }

	static GLSLSource FromFile(const char* path)
	{
		return GLSLSource(path, FromFile_());
	}

	static GLSLSource FromFile(const String& path)
	{
		return GLSLSource(path.c_str(), FromFile_());
	}

	/// Count of buffers storing the individual parts of the source
	GLsizei Count(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		assert(bool(_impl));
		return _impl->Count();
	}

	/// Pointers to the individual parts of the source
	const GLchar* const* Parts(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		assert(bool(_impl));
		return _impl->Parts();
	}

	/// Pointer to the lengths of the individual parts of the source
	GLint const * Lengths(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		assert(bool(_impl));
		return _impl->Lengths();
	}
};

} // namespace oglplus

#endif // include guard
