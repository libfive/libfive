/**
 *  @file oglplus/client/viewport.hpp
 *  @brief Client current viewport setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_VIEWPORT_1412071213_HPP
#define OGLPLUS_CLIENT_VIEWPORT_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/viewport.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if GL_VERSION_4_1 || GL_ARB_viewport_array

class ViewportIndexed
 : public SettingStack<context::ViewportExtents, ViewportIndex>
{
private:
	static
	context::ViewportExtents _do_get(ViewportIndex index)
	{
		return context::ViewportState::Viewport(index);
	}

	static
	void _do_set(context::ViewportExtents vp, ViewportIndex index)
	{
		context::ViewportState::Viewport(index, vp);
	}
public:
	ViewportIndexed(ViewportIndex index)
	 : SettingStack<context::ViewportExtents, ViewportIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};


typedef SettingStackIndexed<
	ViewportIndexed,
	context::ViewportExtents,
	ViewportIndex
> Viewport;

class DepthRangeIndexed
 : public SettingStack<context::ViewportDepthRange, ViewportIndex>
{
private:
	static
	context::ViewportDepthRange _do_get(ViewportIndex index)
	{
		return context::ViewportState::DepthRange(index);
	}

	static
	void _do_set(context::ViewportDepthRange vdr, ViewportIndex index)
	{
		context::ViewportState::DepthRange(index, vdr);
	}
public:
	DepthRangeIndexed(ViewportIndex index)
	 : SettingStack<context::ViewportDepthRange, ViewportIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};

typedef SettingStackIndexed<
	DepthRangeIndexed,
	context::ViewportDepthRange,
	ViewportIndex
> DepthRange;

#else

class Viewport
 : public SettingStack<context::ViewportExtents, Nothing>
{
private:
	static
	context::ViewportExtents _do_get(Nothing)
	{
		return context::ViewportState::Viewport();
	}

	static
	void _do_set(context::ViewportExtents vp, Nothing)
	{
		context::ViewportState::Viewport(vp);
	}
public:
	Viewport(void)
	 : SettingStack<context::ViewportExtents, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

class DepthRange
 : public SettingStack<context::ViewportDepthRange, Nothing>
{
private:
	static
	context::ViewportDepthRange _do_get(Nothing)
	{
		return context::ViewportState::DepthRange();
	}

	static
	void _do_set(context::ViewportDepthRange vdr, Nothing)
	{
		context::ViewportState::DepthRange(vdr);
	}
public:
	DepthRange(void)
	 : SettingStack<context::ViewportDepthRange, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

#endif

} // namespace aux

class ViewportState
{
public:
	aux::Viewport Viewport;
	aux::DepthRange DepthRange;
};

} // namespace client
} // namespace oglplus

#endif // include guard
