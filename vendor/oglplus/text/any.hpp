/**
 *  @file oglplus/text/any.hpp
 *  @brief A type erasure for various implementation of text rendering
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_ANY_HPP
#define OGLPLUS_TEXT_ANY_HPP

#include <oglplus/text/unicode.hpp>

#include <memory>
#include <cassert>

namespace oglplus {
namespace text {

class AnyRenderer;
class AnyRendering;

class AnyFont
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual _intf* _clone(void) const = 0;
		virtual const void* _addr(void) = 0;

		virtual Rectangle GlyphLogicalMetrics(
			CodePoint code_point
		) = 0;

		virtual GLfloat QueryXOffsets(
			const CodePoint* cps,
			GLsizei size,
			std::vector<GLfloat>& x_offsets
		) = 0;
	};

	template <class Font>
	class _impl : public _intf
	{
	private:
		Font _font;
	public:
		_impl(const Font& font)
		 : _font(font)
		{ }

		_intf* _clone(void) const
		OGLPLUS_OVERRIDE
		{
			return new _impl(_font);
		}

		const void* _addr(void)
		OGLPLUS_OVERRIDE
		{
			return static_cast<void*>(&_font);
		}

		Rectangle GlyphLogicalMetrics(
			CodePoint code_point
		) OGLPLUS_OVERRIDE
		{
			return _font.GlyphLogicalMetrics(code_point);
		}

		GLfloat QueryXOffsets(
			const CodePoint* cps,
			GLsizei size,
			std::vector<GLfloat>& x_offsets
		) OGLPLUS_OVERRIDE
		{
			return _font.QueryXOffsets(cps, size, x_offsets);
		}
	};

	_intf* _pimpl;

	_intf* _clone(void) const
	{
		assert(_pimpl);
		return _pimpl->_clone();
	}

	template <class Font>
	AnyFont(const Font& font)
	 : _pimpl(new _impl<Font>(font))
	{ }

	friend class AnyRendering;
public:
	AnyFont(const AnyFont& that)
	 : _pimpl(that._clone())
	{ }

	AnyFont(AnyFont&& tmp)
	 : _pimpl(tmp._pimpl)
	{
		tmp._pimpl = nullptr;
	}

	~AnyFont(void)
	{
		if(_pimpl) delete _pimpl;
	}

	AnyFont& operator = (const AnyFont& that)
	{
		if(this != &that)
		{
			_intf* tmp = that._clone();
			if(_pimpl) delete _pimpl;
			_pimpl = tmp;
		}
		return *this;
	}

	AnyFont& operator = (AnyFont&& tmp)
	{
		if(this != &tmp)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = tmp._pimpl;
			tmp._pimpl = nullptr;
		}
		return *this;
	}

	template <class Font>
	const Font& As(void) const
	{
		assert(_pimpl);
		const void* ptr = _pimpl->_addr();
		assert(ptr);
		return *static_cast<const Font*>(ptr);
	}

	Rectangle GlyphLogicalMetrics(CodePoint code_point)
	{
		assert(_pimpl);
		return _pimpl->GlyphLogicalMetrics(code_point);
	}

	GLfloat QueryXOffsets(
		const CodePoint* cps,
		SizeType size,
		std::vector<GLfloat>& x_offsets
	)
	{
		assert(_pimpl);
		return _pimpl->QueryXOffsets(cps, size, x_offsets);
	}
};


class AnyLayout
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual const void* _addr(void) = 0;

		virtual GLsizei Capacity(void) const = 0;
		virtual GLfloat Width(void) const = 0;

		virtual void Set(
			const CodePoint* code_points,
			const GLsizei length
		) = 0;
		virtual void Set(StrCRef string) = 0;
	};

	template <class Layout>
	class _impl : public _intf
	{
	private:
		Layout _layout;
	public:
		_impl(Layout&& layout)
		 : _layout(std::move(layout))
		{ }

		const void* _addr(void)
		OGLPLUS_OVERRIDE
		{
			return static_cast<void*>(&_layout);
		}

		GLsizei Capacity(void) const
		OGLPLUS_OVERRIDE
		{
			return _layout.Capacity();
		}

		GLfloat Width(void) const
		OGLPLUS_OVERRIDE
		{
			return _layout.Width();
		}

		void Set(
			const CodePoint* code_points,
			const GLsizei length
		) OGLPLUS_OVERRIDE
		{
			_layout.Set(code_points, length);
		}

		void Set(StrCRef str)
		OGLPLUS_OVERRIDE
		{
			_layout.Set(str);
		}
	};

	_intf* _pimpl;

	AnyLayout(_intf* pimpl)
	 : _pimpl(pimpl)
	{ }

	template <class Layout>
	AnyLayout(Layout&& layout)
	 : _pimpl(new _impl<Layout>(std::move(layout)))
	{ }

	friend class AnyRendering;
public:
	AnyLayout(AnyLayout&& tmp)
	 : _pimpl(tmp._pimpl)
	{
		tmp._pimpl = nullptr;
	}

	~AnyLayout(void)
	{
		if(_pimpl) delete _pimpl;
	}

	AnyLayout& operator = (AnyLayout&& tmp)
	{
		if(this != &tmp)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = tmp._pimpl;
			tmp._pimpl = nullptr;
		}
		return *this;
	}

	template <class Layout>
	const Layout& As(void) const
	{
		assert(_pimpl);
		const void* ptr = _pimpl->_addr();
		assert(ptr);
		return *static_cast<const Layout*>(ptr);
	}

	SizeType Capacity(void) const
	{
		assert(_pimpl);
		return MakeSizeType(_pimpl->Capacity(), std::nothrow);
	}

	GLfloat Width(void) const
	{
		assert(_pimpl);
		return _pimpl->Width();
	}

	void Set(const CodePoint* code_points, const GLsizei length)
	{
		assert(_pimpl);
		_pimpl->Set(code_points, length);
	}

	void Set(StrCRef str)
	{
		assert(_pimpl);
		_pimpl->Set(str);
	}
};


class AnyRenderer
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual void SetProjection(const Mat4f& projection_matrix) = 0;
		virtual void SetCamera(const Mat4f& camera_matrix) = 0;
		virtual void SetLayoutTransform(const Mat4f& layout_matrix) = 0;
		virtual void SetAlignment(Alignment alignment) = 0;
		virtual void SetDirection(Direction direction) = 0;
		virtual void Use(void) = 0;
		virtual void Render(const AnyLayout& layout) = 0;
	};

	template <class Renderer, class Layout>
	class _impl : public _intf
	{
	private:
		Renderer _renderer;
	public:
		_impl(Renderer&& renderer)
		 : _renderer(std::move(renderer))
		{ }

		void SetProjection(const Mat4f& projection_matrix)
		OGLPLUS_OVERRIDE
		{
			_renderer.SetProjection(projection_matrix);
		}

		void SetCamera(const Mat4f& camera_matrix)
		OGLPLUS_OVERRIDE
		{
			_renderer.SetCamera(camera_matrix);
		}

		void SetLayoutTransform(const Mat4f& layout_matrix)
		OGLPLUS_OVERRIDE
		{
			_renderer.SetLayoutTransform(layout_matrix);
		}

		void SetAlignment(Alignment alignment)
		OGLPLUS_OVERRIDE
		{
			_renderer.SetAlignment(alignment);
		}

		void SetDirection(Direction direction)
		OGLPLUS_OVERRIDE
		{
			_renderer.SetDirection(direction);
		}

		void Use(void)
		OGLPLUS_OVERRIDE
		{
			_renderer.Use();
		}

		void Render(const AnyLayout& layout)
		OGLPLUS_OVERRIDE
		{
			_renderer.Render(layout.As<Layout>());
		}
	};

	_intf* _pimpl;

	template <class Renderer, class Layout>
	AnyRenderer(Renderer&& renderer, Layout*)
	 : _pimpl(new _impl<Renderer, Layout>(std::move(renderer)))
	{ }

	friend class AnyRendering;
public:
	AnyRenderer(AnyRenderer&& tmp)
	 : _pimpl(tmp._pimpl)
	{
		tmp._pimpl = nullptr;
	}

	~AnyRenderer(void)
	{
		if(_pimpl) delete _pimpl;
	}

	AnyRenderer& operator = (AnyRenderer&& tmp)
	{
		if(this != &tmp)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = tmp._pimpl;
			tmp._pimpl = nullptr;
		}
		return *this;
	}

	void SetProjection(const Mat4f& projection_matrix)
	{
		assert(_pimpl);
		_pimpl->SetProjection(projection_matrix);
	}

	void SetCamera(const Mat4f& camera_matrix)
	{
		assert(_pimpl);
		_pimpl->SetCamera(camera_matrix);
	}

	void SetLayoutTransform(const Mat4f& layout_matrix)
	{
		assert(_pimpl);
		_pimpl->SetLayoutTransform(layout_matrix);
	}

	void SetAlignment(Alignment alignment)
	{
		assert(_pimpl);
		_pimpl->SetAlignment(alignment);
	}

	void SetDirection(Direction direction)
	{
		assert(_pimpl);
		_pimpl->SetDirection(direction);
	}

	void Use(void)
	{
		assert(_pimpl);
		_pimpl->Use();
	}

	void Render(const AnyLayout& layout)
	{
		assert(_pimpl);
		_pimpl->Render(layout);
	}
};

class AnyRendering
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual AnyFont LoadFont(
			const char* font_name
		) = 0;

		virtual AnyLayout MakeLayout(
			const AnyFont& font,
			GLsizei capacity
		) = 0;

		virtual AnyLayout MakeLayout(
			const AnyFont& font,
			StrCRef str
		) = 0;

		virtual AnyRenderer GetRenderer(
			const FragmentShader& shader
		) = 0;
	};

	template <class RenderingUtility>
	class _impl : public _intf
	{
	private:
		RenderingUtility _ru;
		typedef typename RenderingUtility::Renderer Renderer;
		typedef typename RenderingUtility::Layout Layout;
		typedef typename RenderingUtility::Font Font;
	public:
		_impl(RenderingUtility&& ru)
		 : _ru(std::move(ru))
		{ }

		AnyFont LoadFont(const char* font_name)
		OGLPLUS_OVERRIDE
		{
			return AnyFont(_ru.LoadFont(font_name));
		}

		AnyLayout MakeLayout(
			const AnyFont& font,
			GLsizei capacity
		) OGLPLUS_OVERRIDE
		{
			return AnyLayout(
				_ru.MakeLayout(
					font.As<Font>(),
					capacity
				)
			);
		}

		AnyLayout MakeLayout(
			const AnyFont& font,
			StrCRef str
		) OGLPLUS_OVERRIDE
		{
			return AnyLayout(
				_ru.MakeLayout(
					font.As<Font>(),
					str
				)
			);
		}

		AnyRenderer GetRenderer(const FragmentShader& shader)
		OGLPLUS_OVERRIDE
		{
			return AnyRenderer(
				_ru.GetRenderer(shader),
				(Layout*)nullptr
			);
		}
	};

	_intf* _pimpl;
public:
	template <class RenderingUtility>
	AnyRendering(RenderingUtility&& renderer)
	 : _pimpl(new _impl<RenderingUtility>(std::move(renderer)))
	{ }

	AnyRendering(AnyRendering&& tmp)
	 : _pimpl(tmp._pimpl)
	{
		tmp._pimpl = nullptr;
	}

	~AnyRendering(void)
	{
		if(_pimpl) delete _pimpl;
	}

	AnyRendering& operator=(AnyRendering&& tmp)
	{
		if(_pimpl) delete _pimpl;
		_pimpl = tmp._pimpl;
		tmp._pimpl = nullptr;
		return *this;
	}

	typedef AnyFont Font;

	Font LoadFont(const char* font_name)
	{
		assert(_pimpl);
		return _pimpl->LoadFont(font_name);
	}

	typedef AnyLayout Layout;

	Layout MakeLayout(const Font& font, GLsizei capacity)
	{
		assert(_pimpl);
		return _pimpl->MakeLayout(font, capacity);
	}

	Layout MakeLayout(const Font& font, StrCRef str)
	{
		assert(_pimpl);
		return _pimpl->MakeLayout(font, str);
	}

	typedef AnyRenderer Renderer;

	Renderer GetRenderer(const FragmentShader& fragment_shader)
	{
		assert(_pimpl);
		return _pimpl->GetRenderer(fragment_shader);
	}
};

typedef AnyRendering AnyTextRendering;

} // namespace text
} // namespace oglplus

#endif // include guard
