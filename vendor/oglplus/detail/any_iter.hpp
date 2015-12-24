/**
 *  @file oglplus/detail/any_iter.hpp
 *  @brief Type erasure for input iterators
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_ANY_ITER_1302042006_HPP
#define OGLPLUS_AUX_ANY_ITER_1302042006_HPP

#include <cassert>
#include <string>

namespace oglplus {
namespace aux {

template <typename T>
class AnyInputIter
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual _intf* _clone(void) const = 0;

		virtual const T& _deref(void) const = 0;

		virtual void _incr(void) = 0;

		virtual bool _equal(const _intf* that) const = 0;

		virtual std::ptrdiff_t _dist(const _intf* that) const = 0;
	};

	template <typename Iter>
	class _impl : public _intf
	{
	private:
		Iter _iter;

		static const T& _conv(const T& val)
		{
			return val;
		}

		template <typename U>
		static typename std::enable_if<
			std::is_same<T, const char*>::value &&
			std::is_same<U, std::string>::value,
			const char*
		>::type _conv(const U& val)
		{
			return val.c_str();
		}
	public:
		_impl(Iter iter)
		 : _iter(iter)
		{ }

		_intf* _clone(void) const
		OGLPLUS_OVERRIDE
		{
			return new _impl(_iter);
		}

		const T& _deref(void) const
		OGLPLUS_OVERRIDE
		{
			return _conv(*_iter);
		}

		void _incr(void)
		OGLPLUS_OVERRIDE
		{
			++_iter;
		}

		bool _equal(const _intf* that) const
		OGLPLUS_OVERRIDE
		{
			const _impl* i = dynamic_cast<const _impl*>(that);
			assert(i != nullptr);
			return _iter == i->_iter;
		}

		std::ptrdiff_t _dist(const _intf* that) const
		OGLPLUS_OVERRIDE
		{
			const _impl* i = dynamic_cast<const _impl*>(that);
			assert(i != nullptr);
			return std::distance(_iter, i->_iter);
		}
	};

	_intf* _pimpl;

	_intf* _clone(void) const
	{
		assert(_pimpl != nullptr);
		return _pimpl->_clone();
	}

	AnyInputIter(_intf* pimpl)
	 : _pimpl(pimpl)
	{
		assert(_pimpl != nullptr);
	}
public:
	typedef T value_type;
	typedef T* pointer;
	typedef T& reference;
	typedef const T& const_reference;
	typedef std::ptrdiff_t difference_type;
	typedef std::input_iterator_tag iterator_category;

	template <typename Iter>
	AnyInputIter(Iter i)
	 : _pimpl(new _impl<Iter>(i))
	{ }

	AnyInputIter(const AnyInputIter& that)
	 : _pimpl(that._clone())
	{ }

	AnyInputIter(AnyInputIter&& tmp)
	 : _pimpl(tmp._pimpl)
	{
		tmp._pimpl = nullptr;
	}

	~AnyInputIter(void)
	{
		if(_pimpl) delete _pimpl;
	}

	AnyInputIter& operator = (const AnyInputIter& that)
	{
		if(this != &that)
		{
			_intf* tmp = that._clone();
			if(_pimpl) delete _pimpl;
			_pimpl = tmp;
		}
		return *this;
	}

	AnyInputIter& operator = (AnyInputIter&& tmp)
	{
		if(this != &tmp)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = tmp._pimpl;
			tmp._pimpl = nullptr;
		}
		return *this;
	}

	const T& operator * (void) const
	{
		assert(_pimpl != nullptr);
		return _pimpl->_deref();
	}

	const T* operator -> (void) const
	{
		assert(_pimpl != nullptr);
		return &_pimpl->_deref();
	}

	AnyInputIter& operator ++ (void)
	{
		assert(_pimpl != nullptr);
		_pimpl->_incr();
		return *this;
	}

	AnyInputIter operator ++ (int)
	{
		AnyInputIter copy(_clone());
		assert(_pimpl != nullptr);
		_pimpl->incr();
		return std::move(copy);
	}

	friend bool operator == (const AnyInputIter& a, const AnyInputIter& b)
	{
		assert(a._pimpl != nullptr);
		assert(b._pimpl != nullptr);
		return a._pimpl->_equal(b._pimpl);
	}

	friend bool operator != (const AnyInputIter& a, const AnyInputIter& b)
	{
		assert(a._pimpl != nullptr);
		assert(b._pimpl != nullptr);
		return !a._pimpl->_equal(b._pimpl);
	}

	friend std::ptrdiff_t operator - (
		const AnyInputIter& a,
		const AnyInputIter& b
	)
	{
		assert(a._pimpl != nullptr);
		assert(b._pimpl != nullptr);
		return b._pimpl->_dist(a._pimpl);
	}
};

template <typename T>
std::ptrdiff_t distance(const AnyInputIter<T>& from, const AnyInputIter<T>& to)
{
	return to - from;
}

} // aux
} // oglplus

#endif // include guard
