/**
 *  @file oglplus/error/object.hpp
 *  @brief Declaration of OGLplus object-related error
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ERROR_OBJECT_1107121317_HPP
#define OGLPLUS_ERROR_OBJECT_1107121317_HPP

#include <oglplus/error/basic.hpp>
#include <oglplus/object/tags.hpp>
#include <oglplus/object/type.hpp>
#include <oglplus/object/name.hpp>

namespace oglplus {

/// Exception class for GL object-related errors
class ObjectError
 : public Error
{
private:
#if !OGLPLUS_ERROR_NO_OBJECT_TYPE
	GLenum _obj_type;
#endif
#if !OGLPLUS_ERROR_NO_BIND_TARGET
	GLenum _bind_tgt;
#endif
#if !OGLPLUS_ERROR_NO_TARGET_NAME
	const char* _tgt_name;
#endif
	int _obj_typeid;
	GLuint _obj_name;
public:
	ObjectError(const char* message);

	ObjectError& ObjectType(oglplus::ObjectType obj_type)
	{
#if !OGLPLUS_ERROR_NO_OBJECT_TYPE
		_obj_type = GLenum(obj_type);
#endif
		(void)obj_type;
		return *this;
	}

	/// Returns the object type
	GLenum ObjectType(void) const
	OGLPLUS_OVERRIDE;

	/// Returns the class name
	const char* ObjectTypeName(void) const
	OGLPLUS_OVERRIDE;

	template <typename BindTarget_>
	ObjectError& BindTarget(BindTarget_ bind_tgt)
	{
#if !OGLPLUS_ERROR_NO_BIND_TARGET
		_bind_tgt = GLenum(bind_tgt);
#endif
#if !OGLPLUS_ERROR_NO_TARGET_NAME
		_tgt_name = EnumValueName(bind_tgt).c_str();
#endif
		(void)bind_tgt;
		return *this;
	}

	/// Returns the bind target
	GLenum BindTarget(void) const
	OGLPLUS_OVERRIDE;

	/// Returns the bind target name
	const char* TargetName(void) const
	OGLPLUS_OVERRIDE;

	template <typename ObjTag>
	ObjectError& Object(oglplus::ObjectName<ObjTag> object)
	{
#if !OGLPLUS_ERROR_NO_OBJECT_TYPE
		_obj_type = GLenum(ObjTypeOps<ObjTag>::ObjectType());
#endif
		_obj_typeid = ObjTag::value;
		_obj_name = GetGLName(object);
		return *this;
	}

	/// Object GL name
	GLint ObjectName(void) const
	OGLPLUS_OVERRIDE;

	/// Object textual description
	const String& ObjectDesc(void) const
	OGLPLUS_OVERRIDE;

	template <typename BindTarget_>
	ObjectError& ObjectBinding(BindTarget_ bind_tgt)
	{
		typedef typename ObjectTargetTag<BindTarget_>::Type Tag;
		Object(ObjBindingOps<Tag>::Binding(bind_tgt));
		return BindTarget(bind_tgt);
	}

	template <typename BindTarget_>
	ObjectError& ObjectBinding(BindTarget_ bind_tgt, GLuint index)
	{
		typedef typename ObjectTargetTag<BindTarget_>::Type Tag;
		Object(ObjBindingOps<Tag>::Binding(bind_tgt, index));
		return BindTarget(bind_tgt);
	}
};

class ObjectPairError
 : public ObjectError
{
private:
#if !OGLPLUS_ERROR_NO_OBJECT_TYPE
	GLenum _sub_type;
#endif
	int _sub_typeid;
	GLuint _sub_name;
public:
	ObjectPairError(const char* message);

	ObjectPairError& SubjectType(oglplus::ObjectType sub_type)
	{
#if !OGLPLUS_ERROR_NO_OBJECT_TYPE
		_sub_type = GLenum(sub_type);
#endif
		(void)sub_type;
		return *this;
	}

	/// Returns the subject type
	GLenum SubjectType(void) const
	OGLPLUS_OVERRIDE;

	/// Returns the subject class name
	const char* SubjectTypeName(void) const
	OGLPLUS_OVERRIDE;

	template <typename ObjTag>
	ObjectPairError& Subject(oglplus::ObjectName<ObjTag> subject)
	{
		_sub_typeid = ObjTag::value;
		_sub_name = GetGLName(subject);
		return *this;
	}

	/// Subject GL name
	GLint SubjectName(void) const
	OGLPLUS_OVERRIDE;

	/// Object textual description
	const String& SubjectDesc(void) const
	OGLPLUS_OVERRIDE;

	template <typename BindTarget_>
	ObjectPairError& SubjectBinding(BindTarget_ bind_tgt)
	{
		typedef typename ObjectTargetTag<BindTarget_>::Type Tag;
		return Subject(ObjBindingOps<Tag>::Binding(bind_tgt));
	}
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/error/object.ipp>
#endif

#endif // include guard
