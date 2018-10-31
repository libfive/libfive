/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#if defined _MSC_VER && !defined FIVE_STATIC
#ifdef FIVE_DLL
#define FIVE_EXPORT __declspec(dllexport)
#else
#define FIVE_EXPORT __declspec(dllimport)
#endif
#else
#define FIVE_EXPORT
#endif
