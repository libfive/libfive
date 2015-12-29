#!/bin/sh
cd ../../lib
ln -s libefsw.so.$1.$2.$3 libefsw.so.$1
ln -s libefsw.so.$1 libefsw.so

if [ "$4" == "strip-symbols" ]; then
	objcopy --only-keep-debug libefsw.so.$1.$2.$3 libefsw.debug
	objcopy --strip-debug libefsw.so.$1.$2.$3
fi
