#!/bin/sh

SRC="${RAYLIB_ALIRE_PREFIX}/raylib"
BUILDDIR="${RAYLIB_ALIRE_PREFIX}/build-raylib"
LIB="${BUILDDIR}/raylib/libraylib.a"

if [ ! -e ${LIB} ]; then
    mkdir ${BUILDDIR} || true
    cd ${BUILDDIR}
    cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_EXAMPLES=no ${SRC}
    make
fi

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${BUILDDIR}/raylib
