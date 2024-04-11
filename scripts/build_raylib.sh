#!/bin/sh

set -e

CONFIG="${RAYLIB_ALIRE_PREFIX}/config/raylib_config.gpr"
SRC="${RAYLIB_ALIRE_PREFIX}/raylib"
BUILDDIR="${RAYLIB_ALIRE_PREFIX}/build-raylib"
LIB="${BUILDDIR}/raylib/libraylib.a"

ENABLED=$(cat ${CONFIG} | sed -n 's/[ ]*ENABLE_RAYLIB_BUILD := "\(True\)";/\1/p')
BUILD_MODE=$(cat ${CONFIG} | sed -n 's/[ ]*Build_Profile : Build_Profile_Kind := "\([a-z]*\)";/\1/p')

if [ x${BUILD_MODE} = "xdevelopment" ]; then
    RAYLIB_BUILD_TYPE=Debug
else
    RAYLIB_BUILD_TYPE=Release
fi

if [ x${ENABLED} = "xTrue" ]; then
    echo "Building raylib... (${RAYLIB_BUILD_TYPE})"
    if [ ! -e ${BUILDDIR} ]; then
        mkdir ${BUILDDIR} || true
        cd ${BUILDDIR}
        cmake -DCMAKE_BUILD_TYPE=${RAYLIB_BUILD_TYPE} -DBUILD_EXAMPLES=no ${SRC}
    else
        cd ${BUILDDIR}
    fi
    make
    echo "Raylib build done."
else
    echo "Raylib build disabled."
fi
