
cmake_minimum_required( VERSION 3.0.2 )

file( STRINGS configure.ac line LIMIT_COUNT 1 REGEX "AC_INIT\\(.*\\)" )
if( line MATCHES "AC_INIT\\(\\[(.*)\\],[ \t]*\\[(.*)\\],[ \t]*\\[(.*)\\]\\)" )
	set( PACKAGE_NAME ${CMAKE_MATCH_1} )
	set( PACKAGE_VERSION ${CMAKE_MATCH_2} )
	set( PACKAGE_BUGREPORT ${CMAKE_MATCH_3} )
endif()

project( ${PACKAGE_NAME} )

set( SWIG_CXX ${CMAKE_CXX_COMPILER_ID} )
set( SWIG_PLATFORM ${CMAKE_SYSTEM_PROCESSOR}-${CMAKE_SYSTEM} )
set( SWIG_LIB ${PROJECT_SOURCE_DIR}/Lib )
set( SWIG_LIB_WIN_UNIX ${SWIG_LIB} )

include( CheckTypeSize )
CHECK_TYPE_SIZE( bool BOOL BUILTIN_TYPES_ONLY LANGUAGE CXX )

include( CheckIncludeFile )
CHECK_INCLUDE_FILE( stdlib.h HAVE_STDLIB_H )
CHECK_INCLUDE_FILE( strings.h HAVE_STRINGS_H )
CHECK_INCLUDE_FILE( string.h HAVE_STRING_H )

if( MSVC )
	set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /D_CRT_SECURE_NO_WARNINGS" )
	set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D_CRT_SECURE_NO_WARNINGS" )
	set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4800" ) # conversion to bool
endif()

add_subdirectory( Source )
