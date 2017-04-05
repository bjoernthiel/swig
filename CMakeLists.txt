
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
	# static runtime
	foreach( language C CXX )
		foreach( flag
				CMAKE_${language}_FLAGS_DEBUG
				CMAKE_${language}_FLAGS_RELEASE
				CMAKE_${language}_FLAGS_RELWITHDEBINFO
				CMAKE_${language}_FLAGS_MINSIZEREL )
			if( ${flag} MATCHES "/MD" )
				string( REGEX REPLACE "/MD" "/MT" ${flag} "${${flag}}" )
			endif()
		endforeach()
	endforeach()
endif()

configure_file(
	${CMAKE_SOURCE_DIR}/Source/Include/swigconfig.h.in
	${CMAKE_BINARY_DIR}/Source/Include/swigconfig.h )

if( MSVC )
	list( APPEND CMAKE_PROGRAM_PATH C:/msys64/usr/bin )
endif()
find_program( bison_EXECUTABLE bison )
add_custom_command(
	OUTPUT ${CMAKE_BINARY_DIR}/Source/CParse/parser.c
	DEPENDS ${CMAKE_SOURCE_DIR}/Source/CParse/parser.y
	COMMAND ${bison_EXECUTABLE} 
		-o ${CMAKE_BINARY_DIR}/Source/CParse/parser.c 
		--defines=${CMAKE_BINARY_DIR}/Source/Include/parser.h 
		${CMAKE_SOURCE_DIR}/Source/CParse/parser.y )
set_source_files_properties( 
	${CMAKE_BINARY_DIR}/Source/CParse/parser.c
	${CMAKE_BINARY_DIR}/Source/CParse/parser.h
	PROPERTIES GENERATED TRUE )

set( headers
	Source/CParse/cparse.h
	${CMAKE_BINARY_DIR}/Source/CParse/parser.h
	Source/DOH/doh.h
	Source/DOH/dohint.h
	Source/Include/swigwarn.h
	Source/Modules/swigmod.h
	Source/Preprocessor/preprocessor.h
	Source/Swig/swigwrap.h
	Source/Swig/swig.h
	Source/Swig/swigfile.h
	Source/Swig/swigopt.h
	Source/Swig/swigparm.h
	Source/Swig/swigscan.h
	Source/Swig/swigtree.h
	${CMAKE_BINARY_DIR}/Source/Include/swigconfig.h
)

set( sources
	Source/CParse/cscanner.c
	${CMAKE_BINARY_DIR}/Source/CParse/parser.c
	Source/CParse/templ.c
	Source/CParse/util.c
	Source/DOH/base.c
	Source/DOH/file.c
	Source/DOH/fio.c
	Source/DOH/hash.c
	Source/DOH/list.c
	Source/DOH/memory.c
	Source/DOH/string.c
	Source/DOH/void.c
	Source/Modules/allegrocl.cxx
	Source/Modules/allocate.cxx
	Source/Modules/browser.cxx
	Source/Modules/cffi.cxx
	Source/Modules/chicken.cxx
	Source/Modules/clisp.cxx
	Source/Modules/contract.cxx
	Source/Modules/csharp.cxx
	Source/Modules/d.cxx
	Source/Modules/directors.cxx
	Source/Modules/emit.cxx
	Source/Modules/go.cxx
	Source/Modules/guile.cxx
	Source/Modules/java.cxx
	Source/Modules/javascript.cxx
	Source/Modules/lang.cxx
	Source/Modules/lua.cxx
	Source/Modules/main.cxx
	Source/Modules/matlab.cxx
	Source/Modules/modula3.cxx
	Source/Modules/module.cxx
	Source/Modules/mzscheme.cxx
	Source/Modules/nested.cxx  
	Source/Modules/ocaml.cxx
	Source/Modules/octave.cxx
	Source/Modules/overload.cxx
	Source/Modules/perl5.cxx
	Source/Modules/php.cxx
	Source/Modules/pike.cxx
	Source/Modules/python.cxx
	Source/Modules/r.cxx
	Source/Modules/ruby.cxx
	Source/Modules/s-exp.cxx
	Source/Modules/scilab.cxx
	Source/Modules/swigmain.cxx
	Source/Modules/tcl8.cxx
	Source/Modules/typepass.cxx
	Source/Modules/uffi.cxx
	Source/Modules/utils.cxx
	Source/Modules/xml.cxx
	Source/Preprocessor/cpp.c
	Source/Preprocessor/expr.c
	Source/Swig/cwrap.c
	Source/Swig/deprecate.c
	Source/Swig/error.c
	Source/Swig/extend.c
	Source/Swig/fragment.c
	Source/Swig/getopt.c
	Source/Swig/include.c
	Source/Swig/misc.c
	Source/Swig/naming.c
	Source/Swig/parms.c
	Source/Swig/scanner.c
	Source/Swig/stype.c
	Source/Swig/symbol.c
	Source/Swig/tree.c
	Source/Swig/typeobj.c
	Source/Swig/typemap.c
	Source/Swig/typesys.c
	Source/Swig/wrapfunc.c
)

add_executable( swig ${headers} ${sources} )
target_include_directories( swig
	PRIVATE
		${CMAKE_BINARY_DIR}/Source/Include
		${CMAKE_BINARY_DIR}/Source/CParse
		Source/Include
		Source/DOH
		Source/CParse
		Source/Preprocessor
		Source/Swig
		Source/Modules
)
