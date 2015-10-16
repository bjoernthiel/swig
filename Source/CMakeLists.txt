
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/Include/swigconfig.h.in
	${CMAKE_CURRENT_BINARY_DIR}/Include/swigconfig.h )

if( MSVC )
	set( ProgramFiles_x86 "ProgramFiles(x86)" )
	list( APPEND CMAKE_PROGRAM_PATH "$ENV{${ProgramFiles_x86}}/Git/bin" )
endif()
find_program( bison_EXECUTABLE bison )
add_custom_command(
	OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.c
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CParse/parser.y
	COMMAND ${bison_EXECUTABLE} 
		-o ${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.c 
		--defines=${CMAKE_CURRENT_BINARY_DIR}/Include/parser.h 
		${CMAKE_CURRENT_SOURCE_DIR}/CParse/parser.y )
set_source_files_properties( 
	${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.c
	${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.h
	PROPERTIES GENERATED TRUE )

set( headers
	CParse/cparse.h
	${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.h
	DOH/doh.h
	DOH/dohint.h
	Include/swigwarn.h
	Modules/swigmod.h
	Preprocessor/preprocessor.h
	Swig/swigwrap.h
	Swig/swig.h
	Swig/swigfile.h
	Swig/swigopt.h
	Swig/swigparm.h
	Swig/swigscan.h
	Swig/swigtree.h
	${CMAKE_CURRENT_BINARY_DIR}/Include/swigconfig.h
)

set( sources
	CParse/cscanner.c
	${CMAKE_CURRENT_BINARY_DIR}/CParse/parser.c
	CParse/templ.c
	CParse/util.c
	DOH/base.c
	DOH/file.c
	DOH/fio.c
	DOH/hash.c
	DOH/list.c
	DOH/memory.c
	DOH/string.c
	DOH/void.c
	Modules/allegrocl.cxx
	Modules/allocate.cxx
	Modules/browser.cxx
	Modules/cffi.cxx
	Modules/chicken.cxx
	Modules/clisp.cxx
	Modules/contract.cxx
	Modules/csharp.cxx
	Modules/d.cxx
	Modules/directors.cxx
	Modules/emit.cxx
	Modules/go.cxx
	Modules/guile.cxx
	Modules/java.cxx
	Modules/javascript.cxx
	Modules/lang.cxx
	Modules/lua.cxx
	Modules/main.cxx
	Modules/matlab.cxx
	Modules/modula3.cxx
	Modules/module.cxx
	Modules/mzscheme.cxx
	Modules/nested.cxx  
	Modules/ocaml.cxx
	Modules/octave.cxx
	Modules/overload.cxx
	Modules/perl5.cxx
	Modules/php.cxx
	Modules/pike.cxx
	Modules/python.cxx
	Modules/r.cxx
	Modules/ruby.cxx
	Modules/s-exp.cxx
	Modules/scilab.cxx
	Modules/swigmain.cxx
	Modules/tcl8.cxx
	Modules/typepass.cxx
	Modules/uffi.cxx
	Modules/utils.cxx
	Modules/xml.cxx
	Preprocessor/cpp.c
	Preprocessor/expr.c
	Swig/cwrap.c
	Swig/deprecate.c
	Swig/error.c
	Swig/extend.c
	Swig/fragment.c
	Swig/getopt.c
	Swig/include.c
	Swig/misc.c
	Swig/naming.c
	Swig/parms.c
	Swig/scanner.c
	Swig/stype.c
	Swig/symbol.c
	Swig/tree.c
	Swig/typeobj.c
	Swig/typemap.c
	Swig/typesys.c
	Swig/wrapfunc.c
)

add_executable( swig ${headers} ${sources} )
target_include_directories( swig
	PRIVATE
		${CMAKE_CURRENT_BINARY_DIR}/Include
		${CMAKE_CURRENT_BINARY_DIR}/CParse
		Include
		DOH
		CParse
		Preprocessor
		Swig
		Modules
)
