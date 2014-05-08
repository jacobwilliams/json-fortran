# Put common compiler compatibility check in a variable to be written out, rather than
# duplicating it across the build and install package-config files

set ( COMPILER_CONSISTENCY_CHECK
  "# Check that the correct compiler is in use. Mod files and object files/archives
# are NOT compatible across different Fortran compilers when modules are present
set ( ${PACKAGE_NAME}_Fortran_COMPILER_ID ${CMAKE_Fortran_COMPILER_ID} )
set ( ${PACKAGE_NAME}_COMPATIBLE_COMPILER TRUE )
if ( NOT (\"${CMAKE_Fortran_COMPILER_ID}\" MATCHES \"\${CMAKE_Fortran_COMPILER_ID}\") )
  message ( SEND_ERROR \"Incompatible Fortran compilers detected! ${PACKAGE_NAME} was compiled with the ${CMAKE_Fortran_COMPILER_ID} Fortran compiler, but the current project is trying to use the \${CMAKE_Fortran_COMPILER_ID} Fortran compiler! In general, Fortran modules and libraries can only link against other projects built using the same compiler.\" )
  set ( ${PACKAGE_NAME}_COMPATIBLE_COMPILER FALSE )
endif ( NOT (\"${CMAKE_Fortran_COMPILER_ID}\" MATCHES \"\${CMAKE_Fortran_COMPILER_ID}\") )" )
