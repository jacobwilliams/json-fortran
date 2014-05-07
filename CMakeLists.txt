# CMake Configuration and build added by Izaak Beekman -- May, 2014

# Copy right (c) 2014, Izaak Beekman
# All rights reserved.

# This file is contributed to the json-fortran project, and
# is licensed under the terms of json-fortran license. The json-fortran
# license is located in the LICENSE file which must be distributed with
# this software. The contributing author, Izaak Beekman, retains all
# rights permitted by the terms of the json-fortran license.

cmake_minimum_required ( VERSION 2.8 FATAL_ERROR )

# Set the type/configuration of build to perform
set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "MinSizeRel" "RelWithDebInfo" )
set ( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )
set_property ( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES} )


enable_language ( Fortran )
include ( "cmake/pickFortranCompilerFlags.cmake" )

# Check for in-source builds and error out if found
# Provides an advanced option to allow in source builds
include ( "cmake/checkOutOfSource.cmake" )

#---------------------
# Declare project name
#---------------------
project ( jsonfortran NONE )

#----------------------------------
# Set version (semantic versioning)
# C.F. semver.org
#----------------------------------
set ( VERSION_MAJOR 1 )
set ( VERSION_MINOR 0 )
set ( VERSION_PATCH 0 )
set ( VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )

set ( PROJ_VERSION "${CMAKE_PROJECT_NAME}-${VERSION}" )

#-------------------------------------
# Collect source files for the library
#-------------------------------------
set ( JF_LIB_SRCS src/json_module.f90 )

#-----------------------------------------
# Collect all the mod files into their own
# directory to ease installation issues
#-----------------------------------------
# No standard mechanism or location for compiled .mod files
# but often they are thought of as similar to precompiled 
# header files. Let's include the compiler ID to ease
# compatibility issues for thos linking against the installed
# libraries.
set ( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include" )

#-------------------------------------
# Define where our files get installed
#-------------------------------------
# Most of this is probably 'wrong' for Windows/Cygwin

#if ( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )
#  include ( GNUInstallDirs ) # Standard CMake module
if ( UNIX ) #Apple, BSD, solaris, other *NIX? Framework on Apple instead? 
  set ( CMAKE_INSTALL_LIBDIR      lib     )
  set ( CMAKE_INSTALL_INCLUDEDIR  include )
  set ( CMAKE_INSTALL_DATAROOTDIR share   )
endif ( UNIX )
#endif ( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )

# Set the package name to be specific to the compiler used, so that
# versions compiled with different compilers can be installed in parallel
string ( TOLOWER ${CMAKE_PROJECT_NAME}-${CMAKE_Fortran_COMPILER_ID} PACKAGE_NAME )
string ( TOLOWER fortran/${CMAKE_Fortran_COMPILER_ID}-compiler FCOMPILER_DIR )

if ( "${CMAKE_SYSTEM_NAME}" MATCHES "Darwin" )
  set ( ENABLE_DYLIBS_USE_RPATH TRUE CACHE BOOL
    "Enable @rpath install name for dylibs" )
  mark_as_advanced ( ENABLE_DYLIBS_USE_RPATH )
endif ( "${CMAKE_SYSTEM_NAME}" MATCHES "Darwin" )

if ( ENABLE_DYLIBS_USE_RPATH )
  message ( WARNING
    "NOTICE: Currently CMake does not support installing @rpath install name dylibs using DESTDIR. If you would like to be able to install using 'DESTDIR=/install/path/root' set ENABLE_DYLIBS_USE_RPATH to FALSE." )
  set ( CMAKE_MACOSX_RPATH TRUE )
else ( ENABLE_DYLIBS_USE_RPATH )
  set ( CMAKE_INSTALL_NAME_DIR
    "\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${FCOMPILER_DIR}" )
  set( CMAKE_MACOSX_RPATH FALSE )
endif ( ENABLE_DYLIBS_USE_RPATH )

#---------------------------------------------
# Build a shared and static library by default
#---------------------------------------------
  
set ( LIB_NAME ${CMAKE_PROJECT_NAME} )
add_library ( ${LIB_NAME}        SHARED ${JF_LIB_SRCS} )
add_library ( ${LIB_NAME}-static STATIC ${JF_LIB_SRCS} )
set_target_properties ( ${LIB_NAME}-static 
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  VERSION ${VERSION} )
set_target_properties ( ${LIB_NAME}
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR} 
  VERSION ${VERSION} )

#---------------------------------------------------------------------
# Add some tests to ensure that the software is performing as expected
#---------------------------------------------------------------------
# Not implemented yet

#-------------------------
# Perform the installation
#-------------------------

install ( TARGETS ${LIB_NAME} ${LIB_NAME}-static
  EXPORT ${PACKAGE_NAME}-targets
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCOMPILER_DIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCOMPILER_DIR}" )
#?ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FCOMPILER_DIR}/${CMAKE_PROJECT_NAME}-${VERSION}"

set( ABS_LIB_INSTALL_DIR "\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${FCOMPILER_DIR}" )
if ( NOT ENABLE_DYLIBS_USE_RPATH )
  if ( CMAKE_INSTALL_NAME_TOOL ) # On Mac and have install_name_tool
    install ( CODE "
       if ( DEFINED ENV{DESTDIR} )
         string ( REGEX REPLACE \"/$\" \"\" DESTDIR \"\$ENV{DESTDIR}\" ) # strip trailing /
         get_filename_component ( INSALL_LIB 
              \${DESTDIR}/${ABS_LIB_INSTALL_DIR}/lib${LIB_NAME}.${VERSION_MAJOR}.${VERSION_MINOR}.dylib
              ABSOLUTE )
         execute_process ( COMMAND \"${CMAKE_INSTALL_NAME_TOOL}\" 
            -id \"\${INSALL_LIB}\" \"\${INSALL_LIB}\" )
       endif ( DEFINED ENV{DESTDIR} )" )
  endif ( CMAKE_INSTALL_NAME_TOOL )
endif ( NOT ENABLE_DYLIBS_USE_RPATH )
  

set ( INSTALL_MOD_DIR "${CMAKE_INSTALL_INCLUDEDIR}/${FCOMPILER_DIR}/${PROJ_VERSION}" )
install ( DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}/" DESTINATION  "${INSTALL_MOD_DIR}" )

#------------------------------------------
# Add portable unistall command to makefile
#------------------------------------------
# Adapted from the CMake Wiki FAQ
configure_file ( "${CMAKE_SOURCE_DIR}/cmake/uninstall.cmake.in" "${CMAKE_BINARY_DIR}/uninstall.cmake"
    @ONLY)

add_custom_target ( uninstall
    COMMAND ${CMAKE_COMMAND} -P "${CMAKE_BINARY_DIR}/uninstall.cmake" )

#-----------------------------------------------------
# Publicize installed location to other CMake projects
#-----------------------------------------------------
set ( EXPORT_INSTALL_DIR 
  "${CMAKE_INSTALL_DATAROOTDIR}/cmake/${PACKAGE_NAME}-${VERSION}" )
install ( EXPORT ${PACKAGE_NAME}-targets DESTINATION "${EXPORT_INSTALL_DIR}" )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file( "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${VERSION}
  COMPATIBILITY SameMajorVersion )

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

# install package config file
configure_package_config_file (
  "${CMAKE_SOURCE_DIR}/cmake/pkg/${CMAKE_PROJECT_NAME}-config.cmake.in"
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  INSTALL_DESTINATION "${EXPORT_INSTALL_DIR}"
  PATH_VARS EXPORT_INSTALL_DIR INSTALL_MOD_DIR )

# Install the config and version files so that we can find this project with others
install ( FILES
  "${CMAKE_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  DESTINATION "${EXPORT_INSTALL_DIR}" )

#----------------------------------------------
# Make build tree targets accessible for import
#----------------------------------------------
export ( TARGETS ${LIB_NAME} ${LIB_NAME}-static FILE ${PACKAGE_NAME}-targets.cmake )

# build tree package config file, NOT installed
configure_file ( 
  "${CMAKE_SOURCE_DIR}/cmake/${CMAKE_PROJECT_NAME}-config.cmake.in"
  "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config.cmake"
  @ONLY )

set ( ENABLE_BUILD_TREE_EXPORT FALSE CACHE BOOL
  "Add the ${PACKAGE_NAME} build tree to the CMake package registry?" )
if ( ENABLE_BUILD_TREE_EXPORT )
  export ( PACKAGE ${PACKAGE_NAME} )
endif ( ENABLE_BUILD_TREE_EXPORT )
