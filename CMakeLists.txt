# CMake Configuration and build added by Izaak Beekman -- April, 2014
#
# Copy right (c) 2014, Izaak Beekman
# All rights reserved.
# 
# This file is contributed to the json-fortran project, and
# is licensed under the terms of json-fortran license. The json-fortran
# license is located in the LICENSE file which must be distributed with
# this software. The contributing author, Izaak Beekman, retains all
# rights permitted by the terms of the json-fortran license.

cmake_minimum_required ( VERSION 2.8 FATAL_ERROR )

#include ( cmake/OutOfSourceCheck.cmake ) # default to out of source builds
#include ( cmake/BuildSettings.cmake ) # Release, RelWithDebInfo, etc.
enable_language ( Fortran )
#include ( cmake/CompilerCompatibilityCheck.cmake )

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
set ( version ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH} )

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
  VERSION ${version} )
set_target_properties ( ${LIB_NAME}
  PROPERTIES
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR} )

#-------------------------------------
# Define where our files get installed
#-------------------------------------
if ( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )
  include ( GNU_Install_Dirs ) # Standard CMake module
elseif ( UNIX ) #Apple, BSD, solaris, other *NIX? Framework on Apple instead? 
  set ( CMAKE_INSTALL_LIBDIR      lib     )
  set ( CMAKE_INSTALL_INCLUDEDIR  include )
  set ( CMAKE_INSTALL_DATAROOTDIR share   )
endif ( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )

install ( TARGETS ${LIB_NAME} ${LIB_NAME}-static
  EXPORT ${CMAKE_PROJECT_NAME}-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} ) # We may need something different for windows

set ( INSTALL_MOD_DIR "${CMAKE_INSTALL_INCLUDEDIR}/fortran/${CMAKE_Fortran_COMPILER_ID}" )
install ( DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION  "${INSTALL_MOD_DIR}" )

#-----------------------------------------------------
# Publicize installed location to other CMake projects
#-----------------------------------------------------
set ( EXPORT_INSTALL_DIR 
  ${CMAKE_INSTALL_DATAROOTDIR}/cmake/${CMAKE_PROJECT_NAME}-${version} )
install ( EXPORT ${CMAKE_PROJECT_NAME}-targets DESTINATION ${EXPORT_INSTALL_DIR} )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file( ${CMAKE_BINARY_DIR}/${CMAKE_PROJECT_NAME}-config-version.cmake 
  VERSION ${version}
  COMPATIBILITY SameMajorVersion )

# install package config file
configure_package_config_file (
  ${CMAKE_SOURCE_DIR}/cmake/pkg/${CMAKE_PROJECT_NAME}-config.cmake.in
  ${CMAKE_BINARY_DIR}/pkg/${CMAKE_PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${EXPORT_INSTALL_DIR}
  PATH_VARS EXPORT_INSTALL_DIR INSTALL_MOD_DIR )

# Install the config and version files so that we can find this project with others
install ( FILES
  ${CMAKE_BINARY_DIR}/pkg/${CMAKE_PROJECT_NAME}-config.cmake
  ${CMAKE_BINARY_DIR}/${CMAKE_PROJECT_NAME}-config-version.cmake 
  DESTINATION ${EXPORT_INSTALL_DIR} )

#----------------------------------------------
# Make build tree targets accessible for import
#----------------------------------------------
export ( TARGETS ${LIB_NAME} ${LIB_NAME}-static FILE ${CMAKE_PROJECT_NAME}-targets.cmake )

# build tree package config file, NOT installed
configure_file ( 
  ${CMAKE_SOURCE_DIR}/cmake/${CMAKE_PROJECT_NAME}-config.cmake.in
  ${CMAKE_BINARY_DIR}/${CMAKE_PROJECT_NAME}-config.cmake
  @ONLY )

export ( PACKAGE jsonfortran )