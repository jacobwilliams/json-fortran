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

#-------------------------------------
# Collect source files for the library
#-------------------------------------
set ( JF_LIB_SRCS src/json_module.f90 )

#-----------------------------------------
# Collect all the mod files into their own
# directory to ease installation issues
#-----------------------------------------
set ( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/lib" )

#-------------------------------------
# Define where our files get installed
#-------------------------------------
# Set the package name to be specific to the compiler used, so that
# versions compiled with different compilers can be installed in parallel
string ( TOLOWER ${CMAKE_PROJECT_NAME}-${CMAKE_Fortran_COMPILER_ID} PACKAGE_NAME )
string ( TOLOWER ${CMAKE_Fortran_COMPILER_ID}-compiler              FCOMPILER_DIR )
set ( PACKAGE_VERSION "${PACKAGE_NAME}-${VERSION}" )


# Most of this could be 'wrong' for Windows/Cygwin

set ( INSTALL_MOD_DIR    "${PACKAGE_VERSION}/lib" )
set ( INSTALL_LIB_DIR    "${INSTALL_MOD_DIR}" )
set( ABS_LIB_INSTALL_DIR "\${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}" )

# Put package export CMake files where they can be found
# use `find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} <version> REQUIRED )`
set ( EXPORT_INSTALL_DIR "${PACKAGE_VERSION}/cmake" )

if ( "${CMAKE_SYSTEM_NAME}" MATCHES "Darwin" )
  set ( ENABLE_DYLIBS_USE_RPATH TRUE CACHE BOOL
    "Enable @rpath install name for dylibs" )
  mark_as_advanced ( ENABLE_DYLIBS_USE_RPATH )
endif ( "${CMAKE_SYSTEM_NAME}" MATCHES "Darwin" )

if ( ENABLE_DYLIBS_USE_RPATH )
  set ( CMAKE_MACOSX_RPATH TRUE )
else ( ENABLE_DYLIBS_USE_RPATH )
  set ( CMAKE_INSTALL_NAME_DIR
    "${ABS_LIB_INSTALL_DIR}" )
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
  LIBRARY DESTINATION "${INSTALL_LIB_DIR}"
  ARCHIVE DESTINATION "${INSTALL_LIB_DIR}" )

# Code to  fix the dylib install name on Mac.
include ( cmake/fixupInstallNameDir.cmake )

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
install ( EXPORT ${PACKAGE_NAME}-targets DESTINATION "${EXPORT_INSTALL_DIR}" )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file( "${CMAKE_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${VERSION}
  COMPATIBILITY SameMajorVersion )

# provides COMPILER_CONSISTENCY_CHECK
include ( cmake/FCompilerConsistencyCheck.cmake )

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
