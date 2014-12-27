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
set ( VERSION_MAJOR 2 )
set ( VERSION_MINOR 0 )
set ( VERSION_PATCH 0 )
set ( VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )

#-------------------------------------
# Collect source files for the library
#-------------------------------------
set ( JF_LIB_SRCS  src/json_module.f90  )
set ( JF_TEST_SRCS src/json_example.f90 )

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
  VERSION ${VERSION}
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
set_target_properties ( ${LIB_NAME}
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR} 
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

#--------------------------
# Build the test executable
#--------------------------
add_executable ( test-${CMAKE_PROJECT_NAME} ${JF_TEST_SRCS} )
target_link_libraries ( test-${CMAKE_PROJECT_NAME} ${LIB_NAME} )
add_executable ( test-${CMAKE_PROJECT_NAME}-static ${JF_TEST_SRCS} )
target_link_libraries ( test-${CMAKE_PROJECT_NAME}-static ${LIB_NAME}-static )
set_target_properties ( test-${CMAKE_PROJECT_NAME} test-${CMAKE_PROJECT_NAME}-static
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )

#-------------------------------------
# Build the documentation with ROBODoc
#-------------------------------------
set ( ROBODOC_SKIP_DOC_GEN FALSE CACHE BOOL
  "Disable building the API documentation with ROBODoc" )
if ( NOT ROBODOC_SKIP_DOC_GEN )
  find_program ( ROBODOC robodoc )
  if ( ROBODOC ) # Found
    set ( ROBODOC_OPTIONS --tabsize 4 --index --toc --sections --syntaxcolors --source_line_numbers
      CACHE STRING "Options passed to robodoc to control building the documentation" )
    set ( DOC_DIR "${CMAKE_BINARY_DIR}/documentation" )
    set ( REQUIRED_ROBODOC_OPTIONS
      --src "${CMAKE_SOURCE_DIR}/src" --doc "${DOC_DIR}"
      --multidoc --html --ignore_case_when_linking
      --documenttitle "${CMAKE_PROJECT_NAME}" )
    # Dynamically generate the ROBODoc outputs list
    message ( STATUS "Dynamically computing ROBODoc output information..." )
    execute_process ( COMMAND ${CMAKE_COMMAND} -E remove_directory ${DOC_DIR} 
      COMMAND ${CMAKE_COMMAND} -E make_directory ${DOC_DIR}
      COMMAND "${ROBODOC}" ${REQUIRED_ROBODOC_OPTIONS} ${ROBODOC_OPTIONS} )
    file ( GLOB_RECURSE ROBODOC_OUTPUTS
      "${DOC_DIR}/*" )
    execute_process ( COMMAND ${CMAKE_COMMAND} -E remove_directory ${DOC_DIR} )
    message ( STATUS "Done dynamically computing ROBODoc outputs." )

    add_custom_command ( OUTPUT ${ROBODOC_OUTPUTS}
      COMMAND "${CMAKE_COMMAND}" -E make_directory "${DOC_DIR}" # Ensure DOC_DIR exists at build time
      COMMAND "${ROBODOC}" ${REQUIRED_ROBODOC_OPTIONS} ${ROBODOC_OPTIONS}
      DEPENDS "${CMAKE_SOURCE_DIR}/${JF_LIB_SRCS}" "${CMAKE_SOURCE_DIR}/${JF_TEST_SRCS}"
      COMMENT "Building HTML documentation for ${CMAKE_PROJECT_NAME} using ROBODoc" )
    add_custom_target ( documentation ALL
      DEPENDS ${ROBODOC_OUTPUTS} )
  else ( ROBODOC ) # Not found
    message ( WARNING
      "ROBODoc not found! Please set the CMake cache variable ROBODOC to point to the installed ROBODoc binary, and reconfigure or disable building the documentation. ROBODoc can be installed from: http://www.xs4all.nl/~rfsber/Robo/ If you do not wish to install ROBODoc and build the json-fortran documentation, then please set the CMake cache variable SKIP_DOCUMENTATION_GENERATION to FALSE." )
  endif ( ROBODOC )
endif ( NOT ROBODOC_SKIP_DOC_GEN )

#---------------------------------------------------------------------
# Add some tests to ensure that the software is performing as expected
#---------------------------------------------------------------------
enable_testing()
find_program ( JSONLINT jsonlint )
find_program ( DIFF     diff )
set ( DATA_DIR ${CMAKE_BINARY_DIR}/files )
configure_file ( ${CMAKE_SOURCE_DIR}/files/test1.json ${DATA_DIR}/test1.json COPYONLY )
configure_file ( ${CMAKE_SOURCE_DIR}/files/test5.json ${DATA_DIR}/test5.json COPYONLY )

# Validate input
if ( JSONLINT )
  add_test ( NAME validate-input1
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test1.json )
  add_test ( NAME validate-input5
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test5.json )
endif ( JSONLINT )
# As of now these are kind of sniff tests... Need to modify test program to indicate failure with `stop` codes
# or more easily parsed results, and be able to validate json sent to stdout

# Dynamic lib
add_test ( NAME test-${CMAKE_PROJECT_NAME}
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin/
  COMMAND test-${CMAKE_PROJECT_NAME} )
# Validate output
if ( JSONLINT )
  add_test ( NAME validate-test2
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test2.json )
  add_test ( NAME validate-test4
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test4.json )
endif ( JSONLINT )
# Check output for differences
if ( DIFF )
  add_test ( NAME test2-regression
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${DIFF} -q test2.json ${CMAKE_SOURCE_DIR}/files/test2.json )
  add_test ( NAME test4-regression
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${DIFF} -q test4.json ${CMAKE_SOURCE_DIR}/files/test4.json )
else ( DIFF )
  message ( WARNING
    "For full test coverage diff, or a similar tool must be present on your system" )
endif ( DIFF )
set_tests_properties ( validate-test2 test2-regression
  PROPERTIES
  DEPENDS test-${CMAKE_PROJECT_NAME}
  REQUIRED_FILES "${DATA_DIR}/test2.json"
  RESOURCE_LOCK  "${DATA_DIR}/test2.json" )
set_tests_properties ( validate-test4 test4-regression
  PROPERTIES
  DEPENDS test-${CMAKE_PROJECT_NAME}
  REQUIRED_FILES "${DATA_DIR}/test4.json"
  RESOURCE_LOCK  "${DATA_DIR}/test4.json" )

# Static lib
add_test ( NAME test-${CMAKE_PROJECT_NAME}-static
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/bin/"
  COMMAND test-${CMAKE_PROJECT_NAME}-static )
# Validate output
if ( JSONLINT )
  add_test ( NAME validate-test2-static
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test2.json )
  add_test ( NAME validate-test4-static
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${JSONLINT} test4.json )
endif ( JSONLINT )
# Check output for differences
if ( DIFF )
  add_test ( NAME test2-regression-static
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${DIFF} -q test2.json ${CMAKE_SOURCE_DIR}/files/test2.json )
  add_test ( NAME test4-regression-static
    WORKING_DIRECTORY ${DATA_DIR}
    COMMAND ${DIFF} -q test4.json ${CMAKE_SOURCE_DIR}/files/test4.json )
endif ( DIFF )
set_tests_properties ( validate-test2-static test2-regression-static
  PROPERTIES
  DEPENDS test-${CMAKE_PROJECT_NAME}-static
  REQUIRED_FILES "${DATA_DIR}/test2.json"
  RESOURCE_LOCK  "${DATA_DIR}/test2.json" )
set_tests_properties ( validate-test4-static test4-regression-static
  PROPERTIES
  DEPENDS test-${CMAKE_PROJECT_NAME}-static
  REQUIRED_FILES "${DATA_DIR}/test4.json"
  RESOURCE_LOCK  "${DATA_DIR}/test4.json" )

if ( JSONLINT )
  set_tests_properties ( test-${CMAKE_PROJECT_NAME} test-${CMAKE_PROJECT_NAME}-static
    PROPERTIES
    FAIL_REGULAR_EXPRESSION "Error;ERROR;error"
    REQUIRED_FILES "${DATA_DIR}/test1.json;${DATA_DIR}/test5.json"
    RESOURCE_LOCK "${DATA_DIR}/test2.json;${DATA_DIR}/test4.json"
    DEPENDS "validate-input1;validate-input5" )
else ( JSONLINT ) # Don't force validation of input if no JSONLINT
  message ( WARNING
    "For full test coverage please download and install jsonlint, a NODEJS package <http://nodejs.org>" )
  set_tests_properties ( test-${CMAKE_PROJECT_NAME} test-${CMAKE_PROJECT_NAME}-static
    PROPERTIES
    FAIL_REGULAR_EXPRESSION "Error;ERROR;error"
    REQUIRED_FILES "${DATA_DIR}/test1.json;${DATA_DIR}/test5.json"
    RESOURCE_LOCK "${DATA_DIR}/test2.json;${DATA_DIR}/test4.json" )
endif ( JSONLINT )
set_directory_properties ( PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
  "${DATA_DIR}/test2.json;${DATA_DIR}/test4.json" )

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
