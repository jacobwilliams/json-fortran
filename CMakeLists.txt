# CMake Configuration and build added by Izaak Beekman -- May, 2014

# Copy right (c) 2014, Izaak Beekman
# All rights reserved.

# This file is contributed to the json-fortran project, and
# is licensed under the terms of json-fortran license. The json-fortran
# license is located in the LICENSE file which must be distributed with
# this software. The contributing author, Izaak Beekman, retains all
# rights permitted by the terms of the json-fortran license.

cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

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
set ( VERSION_MAJOR 3 )
set ( VERSION_MINOR 0 )
set ( VERSION_PATCH 0 )
set ( VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}" )

#-------------------------------------
# Collect source files for the library
#-------------------------------------
set ( JF_LIB_SRCS  src/json_module.f90 )
file ( GLOB JF_TEST_SRCS "src/tests/jf_test_*.f90" )

#-----------------------------------------
# Collect all the mod files into their own
# directory to ease installation issues
#-----------------------------------------
set ( MODULE_DIR "${CMAKE_BINARY_DIR}/lib" )

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
endif ()

if ( ENABLE_DYLIBS_USE_RPATH )
  set ( CMAKE_MACOSX_RPATH TRUE )
else ()
  set ( CMAKE_INSTALL_NAME_DIR
    "${ABS_LIB_INSTALL_DIR}" )
endif ()

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
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${MODULE_DIR} )
set_target_properties ( ${LIB_NAME}
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR} )

#-------------------------------------
# Build the documentation with ROBODoc
#-------------------------------------
set ( SKIP_DOC_GEN FALSE CACHE BOOL
  "Disable building the API documentation with ROBODoc" )
if ( NOT SKIP_DOC_GEN )
  find_program ( ROBODOC robodoc )
  if ( ROBODOC ) # Found
    set ( ROBODOC_OPTIONS --rc ${CMAKE_SOURCE_DIR}/robodoc.rc --tabsize 4 --index --toc --sections --syntaxcolors --source_line_numbers
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

    foreach ( SRC_FILE ${JF_LIB_SRCS} ${JF_TEST_SRCS} )
      list ( APPEND ROBO_DEPENDS "${SRC_FILE}" )
    endforeach ( SRC_FILE )
    add_custom_command ( OUTPUT ${ROBODOC_OUTPUTS}
      COMMAND "${CMAKE_COMMAND}" -E make_directory "${DOC_DIR}" # Ensure DOC_DIR exists at build time
      COMMAND "${ROBODOC}" ${REQUIRED_ROBODOC_OPTIONS} ${ROBODOC_OPTIONS}
      DEPENDS ${ROBO_DEPENDS}
      COMMENT "Building HTML documentation for ${CMAKE_PROJECT_NAME} using ROBODoc" )
    add_custom_target ( documentation ALL
      DEPENDS ${ROBODOC_OUTPUTS} )
  else () # Not found
    message ( WARNING
      "ROBODoc not found! Please set the CMake cache variable ROBODOC to point to the installed ROBODoc binary, and reconfigure or disable building the documentation. ROBODoc can be installed from: http://www.xs4all.nl/~rfsber/Robo/ If you do not wish to install ROBODoc and build the json-fortran documentation, then please set the CMake cache variable SKIP_DOC_GEN to TRUE." )
  endif ()
endif ()

#--------------------------
# Handle test related stuff
#--------------------------
set ( ENABLE_TESTS TRUE CACHE BOOL
  "Enable the json-fortran tests." )

#---------------------------------------------------------------------
# Add some tests to ensure that the software is performing as expected
#---------------------------------------------------------------------
if ( ENABLE_TESTS )
  enable_testing()

  find_program ( JSONLINT jsonlint )
  find_program ( DIFF     diff )
  set ( DATA_DIR ${CMAKE_BINARY_DIR}/files )
  configure_file ( ${CMAKE_SOURCE_DIR}/files/test1.json    ${DATA_DIR}/test1.json    COPYONLY )
  configure_file ( ${CMAKE_SOURCE_DIR}/files/test5.json    ${DATA_DIR}/test5.json    COPYONLY )
  configure_file ( ${CMAKE_SOURCE_DIR}/files/invalid.json  ${DATA_DIR}/invalid.json  COPYONLY )
  configure_file ( ${CMAKE_SOURCE_DIR}/files/invalid2.json ${DATA_DIR}/invalid2.json COPYONLY )

  set_directory_properties ( PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
    "${DATA_DIR}/test2.json;${DATA_DIR}/test4.json" )

  # Validate input
  if ( JSONLINT )
    add_test ( NAME validate-input1
      WORKING_DIRECTORY ${DATA_DIR}
      COMMAND ${JSONLINT} test1.json )
    add_test ( NAME validate-input5
      WORKING_DIRECTORY ${DATA_DIR}
      COMMAND ${JSONLINT} test5.json )
    add_test ( NAME validate-invalid
      WORKING_DIRECTORY ${DATA_DIR}
      COMMAND ${JSONLINT} invalid.json )
    set_property ( TEST validate-invalid
      PROPERTY
      WILL_FAIL TRUE)
    add_test ( NAME validate-invalid2
      WORKING_DIRECTORY ${DATA_DIR}
      COMMAND ${JSONLINT} invalid2.json )
    set_property ( TEST validate-invalid2
      PROPERTY
      WILL_FAIL TRUE)
  endif ()

  set ( UNIT_TESTS '' )
  foreach ( UNIT_TEST ${JF_TEST_SRCS} )
    get_filename_component ( TEST ${UNIT_TEST} NAME_WE )
    add_executable ( ${TEST} ${UNIT_TEST} )
    target_link_libraries ( ${TEST} ${LIB_NAME} )
    set_target_properties ( ${TEST}
      PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
    add_test( NAME ${TEST}
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin
      COMMAND ./${TEST})
    list ( APPEND UNIT_TESTS ${TEST} )
    if ( JSONLINT )
      set_property ( TEST ${TEST}
	APPEND
	PROPERTY DEPENDS validate-input1 validate-input2 )
    endif()
  endforeach ( UNIT_TEST )

  set_property ( TEST jf_test_3
    APPEND
    PROPERTY DEPENDS jf_test_2 )

  # Validate output
  if ( JSONLINT )
    file ( GLOB JSON_FILES "${DATA_DIR}/test*.json" )
    foreach ( JSON_FILE ${JSON_FILES} )
      get_filename_component ( JSON_STEM ${JSON_FILE} NAME_WE )
      add_test ( NAME validate-output-${JSON_STEM}.json
	WORKING_DIRECTORY ${DATA_DIR}
	COMMAND ${JSONLINT} ${JSON_STEM}.json )
      set_property ( TEST validate-output-${JSON_STEM}.json
	APPEND
	PROPERTY
	DEPENDS ${UNIT_TESTS}
	REQUIRED_FILES ${JSON_FILES} )
    endforeach ( JSON_FILE )
  endif ()
  # Check output for differences
  if ( DIFF )
    file ( GLOB JSON_FILES "${DATA_DIR}/test*.json" )
    foreach ( JSON_FILE ${JSON_FILES} )
      get_filename_component ( JSON_STEM ${JSON_FILE} NAME_WE )
      add_test ( NAME regression-${JSON_STEM}.json
	WORKING_DIRECTORY ${DATA_DIR}
	COMMAND ${DIFF} -q ${JSON_STEM}.json ${CMAKE_SOURCE_DIR}/files/${JSON_STEM}.json )
      set_property ( TEST regression-${JSON_STEM}.json
	APPEND
	PROPERTY
	DEPENDS ${UNIT_TESTS}
	REQUIRED_FILES ${JSON_FILES} )
    endforeach ( JSON_FILE )
  else ()
    message ( WARNING
      "For full test coverage diff, or a similar tool must be present on your system" )
  endif ()

endif ()

#-------------------------
# Perform the installation
#-------------------------

install ( TARGETS ${LIB_NAME} ${LIB_NAME}-static
  EXPORT ${PACKAGE_NAME}-targets
  LIBRARY DESTINATION "${INSTALL_LIB_DIR}"
  ARCHIVE DESTINATION "${INSTALL_LIB_DIR}" )

# Code to  fix the dylib install name on Mac.
include ( cmake/fixupInstallNameDir.cmake )

install ( DIRECTORY "${MODULE_DIR}/" DESTINATION  "${INSTALL_MOD_DIR}" )

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
endif ()
