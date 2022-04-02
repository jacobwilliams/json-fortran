# CMake Configuration and build added by Izaak Beekman -- May, 2014

# Copy right (c) 2014, Izaak Beekman
# All rights reserved.

# This file is contributed to the JSON-Fortran project, and
# is licensed under the terms of JSON-Fortran license. The JSON-Fortran
# license is located in the LICENSE file which must be distributed with
# this software. The contributing author, Izaak Beekman, retains all
# rights permitted by the terms of the JSON-Fortran license.

cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

# Use MSVS folders to organize projects on windows
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

set(PROJECT_DESCRIPTION "A Modern Fortran JSON API")
set(PROJECT_URL "https://github.com/jacobwilliams/json-fortran")

# Set the type/configuration of build to perform
set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "MinSizeRel" "RelWithDebInfo" )
set ( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )

enable_language ( Fortran )
include ( "cmake/pickFortranCompilerFlags.cmake" )

set_property ( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${CMAKE_CONFIGURATION_TYPES} )

# Check for in-source builds and error out if found
# Provides an advanced option to allow in source builds
include ( "cmake/checkOutOfSource.cmake" )

#---------------------
# Declare project name
#---------------------
project ( jsonfortran NONE )

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  option(JSON_FORTRAN_USE_OpenCoarrays
    "Build JSON-Fortran with support for linking against OpenCoarray programs" OFF)
endif()
if(JSON_FORTRAN_USE_OpenCoarrays)
  find_package(OpenCoarrays)
endif()

#---------------------
# Real and Integer kinds
#---------------------

SET(JSON_REAL_KIND "REAL64" CACHE STRING "Real kind parameter")
SET_PROPERTY(CACHE JSON_REAL_KIND PROPERTY STRINGS REAL32 REAL64 REAL128)
if(${JSON_REAL_KIND} MATCHES "REAL32")
  add_definitions(-DREAL32)
elseif(${JSON_REAL_KIND} MATCHES "REAL64")
  add_definitions(-DREAL64)
elseif(${JSON_REAL_KIND} MATCHES "REAL128")
  add_definitions(-DREAL128)
endif()

SET(JSON_INT_KIND "INT32" CACHE STRING "Integer kind parameter")
SET_PROPERTY(CACHE JSON_INT_KIND PROPERTY STRINGS INT8 INT16 INT32 INT64)
if(${JSON_INT_KIND} MATCHES "INT8")
  add_definitions(-DINT8)
elseif(${JSON_INT_KIND} MATCHES "INT16")
  add_definitions(-DINT16)
elseif(${JSON_INT_KIND} MATCHES "INT32")
  add_definitions(-DINT32)
elseif(${JSON_INT_KIND} MATCHES "INT64")
  add_definitions(-DINT64)
endif()

#----------------------------------
# Get version (semantic versioning)
# C.F. semver.org
#----------------------------------
file ( STRINGS "${PROJECT_SOURCE_DIR}/.VERSION" VERSION )
string( REPLACE "." ";" VERSION_LIST ${VERSION} )
list(GET VERSION_LIST 0 VERSION_MAJOR)
list(GET VERSION_LIST 1 VERSION_MINOR)
list(GET VERSION_LIST 2 VERSION_PATCH)
set(PROJECT_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
message ( STATUS "CMake build configuration for JSON-Fortran ${PROJECT_VERSION}" )

#-------------------------------------
# Collect source files for the library
#-------------------------------------
set ( JF_LIB_SRCS  src/json_kinds.F90
                   src/json_parameters.F90
                   src/json_string_utilities.F90
                   src/json_value_module.F90
                   src/json_file_module.F90
                   src/json_module.F90 )
file ( GLOB JF_TEST_SRCS "src/tests/jf_test_*.F90" )
set ( JF_TEST_UCS4_SUPPORT_SRC "${PROJECT_SOURCE_DIR}/src/tests/introspection/test_iso_10646_support.f90")

#-----------------------------------------
# Collect all the mod files into their own
# directory to ease installation issues
#-----------------------------------------
set ( MODULE_DIR "${PROJECT_BINARY_DIR}/include" )

#-------------------------------------
# Define where our files get installed
#-------------------------------------
set ( USE_GNU_INSTALL_CONVENTION FALSE
  CACHE BOOL
  "Install library, module file and documentation to standard GNU locations. Do not use this if supporting multiple Fortran compilers" )

# Set the package name to be specific to the compiler used, so that
# versions compiled with different compilers can be installed in parallel
string ( TOLOWER ${PROJECT_NAME}-${CMAKE_Fortran_COMPILER_ID} PACKAGE_NAME )
set ( PACKAGE_VERSION "${PACKAGE_NAME}-${VERSION}" )

if (USE_GNU_INSTALL_CONVENTION)
  include(GNUInstallDirs)
  set ( INSTALL_MOD_DIR "${CMAKE_INSTALL_INCLUDEDIR}" )
  set ( INSTALL_LIB_DIR "${CMAKE_INSTALL_LIBDIR}")
  set( ABS_LIB_INSTALL_DIR "\${CMAKE_INSTALL_FULL_LIBDIR}" )
else ()
  # Most of this could be 'wrong' for Windows/Cygwin
  set ( INSTALL_MOD_DIR    "${PACKAGE_VERSION}/lib" )
  set ( INSTALL_LIB_DIR    "${INSTALL_MOD_DIR}" )
  set( ABS_LIB_INSTALL_DIR "\${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}" )
endif ()


# Put package export CMake files where they can be found
# use `find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} <version> REQUIRED )`
if (USE_GNU_INSTALL_CONVENTION)
  set ( EXPORT_INSTALL_DIR "${INSTALL_LIB_DIR}/cmake/${PACKAGE_VERSION}" )
else ()
  set ( EXPORT_INSTALL_DIR "${PACKAGE_VERSION}/cmake" )
endif ()

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
# See if our compiler supports ISO 10646/UCS4
#---------------------------------------------
set ( ENABLE_UNICODE FALSE CACHE BOOL
  "Enable unicode/UCS4 support" )
if ( ENABLE_UNICODE )
  try_run( UCS4_TEST_RUNS UCS4_TEST_COMPILES
    ${PROJECT_BINARY_DIR}/bin ${JF_TEST_UCS4_SUPPORT_SRC} )
  if (UCS4_TEST_RUNS EQUAL 0)
    add_definitions (-DUSE_UCS4)
  else ()
    message ( WARNING
      "Unicode support requested but ${CMAKE_Fortran_COMPILER_ID} Fortran compiler does not support 'ISO_10646' characters!" )
  endif ()
endif ()

#---------------------------------------------
# Build a shared and static library by default
#---------------------------------------------

set ( LIB_NAME ${PROJECT_NAME} )
add_library ( ${LIB_NAME}        SHARED ${JF_LIB_SRCS} )
add_library ( ${LIB_NAME}-static STATIC ${JF_LIB_SRCS} )

if(JSON_FORTRAN_USE_OpenCoarrays)
  target_link_libraries(${LIB_NAME}
    PRIVATE OpenCoarrays::caf_mpi_static)
  target_link_libraries(${LIB_NAME}-static
    PRIVATE OpenCoarrays::caf_mpi_static)
endif()

target_include_directories(${LIB_NAME}
  PUBLIC
  $<BUILD_INTERFACE:${MODULE_DIR}>
  $<INSTALL_INTERFACE:${INSTALL_MOD_DIR}>)
target_include_directories(${LIB_NAME}-static
  PUBLIC
  $<BUILD_INTERFACE:${MODULE_DIR}>
  $<INSTALL_INTERFACE:${INSTALL_MOD_DIR}>)
set_target_properties ( ${LIB_NAME}-static
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  if(NOT MSVC_IDE)
  PREFIX lib
  endif()
  VERSION ${VERSION}
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${MODULE_DIR} )
set_target_properties ( ${LIB_NAME}
  PROPERTIES
  OUTPUT_NAME ${LIB_NAME}
  if(NOT MSVC_IDE)
  PREFIX lib
  endif()
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR} )

#-------------------------------------
# Build the documentation with FORD
#-------------------------------------
set ( SKIP_DOC_GEN FALSE CACHE BOOL
  "Disable building the API documentation with FORD" )
if ( NOT SKIP_DOC_GEN )
  find_program ( FORD ford )
  if ( FORD ) # Found
    file ( COPY "${PROJECT_SOURCE_DIR}/media" DESTINATION "${PROJECT_BINARY_DIR}/" )
    file ( GLOB_RECURSE PAGES_FILES "${PROJECT_SOURCE_DIR}/pages/*.*")
    set ( DOC_DIR "${PROJECT_BINARY_DIR}/doc" )
    set ( PAGES_DIR "${PROJECT_SOURCE_DIR}/pages" )
    set ( PROJ_DIR "${PROJECT_SOURCE_DIR}/src" )
    set ( FORD_PROJECT_FILE "${PROJECT_SOURCE_DIR}/json-fortran.md" )
    if ( ENABLE_UNICODE )
      set ( MACRO_FLAG "-m USE_UCS4" )
    else ()
      set ( MACRO_FLAG "" )
    endif ()
    # Pick the preprocessor to use based on the Fortran compiler
    if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
      set ( FPP "fpp\n" )
    else ()
      set ( FPP "gfortran -E\n" ) # default to gfortran -E for gfortran and unsupported compilers
    endif ()
    file ( WRITE "${PROJECT_BINARY_DIR}/.PREPROCESSOR" "${FPP}" )
    # Dynamically generate the FORD outputs list
    message ( STATUS "Dynamically computing FORD output information..." )
    if ( NOT (DEFINED FORD_OUTPUTS_CACHED) )
      message ( STATUS "Running FORD to dynamically compute documentation outputs, this could take a while..." )
      execute_process ( COMMAND ${CMAKE_COMMAND} -E remove_directory ${DOC_DIR}
	COMMAND ${CMAKE_COMMAND} -E make_directory ${DOC_DIR}
	COMMAND "${FORD}" --debug -q ${MACRO_FLAG} -d "${PROJ_DIR}" -o "${DOC_DIR}" -p "${PAGE_DIR}" "${FORD_PROJECT_FILE}" OUTPUT_QUIET )
    else ()
      message ( STATUS "Re-using cached FORD outputs, rather than regenerating them" )
    endif ()
    file ( GLOB_RECURSE FORD_OUTPUTS
      "${DOC_DIR}/*.*" )
    file ( GLOB_RECURSE FORD_CLEAN_OUTPUTS
      "${DOC_DIR}/*.*" )
    if ( (DEFINED FORD_OUTPUTS) AND ( NOT ( "${FORD_OUTPUTS}" STREQUAL "" ) ) )
      message ( STATUS "Caching FORD outputs" )
      set ( FORD_OUTPUTS_CACHED "${FORD_OUTPUTS}"
	CACHE STRING "variable containing FORD outputs to prevent rebuilding FORD docs" FORCE )
    endif ()
    message ( STATUS "Done dynamically computing FORD outputs." )

    foreach ( DOC_SRC_FILE ${JF_LIB_SRCS} ${JF_TEST_SRCS} ${PROJECT_SOURCE_DIR}/README.md
	${PROJECT_SOURCE_DIR}/CHANGELOG.md ${PROJECT_SOURCE_DIR}/.github/CONTRIBUTING.md
	${PROJECT_SOURCE_DIR}/LICENSE ${PROJECT_SOURCE_DIR}/json-fortran.md ${PAGES_FILES} )
      list ( APPEND FORD_DEPENDS "${DOC_SRC_FILE}" )
    endforeach ()
    add_custom_command ( OUTPUT ${FORD_OUTPUTS_CACHED}
      COMMAND "${FORD}" --debug ${MACRO_FLAG} -d "${PROJ_DIR}" -o "${DOC_DIR}" -p "${PROJECT_SOURCE_DIR}/pages" "${FORD_PROJECT_FILE}"
      MAIN_DEPENDENCY "${FORD_PROJECT_FILE}"
      DEPENDS ${FORD_DEPENDS}
      COMMENT "Building HTML documentation for ${PROJECT_NAME} using FORD" )
    add_custom_target ( documentation ALL
      DEPENDS ${FORD_OUTPUTS_CACHED} )
    set ( INSTALL_API_DOCUMENTATION TRUE
      CACHE BOOL "Install FORD generated documentation?" )
    if ( INSTALL_API_DOCUMENTATION )
      if ( USE_GNU_INSTALL_CONVENTION )
	install ( DIRECTORY "${DOC_DIR}/" DESTINATION "${CMAKE_INSTALL_DOCDIR}" )
      else ()
	install ( DIRECTORY "${DOC_DIR}/" DESTINATION "${CMAKE_INSTALL_PREFIX}/${PACKAGE_VERSION}/doc" )
      endif ()
    endif ()
  else () # Not found
    message ( WARNING
      "FORD not found! Please set the CMake cache variable FORD to point to the installed FORD executable, and reconfigure or disable building the documentation. FORD can be installed from PYPI with `pip install ford` or from <https://github.com/Fortran-FOSS-Programmers/ford> If you do not wish to install FORD and build the JSON-Fortran documentation, then please set the CMake cache variable SKIP_DOC_GEN to TRUE." )
  endif ()
endif ()

#--------------------------
# Handle test related stuff
#--------------------------
set ( ENABLE_TESTS TRUE CACHE BOOL
  "Enable the JSON-Fortran tests." )

#---------------------------------------------------------------------
# Add some tests to ensure that the software is performing as expected
#---------------------------------------------------------------------
if ( ENABLE_TESTS )
  enable_testing()

  # emulate GNU Autotools `make check`
  add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure)
  add_custom_target(build_tests) # Make target to build all tests
  add_dependencies(build_tests ${LIB_NAME} ${LIB_NAME}-static)

  find_program ( JSONLINT jsonlint )

  set ( DATA_DIR "${PROJECT_SOURCE_DIR}/files" )

  set_directory_properties ( PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
    "${FORD_CLEAN_OUTPUTS}" )

  # Validate input
  if ( JSONLINT )
    file ( GLOB JSON_INPUTS "${DATA_DIR}/inputs/*.json" )
    file ( GLOB INVALID_JSON "${DATA_DIR}/inputs/*invalid*.json" "${DATA_DIR}/inputs/comments.json")

    list ( REMOVE_ITEM JSON_INPUTS ${INVALID_JSON} )
    list ( REMOVE_ITEM JSON_INPUTS "${DATA_DIR}/inputs/big.json" ) # This takes too long and is valid
                                                                   # JSON from a trusted source

    foreach ( VALID_JSON ${JSON_INPUTS} )
      get_filename_component ( TESTNAME "${VALID_JSON}" NAME )
      add_test ( NAME validate-${TESTNAME}
	      WORKING_DIRECTORY "${DATA_DIR}/inputs"
	      COMMAND ${JSONLINT} "--allow=nonescape-characters" "${VALID_JSON}" )
    endforeach ()

    foreach ( INVALID ${INVALID_JSON} )
      get_filename_component ( TESTNAME "${INVALID}" NAME )
      add_test ( NAME validate-${TESTNAME}
	      WORKING_DIRECTORY "${DATA_DIR}/inputs"
	      COMMAND ${JSONLINT} "${INVALID}" )
      set_property ( TEST validate-${TESTNAME}
	      PROPERTY
	      WILL_FAIL TRUE)
    endforeach ()
  endif ()

  add_test(NAME jf-cleanup-fixture
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
    COMMAND ${CMAKE_COMMAND} -E remove_directory "${PROJECT_BINARY_DIR}/files")
  set_tests_properties(jf-cleanup-fixture
    PROPERTIES FIXTURES_SETUP JF)
  add_test(NAME jf-setup-fixture
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${DATA_DIR}" "${PROJECT_BINARY_DIR}/files")
  set_tests_properties(jf-setup-fixture
    PROPERTIES FIXTURES_SETUP JF
    DEPENDS jf-cleanup-fixture)

  set ( UNIT_TESTS '' )
  foreach ( UNIT_TEST ${JF_TEST_SRCS} )
    get_filename_component ( TEST ${UNIT_TEST} NAME_WE )
    if(MSVC_IDE)
      link_directories(${PROJECT_BINARY_DIR}/lib)
    endif()
    add_executable ( ${TEST} EXCLUDE_FROM_ALL ${UNIT_TEST} )
    target_link_libraries ( ${TEST} ${LIB_NAME} )
    add_dependencies ( check ${TEST} )
    add_dependencies ( build_tests ${TEST} )
    set_target_properties ( ${TEST}
      PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin )
    add_test( NAME ${TEST}
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/bin
      COMMAND ./${TEST})
    set_tests_properties( ${TEST}
      PROPERTIES FIXTURES_REQUIRED JF)
    list ( APPEND UNIT_TESTS ${TEST} )
    if ( JSONLINT )
      set_property ( TEST ${TEST}
	      APPEND
	      PROPERTY DEPENDS validate-input1 validate-input2 )
    endif()
  endforeach ( UNIT_TEST )

  set_property ( TEST jf_test_03
    APPEND
    PROPERTY DEPENDS jf_test_02 )

  # Validate output
  file( GLOB EXPECTED_OUTPUTS "${DATA_DIR}/expected-outputs/*.json")
  if (NOT ${ENABLE_UNICODE})
    list( REMOVE_ITEM EXPECTED_OUTPUTS "${DATA_DIR}/expected-outputs/hello-world-ucs4.json")
  endif()
  list( REMOVE_ITEM EXPECTED_OUTPUTS "${DATA_DIR}/expected-outputs/example2.json")

  if ( JSONLINT )
    foreach ( JSON_FILE ${EXPECTED_OUTPUTS} )
      get_filename_component ( TESTNAME ${JSON_FILE} NAME )
      add_test ( NAME validate-output-${TESTNAME}
	      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/files"
	      COMMAND ${JSONLINT} "--allow=nonescape-characters" ${TESTNAME} )
      set_property ( TEST validate-output-${TESTNAME}
	      APPEND
	      PROPERTY
	      DEPENDS ${UNIT_TESTS})
    endforeach ( JSON_FILE )
  endif ()

  # Check output for differences
  if(CMAKE_VERSION VERSION_GREATER 3.13.5)
    set( JSON_FORTRAN_COMPARE_FLAG "--ignore-eol")
  endif()

  foreach ( JSON_FILE ${EXPECTED_OUTPUTS} )
    get_filename_component (OUTPUT ${JSON_FILE} NAME )
    add_test ( NAME regression-${OUTPUT}
	    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/files"
	    COMMAND ${CMAKE_COMMAND} -E compare_files ${JSON_FORTRAN_COMPARE_FLAG} ${OUTPUT} expected-outputs/${OUTPUT} )
    set_property ( TEST regression-${OUTPUT}
	    APPEND
	    PROPERTY
	    DEPENDS ${UNIT_TESTS}
	    REQUIRED_FILES ${EXPECTED_OUTPUTS} )
  endforeach ( JSON_FILE )


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

set(MOD_DIR_TO_INSTALL "${MODULE_DIR}")
set(MOD_DESTINATION_DIR "${INSTALL_MOD_DIR}")
install(
  CODE "file(GLOB_RECURSE MODULE_FILES \"${MOD_DIR_TO_INSTALL}/*.mod\")"
  CODE "file(GLOB_RECURSE SUBMOD_FILES \"${MOD_DIR_TO_INSTALL}/*.smod\")"
  CODE "file(INSTALL \${MODULE_FILES} DESTINATION \"\${CMAKE_INSTALL_PREFIX}/${MOD_DESTINATION_DIR}\")"
  CODE "file(INSTALL \${SUBMOD_FILES} DESTINATION \"\${CMAKE_INSTALL_PREFIX}/${MOD_DESTINATION_DIR}\")"
  )

#------------------------------------------
# Add portable unistall command to makefile
#------------------------------------------
# Adapted from the CMake Wiki FAQ
configure_file ( "${PROJECT_SOURCE_DIR}/cmake/uninstall.cmake.in" "${PROJECT_BINARY_DIR}/uninstall.cmake"
    @ONLY)

add_custom_target ( uninstall
    COMMAND ${CMAKE_COMMAND} -P "${PROJECT_BINARY_DIR}/uninstall.cmake" )

#-----------------------------------------------------
# Publicize installed location to other CMake projects
#-----------------------------------------------------
install ( EXPORT ${PACKAGE_NAME}-targets
  NAMESPACE ${PACKAGE_NAME}::
  DESTINATION "${EXPORT_INSTALL_DIR}" )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file( "${PROJECT_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${VERSION}
  COMPATIBILITY SameMajorVersion )

# provides COMPILER_CONSISTENCY_CHECK
include ( cmake/FCompilerConsistencyCheck.cmake )

# install package config file
configure_package_config_file (
  "${PROJECT_SOURCE_DIR}/cmake/pkg/${PROJECT_NAME}-config.cmake.in"
  "${PROJECT_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  INSTALL_DESTINATION "${EXPORT_INSTALL_DIR}"
  PATH_VARS EXPORT_INSTALL_DIR INSTALL_MOD_DIR )

# Install the config and version files so that we can find this project with others
install ( FILES
  "${PROJECT_BINARY_DIR}/pkg/${PACKAGE_NAME}-config.cmake"
  "${PROJECT_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  DESTINATION "${EXPORT_INSTALL_DIR}" )

#----------------------------------------------
# Make build tree targets accessible for import
#----------------------------------------------
export ( TARGETS ${LIB_NAME} ${LIB_NAME}-static FILE ${PACKAGE_NAME}-targets.cmake )

# build tree package config file, NOT installed
configure_file (
  "${PROJECT_SOURCE_DIR}/cmake/${PROJECT_NAME}-config.cmake.in"
  "${PROJECT_BINARY_DIR}/${PACKAGE_NAME}-config.cmake"
  @ONLY )

set ( ENABLE_BUILD_TREE_EXPORT FALSE CACHE BOOL
  "Add the ${PACKAGE_NAME} build tree to the CMake package registry?" )
if ( ENABLE_BUILD_TREE_EXPORT )
  export ( PACKAGE ${PACKAGE_NAME} )
endif ()

# pkg-config stuff
configure_file(
   "${CMAKE_CURRENT_SOURCE_DIR}/json-fortran.pc.cmake.in"
   "${CMAKE_CURRENT_BINARY_DIR}/json-fortran.pc"
   @ONLY
)
install(FILES
   "${CMAKE_CURRENT_BINARY_DIR}/json-fortran.pc"
   DESTINATION "${INSTALL_LIB_DIR}/pkgconfig"
)

if(MSVC_IDE)
   INCLUDE_DIRECTORIES("src")
   SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp")
endif()
