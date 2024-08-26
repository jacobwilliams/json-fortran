install ( EXPORT ${PACKAGE_NAME}-targets
  NAMESPACE ${PACKAGE_NAME}::
  DESTINATION "${EXPORT_INSTALL_DIR}" )

include ( CMakePackageConfigHelpers ) # Standard CMake module
write_basic_package_version_file( "${PROJECT_BINARY_DIR}/${PACKAGE_NAME}-config-version.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion )

# provides COMPILER_CONSISTENCY_CHECK
include ( ${PROJECT_SOURCE_DIR}/cmake/FCompilerConsistencyCheck.cmake )

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
   "${PROJECT_SOURCE_DIR}/json-fortran.pc.cmake.in"
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