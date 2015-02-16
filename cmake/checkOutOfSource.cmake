option ( ENABLE_IN_SOURCE_BUILDS
  "Allow in source builds? Do so at your own risk, only if you know what you are doing. We STRONGLY advise against in source builds."
  OFF )
mark_as_advanced ( ENABLE_IN_SOURCE_BUILDS )
get_filename_component ( FULL_BUILD_DIR  "${CMAKE_BINARY_DIR}" REALPATH )
get_filename_component ( FULL_SOURCE_DIR "${CMAKE_SOURCE_DIR}" REALPATH )
if ( "${FULL_BUILD_DIR}" STREQUAL "${FULL_SOURCE_DIR}" )
  if ( ENABLE_IN_SOURCE_BUILDS )
    message ( WARNING
      "Caution, in source build detected, procede at your own risk. Build and source directories are the same: ${CMAKE_SOURCE_DIR}" )
  else ( ENABLE_IN_SOURCE_BUILDS )
    message ( SEND_ERROR
      "Error, in source builds are not supported. If you really want an in source build (and know what you are doing) you may set the advanced ENABLE_IN_SOURCE_BUILDS variable to ON. Otherwise create a build directory not matching the source directory, '${CMAKE_SOURCE_DIR}'." )
  endif ( ENABLE_IN_SOURCE_BUILDS )
endif ( "${FULL_BUILD_DIR}" STREQUAL "${FULL_SOURCE_DIR}" )
