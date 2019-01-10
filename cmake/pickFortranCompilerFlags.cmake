if ( NOT Fortran_FLAGS_INIT )
  set ( Fortran_FLAGS_INIT TRUE )
  set ( ENABLE_BACK_TRACE TRUE CACHE BOOL
    "Enable backtraces on unexpected runtime errors? (Recommended)" )
  set ( ENABLE_COMPILE_TIME_WARNINGS TRUE CACHE BOOL
    "Enable diagnostic warnings at compile time? (Recommended)" )
  set ( ENABLE_RUNTIME_CHECKS FALSE CACHE BOOL
    "Enable compiler run-time checks? (Enabling this will turn off most compiler optimizations.)" )
  mark_as_advanced ( ENABLE_RUNTIME_CHECKS )

  if ( "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows" )
    set(pre /)
    set(spc :)
    set(Q /Q)
  else()
    set(pre -)
    set(spc " ")
    set(Q -)
  endif()
  if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
    set(TRACE_FLAG -traceback)
    if ( ENABLE_BACK_TRACE )
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${pre}traceback")
    endif ()
    if ( ENABLE_COMPILE_TIME_WARNINGS )
      # The following warning might be triggered by ifort unless explicitly silenced:
      # warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure
      # name. (R1214.4). In the context of F2008 this is an erroneous warning.
      # See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${pre}warn ${pre}stand${spc}f08 ${Q}diag-disable${spc}7601 ${Q}diag-disable${spc}5142" )
    endif ()
    if ( ENABLE_RUNTIME_CHECKS )
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -check all" )
    endif ()
  elseif ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    # add a coverage build configuration
    set ( CMAKE_CONFIGURATION_TYPES ${CMAKE_CONFIGURATION_TYPES} "Coverage" )
    set ( CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage -O0" CACHE STRING
      "Fortran compiler flags for coverage configuration" )
    if ( ENABLE_BACK_TRACE )
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace -fno-omit-frame-pointer" )
    endif ()
    if ( ENABLE_COMPILETIME_CHECKS )
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008" )
    endif ()
    if ( ENABLE_RUNTIME_CHECKS )
      set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcheck=all -fno-omit-frame-pointer" )
    endif ()
  endif ()
endif ()
