if ( NOT Fortran_FLAGS_INIT )
  set ( Fortran_FLAGS_INIT TRUE )
  set ( ENABLE_BACK_TRACE TRUE CACHE BOOL
    "Enable backtraces on unexpected runtime errors? (Recommended)" )
  set ( ENABLE_COMPILE_TIME_WARNINGS TRUE CACHE BOOL
    "Enable diagnostic warnings at compile time? (Recommended)" )
  set ( ENABLE_RUNTIME_CHECKS FALSE CACHE BOOL
    "Enable compiler run-time checks? (Enabling this will turn off most compiler optimizations.)" )
  mark_as_advanced ( ENABLE_RUNTIME_CHECKS )

  if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
    if ( ENABLE_BACK_TRACE )
      add_compile_options ( -traceback )
    endif ( ENABLE_BACK_TRACE )
    if ( ENABLE_COMPILE_TIME_WARNINGS )
      # The following warning might be triggered by ifort unless explicitly silenced:
      # warning #7601: F2008 standard does not allow an internal procedure to be an actual argument procedure 
      # name. (R1214.4). In the context of F2008 this is an erroneous warning.
      # See https://prd1idz.cps.intel.com/en-us/forums/topic/486629
      add_compile_options ( -warn -stand f08 -diag-disable 7601 )
    endif ( ENABLE_COMPILE_TIME_WARNINGS )
    if ( ENABLE_RUNTIME_CHECKS )
      add_compile_options ( -check all )
    endif ( ENABLE_RUNTIME_CHECKS )
  elseif ( "{CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
    if ( ENABLE_BACK_TRACE )
      add_compile_options ( -fbacktrace )
    endif ( ENABLE_BACK_TRACE )
    if ( ENABLE_COMPILETIME_CHECKS )
      add_compile_options ( -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008 )
    endif ( ENABLE_COMPILETIME_CHECKS )
    if ( ENABLE_RUNTIME_CHECKS )
      add_compile_options ( -fcheck=all )
    endif ( ENABLE_RUNTIME_CHECKS )
  endif ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
endif ( NOT Fortran_FLAGS_INIT )