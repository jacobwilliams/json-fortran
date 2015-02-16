# Code to fixup install names when dylibs not using @rpath are installed using DESTDIR
# I think this could be considered a CMake Bug. This is a work around.
if ( NOT ENABLE_DYLIBS_USE_RPATH )
  if ( CMAKE_INSTALL_NAME_TOOL ) # On Mac and have install_name_tool
    install ( CODE
      "if ( DEFINED ENV{DESTDIR} )
         string ( REGEX REPLACE \"/$\" \"\" DESTDIR \"\$ENV{DESTDIR}\" ) # strip trailing /
         get_filename_component ( INSTALL_LIB
              \${DESTDIR}/${ABS_LIB_INSTALL_DIR}/lib${LIB_NAME}.${VERSION_MAJOR}.${VERSION_MINOR}.dylib
              ABSOLUTE )
         execute_process ( COMMAND \"${CMAKE_INSTALL_NAME_TOOL}\"
            -id \"\${INSTALL_LIB}\" \"\${INSTALL_LIB}\" )
       endif ( DEFINED ENV{DESTDIR} )" )
  endif ( CMAKE_INSTALL_NAME_TOOL )
endif ( NOT ENABLE_DYLIBS_USE_RPATH )
