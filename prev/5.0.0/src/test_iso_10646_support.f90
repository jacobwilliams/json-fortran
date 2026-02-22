!*******************************************************************************************************
!> author: Izaak Beekman
!
! This program is run when configuring the json-fortran build,
! to determine whether or not ISO 10646/UCS4 characters are
! supported by the compiler.

    program test_iso_10646_support

    use iso_fortran_env ,only: output_unit, error_unit

    implicit none

    integer, parameter :: UCS4_K = selected_char_kind('ISO_10646')

    if ( UCS4_K == -1 ) then !Not supported!
        write(error_unit,'(A)') 'Your compiler does not support ISO 10646/UCS4 characters!'
        write(error_unit,'(A)') 'JSON-Fortran must/will be configured to use the "DEFAULT"'
        write(error_unit,'(A)') 'character set. (Should be "ASCII" on a reasonable system.)'
        stop 2
    else
        write(error_unit,'(A)') 'Congratulations! Your compiler supports ISO 10646/UCS4!'
        write(error_unit,'(A)') 'JSON-Fortran may be configured to enable UCS4 support.'
        write(output_unit,'(A)') 'UCS4_SUPPORTED'
    end if

    end program test_iso_10646_support
!*******************************************************************************************************