!*****************************************************************************************
!>
!  Module for the 51th unit test. See Issue #569.

module jf_test_51_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_51

contains

    subroutine test_51(error_cnt)

    !! 51th unit test. see Issue #569

    integer,intent(out) :: error_cnt

    type(json_file) :: json

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 51'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! array cases:
    call json%initialize(allow_trailing_comma = .false.)
    call json%deserialize('{"a" : [1,2,3,]}')
    if (.not. json%failed()) then
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'read array with trailing comma'
    else
        ! it was supposed to fail
        write(output_unit,'(A)') '1 success'
    end if

    call json%initialize(allow_trailing_comma = .true.)
    call json%deserialize('{"a" : [1,2,3,]}')
    if (json%failed()) then
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'failed to read array with trailing comma'
    else
        write(output_unit,'(A)') '2 success'
    end if

    ! object cases:
    call json%initialize(allow_trailing_comma = .false.)
    call json%deserialize('{"a" : 1, "b" : 2, }')
    if (.not. json%failed()) then
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'read object with trailing comma'
    else
        ! it was supposed to fail
        write(output_unit,'(A)') '3 success'
    end if
    call json%initialize(allow_trailing_comma = .true.)
    call json%deserialize('{"a" : 1, "b" : 2, }')
    if (json%failed()) then
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'failed to read object with trailing comma'
    else
        write(output_unit,'(A)') '4 success'
    end if

    end subroutine test_51

end module jf_test_51_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_51

    use jf_test_51_mod , only: test_51

    implicit none
    integer :: n_errors

    call test_51(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_51
#endif
!*****************************************************************************************

