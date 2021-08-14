!*****************************************************************************************
!>
!  Module for the 48th unit test.

module jf_test_48_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_48

    character(len=*),parameter :: filename = '../files/inputs/big.json' !! large file to open

contains

    subroutine test_48(error_cnt)

    !! Clone test

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p, p_clone
    type(json_core) :: json  !! factory for manipulating `json_value` pointers

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 48'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') 'open file'
    call json%load(filename, p)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !test the deep copy routine:
    write(error_unit,'(A)') 'json_clone test'
    call json%clone(p,p_clone)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%destroy(p)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%destroy(p_clone)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success'
    else
        write(error_unit,'(A)') 'Failed'
    end if

    write(error_unit,'(A)') ''

    end subroutine test_48

end module jf_test_48_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_48

    !! clone test

    use jf_test_48_mod , only: test_48
    implicit none
    integer :: n_errors
    integer :: i !! counter

    integer,parameter :: n_repeat = 1 !! number of times to repeat the test

    do i = 1, n_repeat
        call test_48(n_errors)
        if (n_errors /= 0) stop 1
    end do

end program jf_test_48
#endif
!*****************************************************************************************
