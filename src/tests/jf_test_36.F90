!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/15/2019
!
! Module for the 36th unit test.

module jf_test_36_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_36

contains

    subroutine test_36(error_cnt)

    !! Test writing a large JSON structure to a string.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file) :: my_file
    character(kind=json_CK,len=:),allocatable :: str_in, str_out

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 36'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    ! create a long string:
    str_in = '{"long_string": "' // repeat('a', 10000) //'"}'

    call my_file%initialize()

    call my_file%load_from_string(str_in)

    if (my_file%failed()) then
        call my_file%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call my_file%print_to_string(str_out)
    if (my_file%failed()) then
        call my_file%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call my_file%destroy()

    if (error_cnt==0) write(error_unit,'(A)') ' Success!'

    end subroutine test_36

end module jf_test_36_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
!*****************************************************************************************
program jf_test_36

    !! 36th unit test.

    use jf_test_36_mod, only: test_36
    implicit none
    integer :: n_errors
    call test_36(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_36
!*****************************************************************************************
#endif
