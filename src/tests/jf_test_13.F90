!*****************************************************************************************
!> author: Jacob Williams
!  date: 09/01/2015
!
! Module for the 13th unit test.

module jf_test_13_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_13

contains

    subroutine test_13(error_cnt)

    !! Tests different real format strings using repeated calls to [[json_initialize]].

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file) :: my_file
    character(kind=json_CK,len=:),allocatable :: str
    integer :: i

    character(len=2),dimension(5),parameter :: fmts=['g ','e ','en','es','* ']
        !! format statements to test

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 13'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    do i=1,size(fmts)

        call my_file%initialize(real_format=trim(fmts(i)))

        call my_file%deserialize('{ "value": 1234.56789 }')
        if (my_file%failed()) then
            call my_file%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        call my_file%serialize(str)
        if (my_file%failed()) then
            call my_file%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(output_unit,'(A)') str
        end if

        call my_file%destroy()

    end do

    end subroutine test_13

end module jf_test_13_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_13

    !! 13th unit test.

    use jf_test_13_mod, only: test_13
    implicit none
    integer :: n_errors
    call test_13(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_13
!*****************************************************************************************
#endif
