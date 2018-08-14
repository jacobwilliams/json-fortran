!*****************************************************************************************
!>
! Module for the thirty sixth unit test.
!
!# HISTORY
!  * Ian Porter : 8/14/2018

module jf_test_36_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    private
    public :: test_36

    character(len=*),parameter :: dir  = '../files/inputs/'   !! working directory
    character(len=*),parameter :: dir2 = 'files/inputs/'      !! working directory
    character(len=*),parameter :: filename36 = 'test36.json'  !! input filename

contains

    subroutine test_36(error_cnt)

    !! Github issue example: https://github.com/josephalevin/fson/issues/156
    !!
    !! Read a matrix

    implicit none

    integer,intent(out) :: error_cnt
    real(wp), dimension(:,:),allocatable :: dd
    real(wp), dimension(:,:,:),allocatable :: ddd
    integer, dimension(:),allocatable :: dd_size
    integer, dimension(:,:),allocatable :: ddd_size
    type(json_file) :: json
    logical :: found, file_exists

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 36'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') 'load file...'
    inquire(file=dir//filename36,exist=file_exists)
    if (file_exists) then
        call json%load_file(filename = dir//filename36)
    else
        inquire(file=dir2//filename36,exist=file_exists) !! cmake for VS integration places in different folder
        if (file_exists) call json%load_file(filename = dir2//filename36)
    end if
    if (json%failed()) then

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        ! print the parsed data to the console:
        write(error_unit,'(A)') 'print file...'
        call json%print_file(error_unit)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        ! extract data from the parsed value:
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'extract data...'

        write(error_unit,'(A)') '--------------------------'
        call json%get('fooList', dd, found, dd_size)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,I5)') 'dd = ',dd
        call json%get('fooList3x', ddd, found, ddd_size)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,I5)') 'ddd = ',ddd

        write(error_unit,'(A)') ''

    end if

    ! clean up
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_36

end module jf_test_36_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
!*****************************************************************************************
program jf_test_36

    !! Thirty sixth unit test.

    use jf_test_36_mod , only: test_36
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_36(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_36
!*****************************************************************************************
#endif
