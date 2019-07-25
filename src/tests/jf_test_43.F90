!*****************************************************************************************
!>
! Module for the fourtieth unit test.
!
!# HISTORY
!  * Ian Porter : 8/14/2018

module jf_test_40_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    private
    public :: test_40

    character(len=*),parameter :: dir  = '../files/inputs/'   !! working directory
    character(len=*),parameter :: dir2 = 'files/inputs/'      !! working directory
    character(len=*),parameter :: filename40 = 'test40.json'  !! input filename

contains

    subroutine test_40(error_cnt)

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
    write(error_unit,'(A)') '   EXAMPLE 40'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') 'load file...'
    inquire(file=dir//filename40,exist=file_exists)
    if (file_exists) then
        call json%load_file(filename = dir//filename40)
    else
        inquire(file=dir2//filename40,exist=file_exists) !! cmake for VS integration places in different folder
        if (file_exists) call json%load_file(filename = dir2//filename40)
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
! TODO: Implement this
!        call json%get('fooList', dd, found, dd_size)
!        if (json%failed()) then
!            call json%print_error_message(error_unit)
!            error_cnt = error_cnt + 1
!        end if
!        if (found) write(error_unit,'(A,I5)') 'dd = ',dd
        call json%get('fooList3x', ddd, found, ddd_size)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,es13.6)') 'ddd = ',ddd

        write(error_unit,'(A)') ''

    end if

    ! clean up
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

  end subroutine test_40

end module jf_test_40_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
!*****************************************************************************************
program jf_test_40

    !! Thirty sixth unit test.

    use jf_test_40_mod , only: test_40
    implicit none
    integer :: n_errors

    call test_40(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_40
!*****************************************************************************************
#endif
