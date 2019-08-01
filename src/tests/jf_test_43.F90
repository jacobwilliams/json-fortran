!*****************************************************************************************
!>
! Module for the forty-third unit test.
! Check ability to read and write (and query) ragged edge matrices
!
!# HISTORY
!  * Ian Porter : 8/14/2018
!  * Izaak Beekman : 7/17/2019

module jf_test_43_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_43

    character(len=*),parameter :: dir  = '../files/inputs/'   !! working directory
    character(len=*),parameter :: dir2 = 'files/inputs/'      !! working directory
    character(len=*),parameter :: filename43 = 'test43.json'  !! input filename

contains

    subroutine test_43(error_cnt)

    !! Read a ragged edge matrix

    implicit none

    integer,intent(out) :: error_cnt
    real(wp),    dimension(:,:),allocatable :: dd
    integer(IK), dimension(:,:),allocatable :: imtx
    logical,     dimension(:,:),allocatable :: lmtx
    integer, dimension(:),allocatable :: dd_size, imtx_size, lmtx_size
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
    write(error_unit,'(A)') '   EXAMPLE 43'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') 'load file...'
    inquire(file=dir//filename43,exist=file_exists)
    if (file_exists) then
        call json%load_file(filename = dir//filename43)
    else
        inquire(file=dir2//filename43,exist=file_exists) !! cmake for VS integration places in different folder
        if (file_exists) call json%load_file(filename = dir2//filename43)
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
       call json%get('ragged_matrix', dd, found, dd_size)
       if (json%failed()) then
           call json%print_error_message(error_unit)
           error_cnt = error_cnt + 1
       end if
       if (found) write(error_unit,'(A,es7.5)') 'dd = ',dd

       ! call json%get('integer_matrix', imtx, found, imtx_size)
       ! if (json%failed()) then
       !     call json%print_error_message(error_unit)
       !     error_cnt = error_cnt + 1
       ! end if
       ! if (found) write(error_unit,'(A,I5)') 'imtx = ',imtx

       ! call json%get('logical_matrix', lmtx, found, lmtx_size)
       ! if (json%failed()) then
       !     call json%print_error_message(error_unit)
       !     error_cnt = error_cnt + 1
       ! end if
       ! if (found) write(error_unit,*) 'lmtx = ',lmtx

       write(error_unit,'(A)') ''

   end if

   ! clean up
   call json%destroy()
   if (json%failed()) then
       call json%print_error_message(error_unit)
       error_cnt = error_cnt + 1
   end if

end subroutine test_43

end module jf_test_43_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
!*****************************************************************************************
program jf_test_43

    !! Forty third unit test.

    use jf_test_43_mod , only: test_43
    implicit none
    integer :: n_errors

    call test_43(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_43
!*****************************************************************************************
#endif
