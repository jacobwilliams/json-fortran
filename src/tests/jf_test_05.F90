!*****************************************************************************************
!>
! Module for the fifth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_5_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_5

    character(len=*),parameter :: dir = '../files/inputs/'   !! working directory
    character(len=*),parameter :: filename5 = 'test5.json'

contains

    subroutine test_5(error_cnt)

    !! Github issue example: https://github.com/josephalevin/fson/issues/12
    !!
    !! Read an existing file and extract some variables.

    implicit none

    integer,intent(out) :: error_cnt

    integer(IK) :: vv
    integer(IK),dimension(:),allocatable :: vvv
    real(wp) :: d
    type(json_file) :: json
    logical(LK) :: found

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 5'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') 'load file...'
    call json%load(filename = dir//filename5)
    if (json%failed()) then

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        ! print the parsed data to the console:
        write(error_unit,'(A)') 'print file...'
        call json%print(int(error_unit,IK))
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        ! extract data from the parsed value:
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'extract data...'

        write(error_unit,'(A)') '--------------------------'
        call json%get('Correl.ID2', vv, found)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,I5)') 'vv = ',vv

        call json%get('Correl.ID1', vvv, found)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,*(I5,1X))') 'vvv= ',vvv

        call json%get('Prior[3].mode', d, found)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (found) write(error_unit,'(A,E30.16)') 'd  = ',d

        write(error_unit,'(A)') ''

    end if

    ! clean up
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_5

end module jf_test_5_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_5

    !! Fifth unit test.

    use jf_test_5_mod , only: test_5
    implicit none
    integer :: n_errors
    call test_5(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_5
!*****************************************************************************************
#endif
