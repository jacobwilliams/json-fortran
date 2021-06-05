!*****************************************************************************************
!>
! Module for the third unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_3_mod

    use json_module, wp => json_RK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_3

    character(len=*),parameter :: dir = '../files/inputs/' !! working directory
    character(len=*),parameter :: filename2 = 'test2.json'

contains

    subroutine test_3(error_cnt)

    !! Read the file generated in [[test_2]], and extract some data from it.

    implicit none

    integer,intent(out) :: error_cnt

    integer(IK) :: ival
    character(kind=json_CK,len=:),allocatable :: cval
    real(wp) :: rval
    type(json_file) :: json    !the JSON structure read from the file:
    integer :: i
    character(kind=json_CK,len=10) :: str
    real(wp),dimension(:),allocatable :: rvec
    type(json_core) :: core

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 3'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0
    call json%initialize(allow_duplicate_keys=.false.)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file: '//dir//filename2

    call json%load(filename = dir//filename2)

    if (json%failed()) then    !if there was an error reading the file

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'reading data from file...'
        !get scalars:
        write(error_unit,'(A)') ''
        call json%get('inputs.integer_scalar', ival)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A,1X,I5)') 'inputs.integer_scalar = ',ival
        end if
        !get one element from a vector:
        write(error_unit,'(A)') ''
        call json%get('trajectory(1).DATA(2)', rval)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A,1X,F30.16)') 'trajectory(1).DATA(2) = ',rval
        end if
        !get vectors:
        do i=1,4

            write(str,fmt='(I10)') i
            str = adjustl(str)

            write(error_unit,'(A)') ''
            call json%get('trajectory('//trim(str)//').VARIABLE', cval)
            if (json%failed()) then

                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1

            else

                write(error_unit,'(A)') 'trajectory('//trim(str)//').VARIABLE = '//trim(cval)

                !...get the vector using the callback method:
                call json%get('trajectory('//trim(str)//').DATA', rvec)
                if (json%failed()) then
                    call json%print_error_message(error_unit)
                    error_cnt = error_cnt + 1
                else
                    write(error_unit,'(A,1X,*(F30.16,1X))') 'trajectory('//trim(str)//').DATA = ',rvec
                end if

            end if

        end do

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'json_core tests...'
    ! test json_core manipulation:
    call core%initialize(trailing_spaces_significant=.true.,&
                         case_sensitive_keys=.true.)
    call json%initialize(core)  ! send it to the file
    call core%destroy()
    call json%get_core(core)    ! get it back
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_3

end module jf_test_3_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS

!*****************************************************************************************
program jf_test_3

    !! Third unit test.

    use jf_test_3_mod , only: test_3
    implicit none
    integer :: n_errors
    call test_3(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_3
!*****************************************************************************************
#endif
