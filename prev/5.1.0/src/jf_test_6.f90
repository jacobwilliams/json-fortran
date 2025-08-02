!*****************************************************************************************
!>
!  Module for the sixth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_6_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/inputs/'   !! working directory

contains

    subroutine test_6(error_cnt)

    !! This example tries to read an invalid JSON file.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    integer :: i

    character(len=*),dimension(2),parameter :: files = ['invalid.json ',&
                                                        'invalid2.json']

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 6 : invalid JSON files'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    do i=1,2

        ! parse the json file:
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'load file: '//trim(files(i))
        write(error_unit,'(A)') ''
        call json%load_file(filename = dir//trim(files(i)))
        if (json%failed()) then
            call json%print_error_message(error_unit)
        else
            write(error_unit,'(A)') 'An error should have been raised!'
            error_cnt = error_cnt + 1
        end if
        ! clean up
        call json%destroy()
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
    end do

    end subroutine test_6

end module jf_test_6_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_6

    !! Sixth unit test.

    use jf_test_6_mod , only: test_6
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_6(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_6
!*****************************************************************************************
