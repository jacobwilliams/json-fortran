!*****************************************************************************************
!>
!  Module for the eighth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactored original json_example.f90 file)

module jf_test_8_mod

    use json_module, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_8

contains

    subroutine test_8(error_cnt)

    !! read a JSON structure from a string

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p
    type(json_core) :: json       !! factory for manipulating `json_value` pointers

    character(len=*),parameter :: newline = achar(10)

    character(len=*),parameter :: str = '{ "label": "foo",'//newline//' "value": "bar" }'

    character(len=*),parameter :: str2 = '{ "label": "foo",'//newline//&
                                         '  "value": "bar",'//newline//&
                                         '  "empty_array": [],'//newline//&
                                         '  "empty_object": {}' //newline//&
                                         '}'

    character(len=*),parameter :: str_invalid = '{ "label": "foo",'//newline//' "value : "bar" }'

    error_cnt = 0
    call json%initialize(allow_duplicate_keys=.false.)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 8 : read JSON from string'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Valid test 1:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json%deserialize(str=str, p=p)   ! read it from str
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '{ "part a" : '
    call json%print(p,int(output_unit,IK))  ! print to console
    write(output_unit,'(A)') ','
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%destroy(p)            ! cleanup
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Valid test 2:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json%deserialize(str=str2, p=p)   ! read it from str
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '"part b" : '
    call json%print(p,int(output_unit,IK))  ! print to console
    write(output_unit,'(A)') ','
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%destroy(p)            ! cleanup
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Invalid test:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json%deserialize(str=str_invalid, p=p)   ! read it from str
    if (json%failed()) then
        call json%print_error_message(error_unit)
    else
        write(error_unit,'(A)') 'This should have failed!'
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '"part c" : '
    call json%print(p,int(output_unit,IK))  ! print to console
    write(output_unit,'(A)') '}'
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%destroy(p)            ! cleanup
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    end subroutine test_8

end module jf_test_8_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_8

    !! Eighth unit test.

    use jf_test_8_mod , only: test_8
    implicit none
    integer :: n_errors
    call test_8(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_8
!*****************************************************************************************
#endif
!*******************************************************************************************************
