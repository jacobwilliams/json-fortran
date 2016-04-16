!*****************************************************************************************
!>
! Module for the eighth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_8_mod

    use json_kinds
    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

contains

    subroutine test_8(error_cnt)

    !! read a JSON structure from a string

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p

    character(len=*),parameter :: newline = achar(10)

    character(len=*),parameter :: str = '{ "label": "foo",'//newline//' "value": "bar" }'

    character(len=*),parameter :: str2 = '{ "label": "foo",'//newline//&
                                         '  "value": "bar",'//newline//&
                                         '  "empty_array": [],'//newline//&
                                         '  "empty_object": {}' //newline//&
                                         '}'

    character(len=*),parameter :: str_invalid = '{ "label": "foo",'//newline//' "value : "bar" }'

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    call json_parse(str=str, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '{ "part a" : '
    call json_print(p,output_unit)  ! print to console
    write(output_unit,'(A)') ','
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Valid test 2:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json_parse(str=str2, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '"part b" : '
    call json_print(p,output_unit)  ! print to console
    write(output_unit,'(A)') ','
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Invalid test:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json_parse(str=str_invalid, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
    else
        write(error_unit,'(A)') 'This should have failed!'
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') '"part c" : '
    call json_print(p,output_unit)  ! print to console
    write(output_unit,'(A)') '}'
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    end subroutine test_8

end module jf_test_8_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_8

    !! Eighth unit test.

    use jf_test_8_mod , only: test_8
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_8(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_8
!*****************************************************************************************

!*******************************************************************************************************
