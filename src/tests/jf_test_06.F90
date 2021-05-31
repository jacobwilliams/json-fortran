!*****************************************************************************************
!>
!  Module for the sixth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactored original json_example.f90 file)

module jf_test_6_mod

    use json_module, CK => json_CK, LK => json_LK
    use json_parameters, only: newline
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_6

    character(len=*),parameter :: dir = '../files/inputs/'   !! working directory

contains

    subroutine test_6(error_cnt)

    !! This example tries to read an invalid JSON file.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    integer :: i, j
    character(kind=CK,len=:),allocatable :: error_msg
    character(kind=CK,len=:),allocatable :: expected_error_msg
    logical(LK) :: status_ok

    character(len=*),dimension(5),parameter :: files = ['invalid.json ',&
                                                        'invalid2.json',&
                                                        'invalid3.json',&
                                                        'invalid4.json',&
                                                        '             ']

    character(len=*),parameter :: invalid_str = '{"a":1} "b": 2}'  !! invalid JSON string

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

    do i=1,size(files)

        ! parse the json file:
        write(error_unit,'(A)') ''
        if (files(i)=='') then
            write(error_unit,'(A)') 'load string: '//invalid_str
            write(error_unit,'(A)') ''
            call json%deserialize(str = invalid_str)
        else
            write(error_unit,'(A)') 'load file: '//trim(files(i))
            write(error_unit,'(A)') ''
            call json%load(filename = dir//trim(files(i)))
        end if
        if (json%failed()) then

            if (i==1) then
                call json%check_for_errors(status_ok, error_msg=error_msg)
                expected_error_msg = CK_'Error in parse_array: Unexpected character encountered when parsing array.'//newline//&
                                     CK_'line: 13, character: 1'//newline//&
                                     CK_'}'//newline//'^'
                if (error_msg /= expected_error_msg) then
                    ! verify that the expected error string is present
                    write(error_unit,'(A)') 'Error: unexpected error message string: "'//error_msg//'"'
                    write(error_unit,'(A)') ''
                    write(error_unit,*) 'len(error_msg)          = ', len(error_msg)
                    write(error_unit,*) 'len(expected_error_msg) = ', len(expected_error_msg)
                    do j = 1, min(len(error_msg), len(expected_error_msg))
                        if (error_msg(j:j) /= expected_error_msg(j:j)) then
                            write(error_unit,'(I3,1X,A,A,A,A,A)') j, '"', error_msg(j:j), '" /= "', expected_error_msg(j:j), '"'
                        else
                            write(error_unit,'(I3,1X,A,A,A,A,A)') j, '"', error_msg(j:j), '" == "', expected_error_msg(j:j), '"'
                        end if
                    end do
                    error_cnt = error_cnt + 1
                end if
            end if

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

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_6

    !! Sixth unit test.

    use jf_test_6_mod , only: test_6
    implicit none
    integer :: n_errors
    call test_6(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_6
!*****************************************************************************************
#endif
