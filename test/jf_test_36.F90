!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/15/2019
!
! Module for the 36th unit test.

module jf_test_36_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_36

contains

    subroutine test_36(error_cnt)

    !! Test writing a large JSON structure to a string.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file) :: my_file
    character(kind=json_CK,len=:),allocatable :: str_in, str_out
    integer :: i !! counter for number of test cases
    integer :: j !! counter
    integer :: n !! size of strings to append

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 36'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    do i = 1, 2

        do n = 0, 200

            str_in = ''
            str_out = ''

            ! create some long strings:
            ! [the idea here is to check the chunk
            !  code for different sized strings]
            select case (i)
            case (1)
                ! one big string
                str_in = '{"long_string":"' // repeat('a', 10000) //'"}'
            case (2:)
                ! a lot of little strings
                str_in = '{"big_array":['
                do j = 1, 1000
                    str_in = str_in // '"' // repeat('a', n) // '"'
                    if (j<1000) str_in = str_in // ','
                end do
                str_in = str_in//']}'
            end select

            ! don't print extra spaces, so the result will match the input exactly
            call my_file%initialize(no_whitespace=.true.)

            ! load from the original string:
            call my_file%deserialize(str_in)
            if (my_file%failed()) then
                call my_file%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            end if

            ! now, write it to a new string:
            call my_file%serialize(str_out)
            if (my_file%failed()) then
                call my_file%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            end if
            call my_file%destroy()

            ! verify that the strings are the same:
            if (str_in /= str_out) then
                write(error_unit,'(A,1X,I2,1X,I4)') ' Error: the strings are not the same for case', i, n
                error_cnt = error_cnt + 1
                if (n==2) then
                    write(error_unit,'(A)') ''
                    write(error_unit,'(A)') '----str_in:'
                    write(error_unit,'(A)') str_in
                    write(error_unit,'(A)') ''
                    write(error_unit,'(A)') '----str_out:'
                    write(error_unit,'(A)') str_out
                    write(error_unit,'(A)') ''
                    write(error_unit,'(A)') ''
                    error stop
                end if
            end if

            ! now load the string again to verify that it
            ! printed correctly without errors:
            call my_file%deserialize(str_out)
            if (my_file%failed()) then
                call my_file%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            end if

            if (i==1) exit ! only one for this case

        end do

    end do

    if (error_cnt==0) write(error_unit,'(A)') ' Success!'

    end subroutine test_36

end module jf_test_36_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_36

    !! 36th unit test.

    use jf_test_36_mod, only: test_36
    implicit none
    integer :: n_errors
    call test_36(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_36
!*****************************************************************************************
#endif
