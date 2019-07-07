!*****************************************************************************************
!>
! Module for the 42nd unit test

module jf_test_42_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_42

contains

    subroutine test_42(error_cnt)

    !! Test of NaN and Infinity

    implicit none

    type(json_file) :: json          !! the JSON structure read from the file
    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{"bad_reals": [1.0, "NaN", "+Infinity", "-Infinity", 4.0]}'

    real(rk),dimension(:),allocatable :: bad_reals
    logical(lk) :: found

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 42'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    ! parse the json string:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing string... '
    call json%load_from_string(str)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'printing...'
    call json%print_file(int(error_unit,IK))

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'get values as real...'
    call json%get('bad_reals',bad_reals,found)

    if (json%failed()) then    !if there was an error reading the file

        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        write(error_unit,'(A)') 'printing...'
        write(error_unit,*) bad_reals
        write(error_unit,'(A)') ''

    end if

    call json%destroy()

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'now add back as a real...'
    call json%add('bad_reals', bad_reals)
    call json%print_file(int(error_unit,IK))
    write(error_unit,'(A)') ''

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if

    end subroutine test_42

end module jf_test_42_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_42

    !! 42nd unit test.

    use jf_test_42_mod , only: test_42
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_42(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_42
!*****************************************************************************************
#endif
