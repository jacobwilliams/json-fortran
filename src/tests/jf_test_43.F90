!*****************************************************************************************
!>
! Module for the 43nd unit test

module jf_test_43_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_43

contains

    subroutine test_43(error_cnt)

    !! integer too large for default integer kind

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{ "x": 2147483648 }'

    type(json_file) :: json
    logical :: found
    real(RK) :: x

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 43'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    ! enable the feature to try to parse an int
    ! as a real if it fails to be parsed as an int
    call json%initialize(strict_integer_type_checking=.false.)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'JSON string to parse... '
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') str

    ! parse the json string:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing string... '
    call json%deserialize(str)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'printing...'
    call json%print(int(error_unit,IK))
    write(error_unit,'(A)') ''

    call json%get('x',x,found)
    if (found) then
        write(error_unit,'(A,1X,F32.17)') 'x = ', x
    else
        write(error_unit,'(A)') 'Error: x not found in string'
        error_cnt = error_cnt + 1
    end if

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if

    end subroutine test_43

end module jf_test_43_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_43

    !! 43nd unit test.

    use jf_test_43_mod , only: test_43
    implicit none
    integer :: n_errors
    call test_43(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_43
!*****************************************************************************************
#endif




