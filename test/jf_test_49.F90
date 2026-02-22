!*****************************************************************************************
!>
!  Module for the 49th unit test.

module jf_test_49_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_49

contains

    subroutine test_49(error_cnt)

    !! 49th unit test. see Issue #488

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    integer :: i !! counter

    integer,parameter :: n_repeat = 1000 !! number of time to repeat the test
    character(kind=CK,len=*),parameter :: str = CK_'{"Substance":[]}' !! string to deserialize

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 49'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    do i = 1, n_repeat
        call json%deserialize(str)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
            call json%destroy()
            exit
        end if
        call json%destroy()
    end do

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success'
    else
        write(error_unit,'(A)') 'Failed'
    end if

    write(error_unit,'(A)') ''

    end subroutine test_49

end module jf_test_49_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_49

    use jf_test_49_mod , only: test_49

    implicit none
    integer :: n_errors

    call test_49(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_49
#endif
!*****************************************************************************************
