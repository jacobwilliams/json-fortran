!*****************************************************************************************
!> author: Jacob Williams
!
!  Module for the 40th unit test.
!
!  This tests to make sure `stop_on_error` doesn't stop the program
!  for internal exceptions that are not really errors (in this case,
!  when creating structures that don't yet exist).

module jf_test_40_mod

    use json_module, CK => json_CK, CDK => json_CDK, RK => json_RK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_40

contains

    subroutine test_40(error_cnt)

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file) :: json

    error_cnt = 0

    call json%initialize(stop_on_error=.true., verbose=.true.)
    call json%add('doesnt.existyet', 0.0_rk)

    ! the exceptions shoule have been cleared:
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%print()
    call json%destroy()

    if (.not. json%failed() .and. error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Test Failed!'
    end if

    end subroutine test_40

end module jf_test_40_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_40

    !! 40th unit test.

    use jf_test_40_mod, only: test_40
    implicit none
    integer :: n_errors
    call test_40(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_40
!*****************************************************************************************
#endif
