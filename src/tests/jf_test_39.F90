!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/26/2019
!
!  Module for the 39th unit test.

module jf_test_39_mod

    use json_module, CK => json_CK, CDK => json_CDK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_39

contains

    subroutine test_39(error_cnt)

    !! Test of some weird (but valid) JSON structures

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file) :: json
    integer :: i !! counter

    character(kind=CK,len=*),dimension(6),parameter :: tests = ['"42" ',&
                                                                '[42] ',&
                                                                '42   ',&
                                                                'true ',&
                                                                'false',&
                                                                'null ' ]

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 39'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    do i = 1, size(tests)

        json = json_file(trim(tests(i)),verbose=.true.,stop_on_error=.true.)
        call json%print(int(error_unit,IK))
        write(error_unit,'(A)') ''
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
            write(error_unit,'(A)') ' FAILED!'
        else
            write(error_unit,'(A)') ' Success!'
        end if
        call json%destroy()
        write(error_unit,'(A)') ''

    end do

    end subroutine test_39

end module jf_test_39_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_39

    !! 39th unit test.

    use jf_test_39_mod, only: test_39
    implicit none
    integer :: n_errors
    call test_39(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_39
!*****************************************************************************************
#endif
