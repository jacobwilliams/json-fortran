!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/30/2016
!
! Module for the 18th unit test.
! Test the name matching options.

module jf_test_18_mod

    use json_module, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_18

contains

    subroutine test_18(error_cnt)

    !! Test the name matching options.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p
    integer(IK) :: ival
    logical(LK) :: found
    logical(LK),dimension(4) :: ok
    integer(IK),dimension(4) :: iresult

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 18'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%deserialize(p,'{ "a" :{"val"  : 1},'//&
                             ' "A" :{"Val"  : 2},'//&
                             ' "a ":{"val  ": 3},'//&
                             ' "A ":{"Val  ": 4} }' )
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''
    call json%print(p,int(error_unit,IK))
    write(error_unit,'(A)') ''

    call json%initialize(trailing_spaces_significant=.true.,&
                         case_sensitive_keys=.true.)
    iresult = [1,2,3,4]
    call go(iresult)

    call json%initialize(trailing_spaces_significant=.false.,&
                         case_sensitive_keys=.true.)
    iresult = [1,2,1,2]
    call go(iresult)

    call json%initialize(trailing_spaces_significant=.true.,&
                         case_sensitive_keys=.false.)
    iresult = [1,1,3,3]
    call go(iresult)

    call json%initialize(trailing_spaces_significant=.false.,&
                         case_sensitive_keys=.false.)
    iresult = [1,1,1,1]
    call go(iresult)

    !cleanup:
    call json%destroy(p)

    contains

    subroutine go(iresult)

        !! run test and get results

        implicit none

        integer(IK),dimension(4),intent(in) :: iresult !! correct answers

        call json%get(p,'a.val',   ival,found); ok(1) = ival==iresult(1)
        call json%get(p,'A.Val',   ival,found); ok(2) = ival==iresult(2)
        call json%get(p,'a .val  ',ival,found); ok(3) = ival==iresult(3)
        call json%get(p,'A .Val  ',ival,found); ok(4) = ival==iresult(4)
        write(error_unit,'(A)') ''
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        if (all(ok)) then
            write(error_unit,'(A)') 'Test passed!'
        else
            write(error_unit,*) ok
            error_cnt = error_cnt + 1
            write(error_unit,'(A)') 'Test failed!'
        end if

    end subroutine go

    end subroutine test_18

end module jf_test_18_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_18

    !! 18th unit test.

    use jf_test_18_mod, only: test_18
    implicit none
    integer :: n_errors
    call test_18(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_18
!*****************************************************************************************
#endif
