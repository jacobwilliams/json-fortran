!*****************************************************************************************
!>
! Module for the 30th unit test.

module jf_test_30_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_30

contains

    subroutine test_30(error_cnt)

    !! Test the `escape_solidus` option.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    type(json_core) :: json_c
    integer :: i

    character(kind=CK,len=*),parameter :: str = CK_'{"vars":{"a/first":1,"a\/second":2,"a\\b\\c\/third/blah/blah":3}}'
    character(len=*),dimension(2),parameter :: tf = ['True ','False']

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 30'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    do i = 1, 2

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'escape_solidus = '//trim(tf(i))
        write(error_unit,'(A)') ''

        call json%initialize(escape_solidus=(i==1), stop_on_error=.true.)
        call json%deserialize(str)
        call json%print(int(error_unit,IK))

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        call json%destroy()

        write(error_unit,'(A)') ''

    end do

    ! do this one just for code coverage:
    call json_c%initialize(escape_solidus=.true.)
    if (json%failed()) then
        call json_c%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_30

end module jf_test_30_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_30

    !! 30th unit test.

    use jf_test_30_mod , only: test_30
    implicit none
    integer :: n_errors
    call test_30(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_30
!*****************************************************************************************
#endif
