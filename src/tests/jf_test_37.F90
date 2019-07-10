!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/20/2019
!
! Module for the 37th unit test.

module jf_test_37_mod

    use json_module, CK => json_CK, CDK => json_CDK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_37

contains

    subroutine test_37(error_cnt)

    !! Test of `json_file` constructor functions.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file)          :: f
    type(json_value),pointer :: p
    type(json_core)          :: json

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 37'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%initialize(no_whitespace=.true.)

    call json%deserialize(p, CK_'{"a": ["1", "2", "3"]}')
    f = json_file(p,no_whitespace=.true.)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    call json%deserialize(p, CK_'{"b": ["4", "5", "6"]}')
    f = json_file(p,json)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    f = json_file(CK_'{"x": [1,2,3]}',no_whitespace=.true.)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    f = json_file(CK_'{"y": [4,5,6]}',json)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

# ifdef USE_UCS4

    ! also test default character kind when unicode is enabled:

    call json%deserialize(p, CDK_'{"a": ["1", "2", "3"]}')
    f = json_file(p,no_whitespace=.true.)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    call json%deserialize(p, CDK_'{"b": ["4", "5", "6"]}')
    f = json_file(p,json)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    f = json_file(CDK_'{"x": [1,2,3]}',no_whitespace=.true.)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

    f = json_file(CDK_'{"y": [4,5,6]}',json)
    call f%print(int(error_unit,IK))
    write(error_unit,'(A)') ''
    call check_for_error()
    call f%destroy()

# endif

    if (error_cnt==0) then
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ' Success!'
    end if
    write(error_unit,'(A)') ''

    contains

    subroutine check_for_error()

    implicit none

    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine check_for_error

    end subroutine test_37

end module jf_test_37_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_37

    !! 37th unit test.

    use jf_test_37_mod, only: test_37
    implicit none
    integer :: n_errors
    call test_37(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_37
!*****************************************************************************************
#endif
