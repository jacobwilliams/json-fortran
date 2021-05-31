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

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{'//&
            CK_'"bad_reals": [1.0, null, "NaN", "+Infinity", "-Infinity", 4.0],'//&
            CK_'"nonstandard_json": [.1e1, .1D1, .1d+1, +.1d1, +.1D1, +1.0, +1.0d0, +1.0D0]'//&
            CK_'}'

    type(json_file) :: json !! the JSON structure read from the file
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
    call json%deserialize(str)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'printing...'
    call json%print(int(error_unit,IK))

    call json%initialize(use_quiet_nan=.false., null_to_real_mode=2_IK) ! signaling nan

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'get values as real [signaling nan]...'
    call json%get('bad_reals',bad_reals,found)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'printing...'
        write(error_unit,*) bad_reals
        write(error_unit,'(A)') ''

        call json%initialize(null_to_real_mode=3_IK) ! 0.0 nan

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'get values as real [nan as 0.0]...'
        call json%get('bad_reals',bad_reals,found)

        write(error_unit,'(A)') 'printing...'
        write(error_unit,*) bad_reals
        write(error_unit,'(A)') ''

        call json%initialize(use_quiet_nan=.true., null_to_real_mode=2_IK) ! quiet nan

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'get values as real [quiet nan]...'
        call json%get('bad_reals',bad_reals,found)

        write(error_unit,'(A)') 'printing...'
        write(error_unit,*) bad_reals
        write(error_unit,'(A)') ''

    end if

    call json%destroy()

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'now add back as a real...'
    call json%add('bad_reals', bad_reals)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'serialize as strings:'
    call json%initialize(non_normal_mode=1_IK)
    call json%print(int(error_unit,IK))

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'serialize as null:'
    call json%initialize(non_normal_mode=2_IK)
    call json%print(int(error_unit,IK))
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
    call test_42(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_42
!*****************************************************************************************
#endif
