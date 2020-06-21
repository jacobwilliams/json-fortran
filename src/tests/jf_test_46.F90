!*****************************************************************************************
!>
! Module for the 46th unit test

module jf_test_46_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_46

contains

    subroutine test_46(error_cnt)

    !! testing of default optional argument

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{"x": 1}'

    type(json_core) :: json
    type(json_file) :: json_f
    type(json_value),pointer :: p
    logical(LK) :: found
    integer(IK) :: ival
    real(RK) :: rval
    real :: r32val
    logical(LK) :: lval
    character(kind=CK,len=:),allocatable :: cval
    character(kind=CK,len=1),dimension(:),allocatable :: cvec

    character(kind=CK,len=1),dimension(1) :: cvec_default = [CK_'1']

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 46'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! note: don't have one for json_get_alloc_string_vec_by_path

    error_cnt = 0

    !---------------------------------
    ! first core routines:
    !---------------------------------

    call json%deserialize(p,str)

    ! unicode:
    call json%get(p, CK_'not_there', ival, found, default=99_IK)
    if (json%failed() .or. found .or. ival /= 99_IK) then
        write(error_unit,'(A)') 'Error using json_get_integer_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, CK_'not_there', r32val, found, default=99.0)  ! real32
    if (json%failed() .or. found .or. r32val-99.0>0.0) then
        write(error_unit,'(A)') 'Error using json_get_real_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, CK_'not_there', rval, found, default=99.0_RK)
    if (json%failed() .or. found .or. rval-99.0_RK>0.0_RK) then
        write(error_unit,'(A)') 'Error using json_get_real_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, CK_'not_there', lval, found, default=.true.)
    if (json%failed() .or. found .or. lval .neqv. .true.) then
        write(error_unit,'(A)') 'Error using json_get_logical_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, CK_'not_there', cval, found, default=CK_'default')
    if (json%failed() .or. found .or. cval /= CK_'default') then
        write(error_unit,'(A)') 'Error using json_get_string_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, CK_'not_there', cvec, found, default=cvec_default)
    if (json%failed() .or. found .or. all(cvec /= cvec_default)) then
        write(error_unit,'(A)') 'Error using json_get_string_vec_by_path default'
        error_cnt = error_cnt + 1
    end if

    ! default:
    call json%get(p, 'not_there', ival, found, default=99_IK)
    if (json%failed() .or. found .or. ival /= 99_IK) then
        write(error_unit,'(A)') 'Error using json_get_integer_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'not_there', rval, found, default=99.0_RK)
    if (json%failed() .or. found .or. rval-99.0_RK>0.0_RK) then
        write(error_unit,'(A)') 'Error using json_get_real_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'not_there', lval, found, default=.true.)
    if (json%failed() .or. found .or. lval .neqv. .true.) then
        write(error_unit,'(A)') 'Error using json_get_logical_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'not_there', cval, found, default=CK_'default')
    if (json%failed() .or. found .or. cval /= CK_'default') then
        write(error_unit,'(A)') 'Error using json_get_string_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'not_there', cvec, found, default=[CK_'1'])
    if (json%failed() .or. found .or. all(cvec /= [CK_'1'])) then
        write(error_unit,'(A)') 'Error using json_get_string_vec_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json%destroy(p)

    !---------------------------------
    ! now, json_file routines:
    !---------------------------------

    json_f = json_file(str)

    ! unicode:
    call json_f%get(CK_'not_there', ival, found, default=99_IK)
    if (json%failed() .or. found .or. ival /= 99_IK) then
        write(error_unit,'(A)') 'Error using json_get_integer_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get(CK_'not_there', rval, found, default=99.0_RK)
    if (json%failed() .or. found .or. rval-99.0_RK>0.0_RK) then
        write(error_unit,'(A)') 'Error using json_get_real_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get(CK_'not_there', lval, found, default=.true.)
    if (json%failed() .or. found .or. lval .neqv. .true.) then
        write(error_unit,'(A)') 'Error using json_get_logical_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get(CK_'not_there', cval, found, default=CK_'default')
    if (json%failed() .or. found .or. cval /= CK_'default') then
        write(error_unit,'(A)') 'Error using json_get_string_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get(CK_'not_there', cvec, found, default=cvec_default)
    if (json%failed() .or. found .or. all(cvec /= cvec_default)) then
        write(error_unit,'(A)') 'Error using json_get_string_vec_by_path default'
        error_cnt = error_cnt + 1
    end if

    ! default:
    call json_f%get('not_there', ival, found, default=99_IK)
    if (json%failed() .or. found .or. ival /= 99_IK) then
        write(error_unit,'(A)') 'Error using json_get_integer_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get('not_there', rval, found, default=99.0_RK)
    if (json%failed() .or. found .or. rval-99.0_RK>0.0_RK) then
        write(error_unit,'(A)') 'Error using json_get_real_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get('not_there', lval, found, default=.true.)
    if (json%failed() .or. found .or. lval .neqv. .true.) then
        write(error_unit,'(A)') 'Error using json_get_logical_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get('not_there', cval, found, default=CK_'default')
    if (json%failed() .or. found .or. cval /= CK_'default') then
        write(error_unit,'(A)') 'Error using json_get_string_by_path default'
        error_cnt = error_cnt + 1
    end if

    call json_f%get('not_there', cvec, found, default=cvec_default)
    if (json%failed() .or. found .or. all(cvec /= cvec_default)) then
        write(error_unit,'(A)') 'Error using json_get_string_vec_by_path default'
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if
    write(error_unit,'(A)') ''

    end subroutine test_46

end module jf_test_46_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_46

    !! 46th unit test.

    use jf_test_46_mod , only: test_46
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_46(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_46
!*****************************************************************************************
#endif