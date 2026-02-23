!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2019
!
! Module for the 38th unit test.
!
!@note This test assumes the library is compiled
!      including `real32` (or greater) support.

module jf_test_38_mod

    use json_module, CK => json_CK, CDK => json_CDK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, wp => real32

    implicit none

    private
    public :: test_38

contains

    subroutine test_38(error_cnt)

    !! Test of support for `real32` interfaces.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_file)                   :: f
    type(json_value),pointer          :: p, p_value, p_vec
    type(json_core)                   :: json
    logical(LK)                       :: found
    real(wp)                          :: rval  !! a single-precision number
    real(wp),dimension(:),allocatable :: rvec  !! a single-precision vector

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 38'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    ! test the core routines:

    call json%initialize(no_whitespace=.true.)

    call json%deserialize(p, '{"a": 1.0}')

    call json%update(p,'a',2.0_wp,found)
    call json%update(p,CK_'a',2.0_wp,found)
    call json%update(p,CDK_'a',2.0_wp,found)

    call json%add_by_path(p,'b',3.0_wp)
    call json%add_by_path(p,'vec1',[1.0_wp, 2.0_wp, 3.0_wp])
    call json%add(p,'c',4.0_wp)
    call json%add(p,'vec2',[1.0_wp, 2.0_wp, 3.0_wp])

    call json%get(p, 'a', rval)
    if (rval /= 2.0) then
        write(error_unit,*) 'Error: 2.0 /= ', rval
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'vec1', rvec)
    if (any([1.0_wp, 2.0_wp, 3.0_wp]/=rvec)) then
        write(error_unit,*) 'Error: [1.0_wp, 2.0_wp, 3.0_wp] /= ', rvec
        error_cnt = error_cnt + 1
    end if

    call json%create_real(p_value,1.0_wp,'d')
    call json%get(p_value, rval)
    if (rval /= 1.0) then
        write(error_unit,*) 'Error: 1.0 /= ', rval
        error_cnt = error_cnt + 1
    end if

    call json%get(p, 'vec1', p_vec)
    call json%get(p_vec, rvec)
    if (any([1.0_wp, 2.0_wp, 3.0_wp]/=rvec)) then
        write(error_unit,*) 'Error: [1.0_wp, 2.0_wp, 3.0_wp] /= ', rvec
        error_cnt = error_cnt + 1
    end if

    call json%destroy(p)
    call json%destroy(p_value)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    ! test the json_file routines:

    call f%initialize(no_whitespace=.true.)

    call f%deserialize('{"a": 1.0}')

    call f%update('a',2.0_wp,found)
    call f%add('b',3.0_wp)
    call f%add('vec1',[1.0_wp, 2.0_wp, 3.0_wp])

    call f%get('a', rval)
    if (rval /= 2.0) then
        write(error_unit,*) 'Error: 2.0 /= ', rval
        error_cnt = error_cnt + 1
    end if

    call f%get('vec1', rvec)
    if (any([1.0_wp, 2.0_wp, 3.0_wp]/=rvec)) then
        write(error_unit,*) 'Error: [1.0_wp, 2.0_wp, 3.0_wp] /= ', rvec
        error_cnt = error_cnt + 1
    end if

    call f%destroy()

    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ' Success!'
    end if
    write(error_unit,'(A)') ''

    end subroutine test_38

end module jf_test_38_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_38

    !! 38th unit test.

    use jf_test_38_mod, only: test_38
    implicit none
    integer :: n_errors
    call test_38(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_38
!*****************************************************************************************
#endif
