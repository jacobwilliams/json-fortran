module jf_test_26_mod

    use json_module, rk=>json_rk, ik=>json_ik, ck=>json_ck, cdk=>json_cdk, lk=>json_lk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_26

contains

    subroutine test_26(error_cnt)

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p,tmp
    type(json_file) :: f
    logical(lk) :: is_valid
    character(kind=CK,len=:),allocatable :: error_msg

    error_cnt = 0

    call f%initialize()  ! specify whatever init options you want.

    write(error_unit,'(A)') 'adding data to json_file...'

    call f%add('inputs.name',     'test case')
    call f%add('inputs.r',        1.0_rk)
    call f%add('inputs.rvec',     [1.0_rk,2.0_rk,3.0_rk])
    call f%add('inputs.i',        1_ik)
    call f%add('inputs.ivec',     [1_ik,2_ik,3_ik])
    call f%add('inputs.l',       .true.)
    call f%add('inputs.lvec',    [ .true. , .false. ] )
    call f%add(ck_'test.scalar.unicode(1)',  ck_'ck, ck')
    call f%add(ck_'test.scalar.unicode(2)',  cdk_'ck, cdk')
    call f%add(cdk_'test.scalar.unicode(3)', ck_'cdk, ck')
    call f%add(cdk_'test.scalar.unicode(4)', cdk_'cdk, cdk')
    call f%add(ck_'test.vector.unicode(1)',  [ck_'ck, ck'])
    call f%add(ck_'test.vector.unicode(2)',  [cdk_'ck, cdk'])
    call f%add(cdk_'test.vector.unicode(3)', [ck_'cdk, ck'])
    call f%add(cdk_'test.vector.unicode(4)', [cdk_'cdk, cdk'])

    !add a json_value pointer:
    call json%create_integer(tmp,999_IK,'') ! note that the name will be replaced
                                            ! with the name given in the path
                                            ! when it is added.
    call f%add('inputs.pointer',tmp)

    write(error_unit,'(A)') 'validating...'
    call f%get(p)
    call json%validate(p,is_valid,error_msg)
    if (.not. is_valid) then
        write(error_unit,'(A)') 'JSON Validation Error: '//error_msg
        error_cnt = error_cnt + 1
        deallocate(error_msg)
    else
        write(error_unit,'(A)') '...Success!'
    end if

    write(error_unit,'(A)') 'printing...'
    call f%print()

    end subroutine test_26

end module jf_test_26_mod

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_26

    !! 26th unit test.

    use jf_test_26_mod , only: test_26
    implicit none
    integer :: n_errors

    call test_26(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_26
!*****************************************************************************************
#endif
