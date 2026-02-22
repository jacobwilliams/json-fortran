!*****************************************************************************************
!>
! Module for the 24rd unit test.

module jf_test_24_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_24

contains

    subroutine test_24(error_cnt)

    !! Test creating a JSON structure using paths.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p
    type(json_core) :: json
    logical(lk) :: found
    logical(lk) :: was_created
    logical(lk) :: is_valid
    character(kind=CK,len=:),allocatable :: error_msg
    type(json_value),pointer :: tmp !! a temp pointer

    error_cnt = 0
    call json%initialize( verbose=.false. )
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 24'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'creating...'

    call json%create_object(p,'root')

    call json%create_real(tmp,99.9_rk,'double') !  (note: it gets renamed when added below)

    call json%create(p,'a'          )
    call json%create(p,'b'          )
    call json%create(p,'c'          )
    call json%create(p,'a.array'    )
    call json%create(p,'a.array(2)' )
    call json%create(p,     'b.null',                      found=found)  ! add a null to keep
    call json%add_by_path(p,'b.bb'              , 1.0_rk , found)
    call json%add_by_path(p,'b.dble'            , tmp    , found)  ! add a json_value pointer
    call json%add_by_path(p,'c.ccc'             , 2_ik   , found)
    call json%add_by_path(p,'a.aa.aaa(1)'       , '3.0'  , found)
    call json%add_by_path(p,'a.aa.aaaa(3)'      , 4.0_rk , found)
    call json%add_by_path(p,'a.array(1)'        , 5_ik   , found)
    call json%add_by_path(p,'a.array(2).scalar' , '6'    , found)

    call json%add_by_path(p,'a.array(2).ivec' , [1_ik,2_ik,3_ik], found)
    call json%add_by_path(p,'a.array(2).rvec' , [1.0_rk,2.0_rk,3.0_rk], found)
    call json%add_by_path(p,'a.array(2).lvec' , [.true.,.false.,.true.], found)
    call json%add_by_path(p,'a.array(2).cvec' , ['1 ','2 ','3 '], found)
    call json%add_by_path(p,'a.array(2).cvec_trim' , ['1 ','2 ','3 '], found, ilen=[1_ik,1_ik,1_ik])

    call json%add_by_path(p,'a.array(2).logical', .true. , found, was_created)

#ifdef USE_UCS4
    call json%create(p,'a.unicode_test')
    call json%add_by_path(p,   CDK_'a.unicode_test.cdk_ck',  CK_'ck'   , found)
    call json%add_by_path(p,    CK_'a.unicode_test.ck_ck',   CK_'ck'   , found)
    call json%add_by_path(p,    CK_'a.unicode_test.ck_cdk',  CDK_'cdk' , found)
    call json%add_by_path(p,   CDK_'a.unicode_test.cdk_cdk', CDK_'cdk' , found)
    call json%add_by_path(p,   CDK_'a.unicode_test.cvec.cdk_ck' , [CK_'1',CK_'2',CK_'3'],   found)
    call json%add_by_path(p,    CK_'a.unicode_test.cvec.ck_ck' ,  [CK_'1',CK_'2',CK_'3'],   found)
    call json%add_by_path(p,    CK_'a.unicode_test.cvec.ck_cdk' , [CDK_'1',CDK_'2',CDK_'3'],found)
    call json%add_by_path(p,   CDK_'a.unicode_test.cvec.cdk_cdk' ,[CDK_'1',CDK_'2',CDK_'3'],found)

    ! also test the unicode operators:
    if (ck_'1' == cdk_'1' .and. &
        ck_'1' /= cdk_'2' .and. &
        cdk_'1' /= ck_'2' ) then
        write(error_unit,'(A)') 'Unicode operator tests passed'
    end if
#endif

    if (.not. was_created) then
        write(error_unit,'(A)') 'Error: variable should have been created.'
        error_cnt = error_cnt + 1
    end if

    ! now for variables that are already present:
    call json%add_by_path(p,'a.aa.aaaa(3)'      , 40.0_rk , found)
    call json%add_by_path(p,'a.array(1)'        , 50_ik   , found)
    call json%add_by_path(p,'a.array(2).scalar' , '60'    , found)
    call json%add_by_path(p,'a.array(2).logical', .false. , found, was_created)

    if (was_created) then
        write(error_unit,'(A)') 'Error: variable should already have been present.'
        error_cnt = error_cnt + 1
    end if

    write(*,*) ''
    write(*,*) 'JSONPath bracket mode'
    call json%initialize(path_mode=3_IK)

    write(*,'(A)') '$["jsonpath"]'
    call json%create(p, '$["jsonpath"]')

    write(*,'(A)') '$["jsonpath"]["array"][2]'
    call json%create(p, '$["jsonpath"]["array"][2]')

    write(*,'(A)') '$["jsonpath"]["a"]["aa"]["aaa"][1]'
    call json%add_by_path(p,'$["jsonpath"]["a"]["aa"]["aaa"][1]', '3.0' , found)

    write(*,'(A)') '$["jsonpath"]["a"]["aa"]["aaa"][3]'
    call json%add_by_path(p,'$["jsonpath"]["a"]["aa"]["aaa"][3]', 4.0_rk, found, was_created)

    if (.not. was_created) then
        write(error_unit,'(A)') 'Error: JSONPath test failed.'
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') 'validating...'
    call json%validate(p,is_valid,error_msg)
    if (.not. is_valid) then
        write(error_unit,'(A)') 'JSON Validation Error: '//error_msg
        error_cnt = error_cnt + 1
        deallocate(error_msg)
    end if

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'printing...'

    call json%print(p,int(output_unit,IK))

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy(p)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_24

end module jf_test_24_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_24

    !! 24rd unit test.

    use jf_test_24_mod , only: test_24
    implicit none
    integer :: n_errors
    call test_24(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_24
!*****************************************************************************************
#endif
