!*****************************************************************************************
!>
! Module for the 25th unit test.

module jf_test_25_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_25

contains

    subroutine test_25(error_cnt)

    !! Test the allocatable string vector routines.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p, tmp
    type(json_core) :: json
    type(json_file) :: f
    logical(lk) :: found
    character(kind=CK,len=:),dimension(:),allocatable :: vec  !! array of strings from JSON
    integer(ik),dimension(:),allocatable :: ilen  !! array of string lengths

    character(kind=CK,len=*),parameter :: json_str = &
            '{"str_array": ["1","22","333","55555"]}'

    error_cnt = 0
    call json%initialize( verbose=.false. )
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 25'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing...'
    call json%deserialize(p,json_str)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'printing...'
    call json%print(p,int(output_unit,IK))

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'getting data...'

    ! get child, then array:
    call json%get_child(p,'str_array',tmp)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%get(tmp, vec, ilen)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    if (allocated(vec) .and. allocated(ilen)) then
        if (all(ilen==[1,2,3,5])) then
            write(error_unit,'(A)') 'success!'
        else
            write(error_unit,'(A,1X,*(I5,1X))') 'failed: ', ilen
            error_cnt = error_cnt + 1
        end if
    else
        write(error_unit,'(A)') 'failed: vectors not allocated.'
        error_cnt = error_cnt + 1
    end if

    ! try get by path:
    call json%get(p, 'str_array', vec, ilen, found)
    if (.not. found) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (all(ilen==[1,2,3,5])) then
            write(error_unit,'(A)') 'success!'
        else
            write(error_unit,'(A,1X,*(I5,1X))') 'failed: ', ilen
            error_cnt = error_cnt + 1
        end if
    end if

#ifdef USE_UCS4
    ! also try unicode versions:
    call json%get(p, CDK_'str_array', vec, ilen, found)
    call json%get(p, CK_'str_array',  vec, ilen)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
#endif

    ! test json_file interface
    f = json_file(p)
    nullify(p) ! data is now in f
    call f%get('str_array', vec, ilen, found)
    if (.not. found) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (all(ilen==[1,2,3,5])) then
            write(error_unit,'(A)') 'json_file success!'
        else
            write(error_unit,'(A,1X,*(I5,1X))') 'json_file failed: ', ilen
            error_cnt = error_cnt + 1
        end if
    end if
#ifdef USE_UCS4
    ! unicode test
    call f%get(CDK_'str_array', vec, ilen, found)
    if (.not. found) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (all(ilen==[1,2,3,5])) then
            write(error_unit,'(A)') 'json_file success!'
        else
            write(error_unit,'(A,1X,*(I5,1X))') 'json_file failed: ', ilen
            error_cnt = error_cnt + 1
        end if
    end if
#endif

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_25

end module jf_test_25_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_25

    !! 25th unit test.

    use jf_test_25_mod , only: test_25
    implicit none
    integer :: n_errors
    call test_25(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_25
!*****************************************************************************************
#endif
