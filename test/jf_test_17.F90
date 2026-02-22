!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/29/2016
!
! Module for the 17th unit test.
! Test the `rename` function.

module jf_test_17_mod

    use json_module, CK => json_CK, CDK => json_CDK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_17

contains

    subroutine test_17(error_cnt)

    !! Test the `rename` function.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,q
    type(json_file) :: f

    character(kind=CK,len=*),parameter :: json_string = &
        '{"city": ["New York","Los Angeles","Chicago"], '//&
        '"value": 1, "iflag": true, "struct":{"vec":[1,2,3]}}'

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 17'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%deserialize(p, json_string)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%print(p,int(error_unit,IK))

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Rename: "city" to "cities"'
    call json%get(p,'city',q)
    call json%rename(q,'cities')     ! also test the unicode ones
    call json%rename(q,CK_'cities')
    call json%rename(q,CDK_'cities')
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(q)

    !verify that it was renamed:
    call json%get(p,'cities',q)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'Success!'
    end if
    nullify(q)

    ! rename by specifying the path:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Rename: "iflag" to "flag"'
    call json%rename(p,'iflag','flag')
    call json%print(p,int(output_unit,IK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%get(p,'flag',q)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'Success!'
    end if
    nullify(q)

    ! unicode wrappers:
    call json%rename(p,CK_'flag',  CK_'iflag')
    call json%rename(p,CK_'iflag', CDK_'flag')
    call json%rename(p,CDK_'flag', CK_'iflag')
    call json%rename(p,CDK_'iflag',CDK_'flag')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'Success!'
    end if

    !cleanup:
    call json%destroy(p)

    ! test the corresponding json_file version:
    call f%deserialize(json_string)
    call f%rename(CK_'iflag',  CK_'flag')
    call f%rename(CK_'flag',   CDK_'iflag')
    call f%rename(CDK_'iflag', CK_'flag')
    call f%rename(CDK_'flag',  CDK_'iflag')
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') 'Success!'
    end if

    ! test the .in. operator and valid_path:
    if (CDK_'city' .in. f ) then
        write(error_unit,'(A)') '.in. operator Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') '.in. operator failed: "city"'
    end if
    if (CK_'city' .in. f)  then
        write(error_unit,'(A)') '.in. operator Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') '.in. operator failed: "city"'
    end if
    if ('struct.vec' .in. f)  then
        write(error_unit,'(A)') '.in. operator Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') '.in. operator failed: "struct.vec"'
    end if

    if (f%valid_path(CDK_'city')) then
        write(error_unit,'(A)') 'valid_path Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'valid_path failed: "city"'
    end if
    if (f%valid_path(CK_'city')) then
        write(error_unit,'(A)') 'valid_path Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'valid_path failed: "city"'
    end if
    if (f%valid_path(CK_'struct.vec')) then
        write(error_unit,'(A)') 'valid_path Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'valid_path failed: "struct.vec"'
    end if
    ! try one not there:
    if (.not. f%valid_path(CK_'?????????')) then
        write(error_unit,'(A)') 'valid_path Success!'
    else
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'valid_path failed: "?????????"'
    end if

    ! cleanup:
    call f%destroy()

    end subroutine test_17

end module jf_test_17_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_17

    !! 17th unit test.

    use jf_test_17_mod, only: test_17
    implicit none
    integer :: n_errors
    call test_17(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_17
!*****************************************************************************************
#endif
