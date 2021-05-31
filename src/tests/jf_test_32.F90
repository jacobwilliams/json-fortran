!*****************************************************************************************
!>
!  Module for the 32nd unit test.

module jf_test_32_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_32

contains

    subroutine test_32(error_cnt)

    !! Test all the options and wrappers for [[json_get_path]].

    implicit none

    integer,intent(out) :: error_cnt

    integer :: i !! counter
    integer :: j !! counter
    type(json_core) :: json
    type(json_value),pointer :: p, p_var
    character(kind=CK,len=:),allocatable :: path_ck
    character(kind=CDK,len=:),allocatable :: path_cdk
    logical(LK) :: use_alt_array_tokens
    logical(LK) :: found

    character(kind=CK,len=1),parameter  :: path_sep_ck  = CK_'.'
    character(kind=CDK,len=1),parameter :: path_sep_cdk = CDK_'.'
    character(kind=CK,len=*),parameter  :: json_string  = CK_'{ "struct": {"a": [1,2]} }'

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 32'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize()

    call json%deserialize(p,json_string)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        call json%get(p,CK_'struct.a(1)',p_var)

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else

            ! CK version:
            do i=1,2
                use_alt_array_tokens = i==1
                do j=1,2
                    if (j==1) then
                        call json%get_path(p_var, path_ck, found, use_alt_array_tokens, path_sep_ck)
                    else
                        call json%get_path(p_var, path_ck, found, use_alt_array_tokens)
                    end if
                    if (json%failed()) then
                        call json%print_error_message(error_unit)
                        error_cnt = error_cnt + 1
                    else
                        write(error_unit,'(A)') path_ck
                    end if
                end do
            end do

            ! CDK version:
            do i=1,2
                use_alt_array_tokens = i==1
                do j=1,2
                    if (j==1) then
                        call json%get_path(p_var, path_cdk, found, use_alt_array_tokens, path_sep_cdk)
                    else
                        call json%get_path(p_var, path_cdk, found, use_alt_array_tokens)
                    end if
                    if (json%failed()) then
                        call json%print_error_message(error_unit)
                        error_cnt = error_cnt + 1
                    else
                        write(error_unit,'(A)') path_cdk
                    end if
                end do
            end do

        end if

    end if

    if (.not. json%failed()) then
        write(error_unit,*) ''
        write(error_unit,*) 'Success!'
    end if

    call json%destroy(p)    ! free memory

    end subroutine test_32

end module jf_test_32_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_32

    !! 32nd unit test.

    use jf_test_32_mod , only: test_32
    implicit none
    integer :: n_errors
    call test_32(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_32
!*****************************************************************************************
#endif
