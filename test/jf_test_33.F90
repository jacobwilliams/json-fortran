!*****************************************************************************************
!>
!  Module for the 33rd unit test.

module jf_test_33_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_33

contains

    subroutine test_33(error_cnt)

    !! Test the clone routine

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p, p_var, p_var_clone
    integer :: i !! counter
    logical(LK) :: is_valid  !! True if the structure is valid.
    character(kind=CK,len=:),allocatable :: error_msg !! if not valid, this will contain
                                                      !! a description of the problem

    character(kind=CK,len=*),parameter  :: json_string  = CK_'{ "struct": {"a": [{"b":1},{"b":2}]} }'

    character(kind=CK,len=*),dimension(5),parameter :: keys = [ CK_'$            ',&
                                                                CK_'struct       ',&
                                                                CK_'struct.a     ',&
                                                                CK_'struct.a(1)  ',&
                                                                CK_'struct.a(1).b' ]

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 33'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize()

    call json%deserialize(p,json_string)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        do i = 1, size(keys)
            write(error_unit,'(A)') 'Cloning "'//trim(keys(i))//'" ...'
            call json%get(p,trim(keys(i)),p_var)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
                exit
            end if
            call json%clone(p_var, p_var_clone)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
                exit
            end if
            call json%print(p_var_clone,int(output_unit,IK))
            call json%validate(p_var_clone,is_valid,error_msg)
            if (.not. is_valid) then
                error_cnt = error_cnt + 1
                write(error_unit,*) error_msg
            else
                write(error_unit,*) '... Valid'
            end if
            call json%destroy(p_var_clone)    ! free memory
            write(error_unit,*) ''
        end do

    end if

    if (.not. json%failed() .and. error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Test Failed!'
    end if

    call json%destroy(p)    ! free memory

    end subroutine test_33

end module jf_test_33_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_33

    !! 33rd unit test.

    use jf_test_33_mod , only: test_33
    implicit none
    integer :: n_errors
    call test_33(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_33
!*****************************************************************************************
#endif
