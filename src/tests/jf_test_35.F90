!*****************************************************************************************
!>
!  Module for the 35th unit test.

module jf_test_35_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_35

contains

    subroutine test_35(error_cnt)

    !! Test destroy a malformed JSON structure

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p_root,p_array,p_element
    logical(LK) :: is_valid  !! True if the structure is valid.
    character(kind=CK,len=:),allocatable :: error_msg !! error message from `validate`
    integer :: i !! counter

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 35'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    do i = 1, 2

        write(error_unit,'(A)') ''

        if (i==1) then
            call json%create_object(p_root, '')
            call json%create_array(p_array, 'array')
            call json%add(p_array,'',1_IK)
            call json%add(p_root,p_array)
            call json%add(p_root,p_array)  ! this creates a malformed JSON structure
        elseif (i==2) then
            call json%create_array(p_root, '')
            call json%create_object(p_element, 'object')
            call json%add(p_element,'int',1_IK)
            call json%add(p_root,p_element)
            call json%add(p_root,p_element)  ! this creates a malformed JSON structure
        end if

        ! test initialize_json_core:
        call json%initialize()

        call json%print(p_root,int(error_unit,IK))

        ! validate it:
        call json%validate(p_root,is_valid,error_msg)
        if (.not. is_valid) then
            write(error_unit,'(A)') 'Validation failed, as expected: '//error_msg
        end if

        call json%destroy(p_root)  ! this crashes with: malloc: *** error
                                   ! for object 0x7f9bea500300: pointer being
                                   ! freed was not allocated

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') 'Destroy successful'
        end if
        write(error_unit,'(A)') ''

    end do

    end subroutine test_35

end module jf_test_35_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_35

    !! 35th unit test.

    use jf_test_35_mod , only: test_35
    implicit none
    integer :: n_errors
    call test_35(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_35
!*****************************************************************************************
#endif
