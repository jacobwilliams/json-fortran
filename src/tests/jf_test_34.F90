!*****************************************************************************************
!>
!  Module for the 34th unit test.

module jf_test_34_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_34

contains

    subroutine test_34(error_cnt)

    !! Test some of more obscure routines and exception cases.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p,e
    integer(IK) :: max_str_len
    logical(LK) :: found
    logical(LK) :: strict_type_checking
    integer :: i !! counter
    integer(IK),dimension(:),allocatable :: ilen
    character(kind=CK,len=:),allocatable :: str
    logical(LK) :: is_matrix
    integer(IK) :: var_type
    integer(IK) :: n_sets
    integer(IK) :: set_size
    character(kind=CK,len=:),allocatable :: name

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 34'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! test initialize_json_core:
    json = json_core( escape_solidus = .true.,&
                      stop_on_error = .true.)
    call json%initialize(path_mode=1_IK)
    call json%initialize(stop_on_error = .false.)

    ! invalid path mode:
    call json%initialize(path_mode=-999_IK)
    call json%clear_exceptions()

    ! string_info:
    do i = 1, 2

        strict_type_checking = i==1

        call json%initialize(strict_type_checking=strict_type_checking)

        ! with found:
        call json%create_string(p,'string','abc')
        call json%string_info(p,max_str_len=max_str_len,ilen=ilen,found=found)
        call json%destroy(p)

        call json%create_array(p,'integer_array')
        call json%create_integer(e,1_IK,CK_'')
        call json%add(p,e)
        call json%string_info(p,max_str_len=max_str_len,ilen=ilen,found=found)
        call json%destroy(p)

        ! without found:
        call json%create_string(p,'string','abc')
        call json%string_info(p,max_str_len=max_str_len,ilen=ilen)
        call json%clear_exceptions()
        call json%destroy(p)

        call json%create_array(p,'integer_array')
        call json%create_integer(e,1_IK,CK_'')
        call json%add(p,e)
        call json%string_info(p,max_str_len=max_str_len,ilen=ilen)
        call json%clear_exceptions()
        call json%destroy(p)

    end do

    ! json_matrix_info:
    do i = 1, 3

        select case (i)
        case(1)
            ! valid matrix:
            str = CK_'{"matrix":[[1,2,3,4],[5,6,7,8],[9,10,11,12]]}'
        case(2)
            ! not valid (wrong number of elements)
            str = CK_'{"matrix":[[1,2,3],[5,6,7,8],[9,10,11,12]]}'
        case(3)
            ! not valid (not same types)
            str = CK_'{"matrix":[["a",2,3,4],[5,6,7,8],[9,10,11,12]]}'
        end select

        call json%initialize()
        call json%deserialize(p,str)

        call json%matrix_info(p,is_matrix,var_type,&
                                n_sets,set_size,name)
        call json%initialize()

        ! without found:
        call json%matrix_info(p,'path.not.there',is_matrix,&
                               var_type=var_type,n_sets=n_sets,&
                               set_size=set_size,name=name)
        call json%initialize()

        call json%matrix_info(p,'matrix',is_matrix,&
                               var_type=var_type,n_sets=n_sets,&
                               set_size=set_size,name=name)
        call json%initialize()

        ! with found:
        call json%matrix_info(p,'path.not.there',is_matrix,&
                               var_type=var_type,n_sets=n_sets,&
                               set_size=set_size,name=name,&
                               found=found)
        call json%initialize()

        call json%matrix_info(p,'matrix',is_matrix,&
                               var_type=var_type,n_sets=n_sets,&
                               set_size=set_size,name=name,&
                               found=found)
        call json%initialize()

        call json%destroy(p)

    end do

    ! rename:
    call json%initialize(trailing_spaces_significant=.true.)
    call json%create_integer(p,1_IK,CK_'a')
    call json%rename(p,CK_'b  ')
    call json%destroy(p)

    end subroutine test_34

end module jf_test_34_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_34

    !! 34th unit test.

    use jf_test_34_mod , only: test_34
    implicit none
    integer :: n_errors
    call test_34(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_34
!*****************************************************************************************
#endif
