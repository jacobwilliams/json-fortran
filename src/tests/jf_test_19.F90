!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/25/2016
!
!  Test the matrix info routines.

module jf_test_19_mod

    use json_module, lk => json_lk, rk => json_rk, ik => json_ik,&
                     ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_19

contains

    subroutine test_19(error_cnt)

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,p_matrix
    logical(lk) :: is_matrix,found
    integer(ik) :: var_type,n_sets,set_size
    character(kind=CK,len=:),allocatable :: name

    !>
    !  Example JSON matrix data
    character(kind=CK,len=*),parameter :: json_example = &
        '{'//&
        '    "matrix": ['//&
        '        [1,2,3,4],'//&
        '        [1,2,3,4],'//&
        '        [1,2,3,4]'//&
        '    ]'//&
        '}'

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 19'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '-------------'
    write(error_unit,'(A)') 'JSON data:'
    write(error_unit,'(A)') '-------------'
    write(error_unit,'(A)') ''
    call json%deserialize(p,json_example)
    call json%print(p,int(error_unit,IK))

    !get some info:
    call json%get(p,ck_'matrix',p_matrix)
    call json%matrix_info(p_matrix,is_matrix,var_type,n_sets,set_size,name)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (is_matrix .and. &
            var_type==json_integer .and. &
            n_sets==3 .and. &
            set_size==4 .and. &
            name=='matrix') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error getting matrix info:'
            write(error_unit,*) 'is_matrix:',is_matrix
            write(error_unit,*) 'var_type :',var_type
            write(error_unit,*) 'n_sets   :',n_sets
            write(error_unit,*) 'set_size :',set_size
            write(error_unit,*) 'name     :'//name
            error_cnt = error_cnt + 1
        end if
    end if

    !now test with a variable that is NOT a matrix:
    call json%get(p,ck_'matrix(1)',p_matrix)
    call json%matrix_info(p_matrix,is_matrix,var_type,n_sets,set_size,name)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (.not. is_matrix) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: this should not be a matrix'
            error_cnt = error_cnt + 1
        end if
    end if

    ! now, test by path:
    call json%matrix_info(p,ck_'matrix',is_matrix,&
                            var_type=var_type,n_sets=n_sets,&
                            set_size=set_size,name=name)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (is_matrix .and. &
            var_type==json_integer .and. &
            n_sets==3 .and. &
            set_size==4 .and. &
            name=='matrix') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error getting matrix info by path:'
            write(error_unit,*) 'is_matrix:',is_matrix
            write(error_unit,*) 'var_type :',var_type
            write(error_unit,*) 'n_sets   :',n_sets
            write(error_unit,*) 'set_size :',set_size
            write(error_unit,*) 'name     :'//name
            error_cnt = error_cnt + 1
        end if
    end if

    !also test with "found" input:
    call json%matrix_info(p,ck_'matrix',is_matrix,found=found,&
                            var_type=var_type,n_sets=n_sets,&
                            set_size=set_size,name=name)
    if (found) then
        write(error_unit,'(A)') '...success'

        !test again with CDK path (for unicode wrapper)
        call json%matrix_info(p,CDK_'matrix',is_matrix,found=found,&
                                var_type=var_type,n_sets=n_sets,&
                                set_size=set_size,name=name)


    else
        write(error_unit,*) 'error calling json_matrix_info_by_path with found input'
        error_cnt = error_cnt + 1
    end if

    !now test with a variable that is NOT a matrix:
    call json%matrix_info(p,ck_'matrix(1)',is_matrix,found=found,&
                            var_type=var_type,n_sets=n_sets,&
                            set_size=set_size,name=name)
    if (.not. is_matrix) then
        write(error_unit,'(A)') '...success'
    else
        write(error_unit,'(A)') 'Error: this should not be a matrix:'
        error_cnt = error_cnt + 1
    end if

    ! cleanup:
    call json%destroy(p)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    end subroutine test_19

end module jf_test_19_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_19

    !! 19th unit test.

    use jf_test_19_mod, only: test_19

    implicit none

    integer :: n_errors
    call test_19(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_19
!*****************************************************************************************
#endif
