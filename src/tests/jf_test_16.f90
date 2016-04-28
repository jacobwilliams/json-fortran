!*****************************************************************************************
!> author: Jacob Williams
!  date: 4/26/2016
!
! Module for the 16th unit test.
! Test the `swap` function.

module jf_test_16_mod

    use json_module, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

contains

    subroutine test_16(error_cnt)

    !! Test the `swap` function.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,p1,p2,q

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 16'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Original:'
    call json%parse(p, '{"cities": ["New York","Los Angeles","Chicago"], '//&
                       '"value": 1, "flag": true, "struct":{"vec":[1,2,3]}}')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    !call print_tree(json,p)
    call json%print(p,error_unit)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: cities <-> flag'
    call json%get(p,'cities',p1)
    call json%get(p,'flag',p2)
    call json%swap(p1,p2)
    !call print_tree(json,p)
    call json%print(p,output_unit)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: flag <-> value'
    call json%get(p,'flag',p1)
    call json%get(p,'value',p2)
    call json%swap(p1,p2)
    !call print_tree(json,p)
    call json%print(p,output_unit)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Swap: flag <-> struct.vec'
    call json%get(p,'flag',p1)
    call json%get(p,'struct.vec',p2)
    call json%swap(p1,p2)
    !call print_tree(json,p)
    call json%print(p,output_unit)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(p1)
    nullify(p2)

    call json%destroy(p)

    end subroutine test_16

    subroutine print_tree(json,p)

    !! just for debugging. Print some info about the structure.

    implicit none

    type(json_core),intent(inout) :: json
    type(json_value),pointer,intent(in) :: p

    character(kind=CK,len=:),allocatable :: p_name, name
    type(json_value),pointer :: q,r
    integer :: n_children,i

    write(*,*) ''
    write(*,*) '------------'
    call json%info(p,name=p_name,n_children=n_children)
    write(*,*) 'name:       '//p_name
    write(*,*) 'n_children: ',n_children

    call json%get_parent(p,q)
    if (associated(q)) then
        call json%info(q,name=name)
        write(*,*) 'root parent: '//name
    end if

    do i=1,n_children
        call json%get_child(p, i, q)
        if (associated(q)) then
            call json%info(q,name=name)
            write(*,*) 'child ',i,name
            call json%get_previous(q,r)
            if (associated(r)) then
                call json%info(r,name=name)
                write(*,*) '   prev ',i,name
            end if
            call json%get_next(q,r)
            if (associated(r)) then
                call json%info(r,name=name)
                write(*,*) '   next ',i,name
            end if
        else
            call json%print_error_message(error_unit)
            exit
        end if
    end do
    write(*,*) '------------'
    write(*,*) ''

    end subroutine print_tree

end module jf_test_16_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_16

    !! 16th unit test.

    use jf_test_16_mod, only: test_16
    implicit none
    integer :: n_errors
    call test_16(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_16
!*****************************************************************************************
