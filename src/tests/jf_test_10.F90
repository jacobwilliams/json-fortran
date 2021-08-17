!*****************************************************************************************
!> author: Jacob Williams
!  date: 3/10/2015
!
! Module for the tenth unit test.

module jf_test_10_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_10

    character(len=*),parameter :: filename = 'test1.json'
    character(len=*),parameter :: dir = '../files/inputs/' !working directory

contains

    subroutine test_10(error_cnt)

    !! Test some of the lesser-used features of the library

    implicit none

    integer,intent(out) :: error_cnt

    character(kind=json_CK,len=256),dimension(:),allocatable :: str_vec
    type(json_file) :: f,f2
    type(json_value),pointer :: p
    type(json_core) :: json       !! factory for manipulating `json_value` pointers
    character(kind=json_CK,len=:),allocatable :: str,name
    logical(LK) :: found,lval
    integer(IK) :: var_type,n_children
    integer(IK) :: ival

    character(kind=json_CDK,len=*),parameter :: json_str = '{ "blah": 123 }'

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 10 '
    write(error_unit,'(A)') '================================='

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Loading file: '//trim(filename)//'...'

    call f%load(dir//filename)  ! will call initialize()
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') 'json_file_move_pointer...'
    call f2%initialize()
    call f2%move(f)
    call f%nullify() ! not strictly necessary since it's already done by move.
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_load_from_string...'
    call f%deserialize(json_str)
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_print_to_string...'
    call f%serialize(str)
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_variable_info...'
    call f%info('blah',found,var_type,n_children,name)
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (var_type==json_integer .and. n_children==0) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error invalid values:',var_type,n_children
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_file_get_logical...'
    call f2%get('data(1).tf1',lval,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. lval) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result: ', lval
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_file_get_integer...'
    call f2%get('a.b',ival,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. ival==1_IK) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A,1X,I5)') 'Error: incorrect result: ', ival
            error_cnt = error_cnt + 1
        end if
    end if

    ! json_file_get_logical_vec .... [add this]

    write(error_unit,'(A)') 'json_file_get_string_vec...'
    call f2%get('files',str_vec,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. size(str_vec)==7 .and. &
            str_vec(1)=='..\path\to\files\file1.txt') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result: '//trim(str_vec(1))
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_file_update_logical [variable present]...'
    call f2%update('data(1).tf1',.false.,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if
    write(error_unit,'(A)') 'json_file_update_logical [variable not present]...'
    call f2%update('new_logical',.true.,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_update_real [variable present]...'
    call f2%update('data[2].real',100.0_wp,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if
    write(error_unit,'(A)') 'json_file_update_real [variable not present]...'
    call f2%update('new_real',1776.0_wp,found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_update_string [variable present]...'
    call f2%update('version.string','10.0.0',found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if
    write(error_unit,'(A)') 'json_file_update_string [variable not present]...'
    call f2%update('new_string','foo',found)
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    !--------------------------------

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'json_file_get_integer...'
    call f2%get('$',p,found)  !get root
    if (f2%failed()) then
        call f2%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'

            write(error_unit,'(A)') 'json_info...'
            call json%info(p,var_type,n_children,name)
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                write(error_unit,'(A)') '...success'
            end if

            write(error_unit,'(A)') 'json_remove_if_present...'
            call json%remove_if_present(p,'version.patch')
            if (json%failed()) then
                call json%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else
                write(error_unit,'(A)') '...success'
            end if
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_update_logical...'
    call json%update(p,'data(1).tf1',.true.,found)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_update_double...'
    call json%update(p,'data(2).real',-1.0_wp,found)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_get_logical...'
    call json%get(p,'data(1).tf1',lval,found)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: variable was not there.'
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_get_string_vec...'
    call json%get(p,'files',str_vec,found)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. size(str_vec)==7 .and. &
            str_vec(1)=='..\path\to\files\file1.txt') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result: '//trim(str_vec(1))
            error_cnt = error_cnt + 1
        end if
    end if

    nullify(p)  ! p is just a pointer to f2

    write(error_unit,'(A)') 'json_create...'
    write(error_unit,'(A)') 'json_create_logical...'; call json%create_logical(p,.true.,'foo')
    write(error_unit,'(A)') 'json_create_integer...'; call json%destroy(p); call json%create_integer(p,1000_IK,'foo')
    write(error_unit,'(A)') 'json_create_real   ...'; call json%destroy(p); call json%create_real   (p,9.0_wp,'foo')
    write(error_unit,'(A)') 'json_create_string ...'; call json%destroy(p); call json%create_string (p,'foo','bar')
    write(error_unit,'(A)') 'json_create_null   ...'; call json%destroy(p); call json%create_null   (p,'foo')
    write(error_unit,'(A)') 'json_create_object ...'; call json%destroy(p); call json%create_object (p,'foo')
    call json%destroy(p)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    !--------------------------------

    !cleanup:
    call f%destroy()
    call f2%destroy()

    end subroutine test_10

end module jf_test_10_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_10

    !! Tenth unit test.

    use jf_test_10_mod , only: test_10
    implicit none
    integer :: n_errors
    call test_10(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_10
!*****************************************************************************************
#endif
