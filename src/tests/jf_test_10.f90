!*******************************************************************************************************
!****u* JSON/jf_test_10
!
!  NAME
!    jf_test_10
!
!  DESCRIPTION
!    Tenth unit test.
!
!  AUTHOR
!    Jacob Williams : 3/10/3015
!
!  LICENSE
!
!    JSON-FORTRAN: A Fortran 2008 JSON API
!
!    https://github.com/jacobwilliams/json-fortran
!
!    Copyright (c) 2014, Jacob Williams
!
!    All rights reserved.
!
!    Redistribution and use in source and binary forms, with or without modification,
!    are permitted provided that the following conditions are met:
!    * Redistributions of source code must retain the above copyright notice, this
!      list of conditions and the following disclaimer.
!    * Redistributions in binary form must reproduce the above copyright notice, this
!      list of conditions and the following disclaimer in the documentation and/or
!      other materials provided with the distribution.
!    * The names of its contributors may not be used to endorse or promote products
!      derived from this software without specific prior written permission.
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
!    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!  SOURCE

module jf_test_10_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: filename = 'test1.json'
    character(len=*),parameter :: dir = '../files/inputs/' !working directory

contains

    subroutine test_10(error_cnt)

!   Test some of the lesser-used features of the library

    implicit none

    integer,intent(out) :: error_cnt

    character(kind=CK,len=256),dimension(:),allocatable :: str_vec
    type(json_file) :: f,f2
    type(json_value),pointer :: p
    character(kind=CK,len=:),allocatable :: str
    logical :: found,lval
    integer :: var_type,n_children

    character(kind=CK,len=*),parameter :: json_str = '{ "blah": 123 }'

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 10 '
    write(error_unit,'(A)') '================================='

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'Loading file: '//trim(filename)//'...'

    call f%load_file(dir//filename)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') 'json_file_move_pointer...'
    call f2%move(f)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_load_from_string...'
    call f%load_from_string(json_str)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_print_to_string...'
    call f%print_to_string(str)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_variable_info...'
    call f%info('blah',found,var_type,n_children)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. lval) then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result.'
            error_cnt = error_cnt + 1
        end if
    end if

    ! json_file_get_logical_vec .... [add this]

    write(error_unit,'(A)') 'json_file_get_string_vec...'
    call f2%get('files',str_vec,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. size(str_vec)==5 .and. &
            str_vec(1)=='..\path\to\files\file1.txt') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result: '//trim(str_vec(1))
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_file_update_logical [variable present]...'
    call f2%update('data(1).tf1',.false.,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_update_real [variable present]...'
    call f2%update('data[2].real',100.0d0,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    call f2%update('new_real',1776.0d0,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    write(error_unit,'(A)') 'json_file_update_string [variable present]...'
    call f2%update('version.string','10.0.0',found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if

    !--------------------------------

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'json_file_get_integer...'
    call f2%get('$',p,found)  !get root
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        if (found) then
            write(error_unit,'(A)') '...success'
            write(error_unit,'(A)') 'json_remove_if_present...'
            call json_remove_if_present(p,'version.patch')
            if (json_failed()) then
                call json_print_error_message(error_unit)
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
    call json_update(p,'data(1).tf1',.true.,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    call json_update(p,'data(2).real',-1.0d0,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    call json_get(p,'data(1).tf1',lval,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
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
    call json_get(p,'files',str_vec,found)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !also make sure the values are correct:
        if (found .and. size(str_vec)==5 .and. &
            str_vec(1)=='..\path\to\files\file1.txt') then
            write(error_unit,'(A)') '...success'
        else
            write(error_unit,'(A)') 'Error: incorrect result: '//trim(str_vec(1))
            error_cnt = error_cnt + 1
        end if
    end if

    write(error_unit,'(A)') 'json_create...'
    write(error_unit,'(A)') 'json_create_logical...'; call json_destroy(p); call json_create_logical(p,.true.,'foo')
    write(error_unit,'(A)') 'json_create_integer...'; call json_destroy(p); call json_create_integer(p,1000,'foo')
    write(error_unit,'(A)') 'json_create_double ...'; call json_destroy(p); call json_create_double (p,9.0d0,'foo')
    write(error_unit,'(A)') 'json_create_string ...'; call json_destroy(p); call json_create_string (p,'foo','bar')
    write(error_unit,'(A)') 'json_create_null   ...'; call json_destroy(p); call json_create_null   (p,'foo')
    write(error_unit,'(A)') 'json_create_object ...'; call json_destroy(p); call json_create_object (p,'foo')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        write(error_unit,'(A)') '...success'
    end if
    

    !--------------------------------

    !cleanup:
    !call f%destroy()   !WARNING: causing "pointer being freed was not allocated" errors.... need to investigate
    !call f2%destroy()

    end subroutine test_10

end module jf_test_10_mod

program jf_test_10
    use jf_test_10_mod , only: test_10
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_10(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_10

!*******************************************************************************************************
