!*******************************************************************************************************
!****u* JSON/jf_test_7
!
!  NAME
!    jf_test_7
!
!  DESCRIPTION
!    Seventh unit test
!
!  USES
!    json_module
!    iso_fortran_env (intrinsic)
!
!  HISTORY
!    Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)
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

module jf_test_7_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

contains

    subroutine test_7(error_cnt)

!   Indent test

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: root,a,b,c,d,e,e1,e2,escaped_string

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 7 : indent test'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

!-----------------------
! jsonlint indention is
!-----------------------
!{
!    "a": {
!        "ints": [
!            1,
!            2,
!            3
!        ],
!        "chars": [
!            "a",
!            "b",
!            "c"
!        ]
!    },
!    "b": {
!        "c": {
!            "val1": 1066
!        }
!    },
!    "d": {
!        "val2": 1815
!    },
!    "array": [
!        {
!            "int1": 1
!        },
!        {
!            "int1": 1,
!            "int2": 2
!        }
!    ]
!    "escaped string": "\\\/\b\f\n\r\t"
!}

    !create a json structure:
    call json_create_object(root,'root')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(a,'a')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(a,'ints', [1,2,3])
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(b,'b')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(a,'chars', ['a','b','c'])
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(c,'c')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(c,'val1', 1066)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(d,'d')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(d,'val2', 1815)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json_create_array(e,'array')   !objects in an array
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(e1,'')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(e1,'int1', 1)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_create_object(e2,'')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(e2,'int1', 1)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(e2,'int2', 2)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(e,e1)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(e,e2)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
        call json_create_object(escaped_string,'escaped string')
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(escaped_string,'escaped string',&
         '\/'//&
         achar(8)//&
         achar(12)//&
         achar(10)//&
         achar(13)//&
         achar(9))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json_add(root,a)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(root,b)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(b,c)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(root,d)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(root,e)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(root,escaped_string)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    nullify(a)  !don't need these anymore
    nullify(b)
    nullify(c)
    nullify(d)
    nullify(e)
    nullify(e1)
    nullify(e2)
    nullify(escaped_string)

    call json_print(root,output_unit)  !print to the console
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_print(root,error_unit)  !print to stderr
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json_destroy(root)  !cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_7

end module jf_test_7_mod

program jf_test_7
    use jf_test_7_mod , only: test_7
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_7(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_7

!*******************************************************************************************************
