!*******************************************************************************************************
!****u* JSON/jf_test_8
!
!  NAME
!    jf_test_8
!
!  DESCRIPTION
!    Eighth unit test
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

module jf_test_8_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none
    
    character(len=*),parameter :: dir = '../files/'               !working directory

contains

    subroutine test_8(error_cnt)

!   read a JSON structure from a string

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p

    character(len=*),parameter :: newline = achar(10)

    character(len=*),parameter :: str = '{ "label": "foo",'//newline//' "value": "bar" }'

    character(len=*),parameter :: str2 = '{ "label": "foo",'//newline//&
                                         '  "value": "bar",'//newline//&
                                         '  "empty_array": [],'//newline//&
                                         '  "empty_object": {}' //newline//&
                                         '}'

    character(len=*),parameter :: str_invalid = '{ "label": "foo",'//newline//' "value : "bar" }'

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 8 : read JSON from string'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Valid test 1:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json_parse(str=str, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_print(p,OUTPUT_UNIT)  ! print to console
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Valid test 2:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json_parse(str=str2, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_print(p,OUTPUT_UNIT)  ! print to console
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ' Invalid test:'
    write(error_unit,'(A)') '**************'
    write(error_unit,'(A)') ''
    call json_parse(str=str_invalid, p=p)   ! read it from str
    if (json_failed()) then
        call json_print_error_message(error_unit)
    else
        write(error_unit,'(A)') 'This should have failed!'
        error_cnt = error_cnt + 1
    end if
    call json_print(p,OUTPUT_UNIT)  ! print to console
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_destroy(p)            ! cleanup
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(error_unit,'(A)') ''

    end subroutine test_8

end module jf_test_8_mod

program jf_test_8
    use jf_test_8_mod , only: test_8
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_8(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_8

!*******************************************************************************************************