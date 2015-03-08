!*******************************************************************************************************
!****u* JSON/jf_test_4
!
!  NAME
!    jf_test_4
!
!  DESCRIPTION
!    Fourth unit test
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

module jf_test_4_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/'               !working directory
    character(len=*),parameter :: filename4 = 'test4.json'

contains


    subroutine test_4(error_cnt)

!    Populate a JSON structure, write it to a file,
!        then read it.
!
!    Also tests the json_value_to_string routine to write
!     the file to a character string.

    implicit none

    integer,intent(out) :: error_cnt
    type(json_value),pointer    :: p,inp
    type(json_file) :: json

    integer :: i
    character(kind=CK,len=10) :: istr
    character(kind=CK,len=:),allocatable :: string

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 4'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'creating structure'

    call json_create_object(p,dir//filename4)     !create the value and associate the pointer
                                                  !add the file name as the name of the overall structure
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !config structure:
    call json_create_object(inp,'INPUTS')    !an object
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    !add just integers:
    do i=1,100
        write(istr,fmt='(I10)') i
        istr = adjustl(istr)
        call json_add(inp, 'x'//trim(istr),i)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
    end do
    call json_add(p, inp)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(inp)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'write to file'

    !write the file:
    call json_print(p,trim(dir//filename4))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'write to string'
    write(error_unit,'(A)') ''
    !write it to a string, and print to console:
    call json_print_to_string(p, string)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') string
    deallocate(string)  !cleanup

    !cleanup:
    call json_destroy(p)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'read file'

    call json%load_file(filename = dir//filename4)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'cleanup'
    call json%destroy()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_4

end module jf_test_4_mod

program jf_test_4
    use jf_test_4_mod , only: test_4
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_4(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_4

!*******************************************************************************************************
