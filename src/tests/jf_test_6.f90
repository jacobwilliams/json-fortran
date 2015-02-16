!*******************************************************************************************************
!****f* JSON/test_6
!
!  NAME
!    json_test
!
!  DESCRIPTION
!    Sixth unit test
!
!  USES
!    json_module
!    iso_fortran_env (intrinsic)
!
!  HISTORY
!    Izaak Beekman  : 2/18/2015 : Created
!
!  COPYRIGHT
!
!    JSON-FORTRAN: A Fortran 2003/2008 JSON API
!    https://github.com/jacobwilliams/json-fortran
!
!    Copyright (c) 2014, Jacob Williams
!    All rights reserved.
!
!    Redistribution and use in source and binary forms, with or without modification,
!    are permitted provided that the following conditions are met:
!
!    * Redistributions of source code must retain the above copyright notice, this
!      list of conditions and the following disclaimer.
!
!    * Redistributions in binary form must reproduce the above copyright notice, this
!      list of conditions and the following disclaimer in the documentation and/or
!      other materials provided with the distribution.
!
!    * The names of its contributors may not be used to endorse or promote products
!      derived from this software without specific prior written permission.
!
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
module jf_test_6_mod
    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64
    implicit none
    character(len=*),parameter :: dir = '../files/'               !working directory

contains
!**************************************************************
    subroutine test_6(error_cnt)
!**************************************************************
!
!    This example tries to read an invalid json file.
!
!**************************************************************
    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    integer :: i

    character(len=*),dimension(2),parameter :: files = ['invalid.json ',&
                                                        'invalid2.json']

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 6 : invalid JSON files'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    do i=1,2

        ! parse the json file:
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'load file: '//trim(files(i))
        write(error_unit,'(A)') ''
        call json%load_file(filename = dir//trim(files(i)))
        if (json_failed()) then
            call json_print_error_message(error_unit)
        else
            write(error_unit,'(A)') 'An error should have been raised!'
            error_cnt = error_cnt + 1
        end if
        ! clean up
        call json%destroy()
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
    end do

!**************************************************************
    end subroutine test_6
!**************************************************************

end module jf_test_6_mod
!*******************************************************************************************************

program jf_test_6
    use jf_test_6_mod , only: test_6
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_6(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_6
