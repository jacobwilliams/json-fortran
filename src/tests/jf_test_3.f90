!*******************************************************************************************************
!****u* JSON/jf_test_3
!
!  NAME
!    jf_test_3
!
!  DESCRIPTION
!    Third unit test
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
!    JSON-Fortran: A Fortran 2008 JSON API
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

module jf_test_3_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/inputs/'               !working directory
    character(len=*),parameter :: filename2 = 'test2.json'

contains

    subroutine test_3(error_cnt)

!   Read the file generated in jf_test_2, and extract some data from it.

    implicit none

    integer,intent(out) :: error_cnt
    integer :: ival
    character(kind=CK,len=:),allocatable :: cval
    real(wp) :: rval
    type(json_file) :: json    !the JSON structure read from the file:
    integer :: i
    character(kind=CK,len=10) :: str
    real(wp),dimension(:),allocatable :: rvec

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 3'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file: '//dir//filename2

    call json%load_file(filename = dir//filename2)

    if (json_failed()) then    !if there was an error reading the file

        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1

    else

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'reading data from file...'
        !get scalars:
        write(error_unit,'(A)') ''
        call json%get('inputs.integer_scalar', ival)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A,1X,I5)') 'inputs.integer_scalar = ',ival
        end if
        !get one element from a vector:
        write(error_unit,'(A)') ''
        call json%get('trajectory(1).DATA(2)', rval)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A,1X,F30.16)') 'trajectory(1).DATA(2) = ',rval
        end if
        !get vectors:
        do i=1,4

            write(str,fmt='(I10)') i
            str = adjustl(str)

            write(error_unit,'(A)') ''
            call json%get('trajectory('//trim(str)//').VARIABLE', cval)
            if (json_failed()) then

                call json_print_error_message(error_unit)
                error_cnt = error_cnt + 1

            else

                write(error_unit,'(A)') 'trajectory('//trim(str)//').VARIABLE = '//trim(cval)

                !...get the vector using the callback method:
                call json%get('trajectory('//trim(str)//').DATA', rvec)
                if (json_failed()) then
                    call json_print_error_message(error_unit)
                    error_cnt = error_cnt + 1
                else
                    write(error_unit,'(A,1X,*(F30.16,1X))') 'trajectory('//trim(str)//').DATA = ',rvec
                end if

            end if

        end do

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_3

end module jf_test_3_mod

program jf_test_3
    use jf_test_3_mod , only: test_3
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_3(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_3

!*******************************************************************************************************
