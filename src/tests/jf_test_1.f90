!*******************************************************************************************************
!****u* JSON/jf_test_1
!
!  NAME
!    jf_test_1
!
!  DESCRIPTION
!    First unit test
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

module jf_test_1_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/'    !working directory
    character(len=*),parameter :: filename1 = 'test1.json'

contains

    subroutine test_1(error_cnt) 

!   Read a sample JSON file and retrieve some data from it

    implicit none

    integer,intent(out) :: error_cnt
    type(json_file) :: json    !the JSON structure read from the file:
    integer :: ival
    character(len=:),allocatable :: cval
    real(wp) :: rval
    logical :: found
    type(json_value),pointer :: p

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
      call json_print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 1'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file '//dir//filename1

    call json%load_file(filename = dir//filename1)

    if (json_failed()) then    !if there was an error reading the file

      call json_print_error_message(error_unit)
      error_cnt = error_cnt + 1

    else

      ! print the parsed data to the console
      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'printing the file...'
      call json%print_file()
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      ! extract data from the parsed value
      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'get some data from the file...'

      write(error_unit,'(A)') ''
      call json%get('version.svn', ival)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A,I5)') 'version.svn = ',ival
      end if

      write(error_unit,'(A)') ''
      call json%get('data(1).array(2)', cval)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'data(1).array(2) = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      call json%get('files(1)', cval)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'files(1) = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      call json%get('files(2)', cval)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'files(2) = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      call json%get('files(3)', cval)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'files(3) = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      call json%get('data(2).real', rval)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A,E30.16)') 'data(2).real = ',rval
      end if

      write(error_unit,'(A)') ''
      call json%get('files[4]', cval)      !has hex characters
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'files[4] = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      call json%get('files[5]', cval)      !string with spaces and no escape characters
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') 'files[5] = '//trim(cval)
      end if

      !
      ! Test of values that aren't there:
      ! Note: when using the "found" output, the exceptions are cleared automatically.
      !

      write(error_unit,'(A)') ''
      call json%get('files[10]', cval, found)      !value that isn't there
      if (.not. found) then
        write(error_unit,'(A)') 'files[10] not in file.'
      else
        write(error_unit,'(1x,A)') 'files[10] = '//trim(cval)
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      call json%get('version.blah', ival, found)      !value that isn't there
      if (.not. found) then
        write(error_unit,'(A)') 'version.blah not in file.'
      else
        write(error_unit,'(A)') 'version.blah = ',ival
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      write(error_unit,'(A)') ' Test removing data from the json structure:'

      call json%get('files', p)         !in the middle of a list
      call json_remove(p)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      call json%get('data(1).array', p)   !at the end of a list
      call json_remove(p)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      call json%get('data(2).number', p)  !at the beginning of a list
      call json_remove(p)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'printing the modified structure...'
      call json%print_file()
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      write(error_unit,'(A)') ' Test replacing data from the json structure:'

      call json%get('data(1)', p)
      call json_update(p,'name','Cuthbert',found)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      !call json%get('data(2)', p)
      !call json_update(p,'real',[1.0_wp, 2.0_wp, 3.0_wp],found)   !don't have one like this yet...

      !use the json_file procedure to update a variable:
      call json%update('version.svn',999,found)
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'printing the modified structure...'
      call json%print_file()
      if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json_failed()) then
      call json_print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    end subroutine test_1

end module jf_test_1_mod

program jf_test_1
    use jf_test_1_mod , only: test_1
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_1(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_1

!*******************************************************************************************************
