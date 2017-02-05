!*****************************************************************************************
!>
! Module for the 23rd unit test.

module jf_test_23_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/inputs/'    !! working directory
    character(len=*),parameter :: filename1 = 'test1.json'    !! file to read

contains

    subroutine test_23(error_cnt)

    !! Read a sample JSON file and retrieve some data from it
    !! using RFC 6901 paths.

    implicit none

    type(json_file) :: json       !! the JSON structure read from the file
    type(json_value),pointer :: p
    type(json_core) :: core
    integer,intent(out) :: error_cnt
    character(kind=json_CK,len=:),allocatable :: cval
    character(kind=json_CK,len=:),allocatable :: key
    character(kind=json_CK,len=:),allocatable :: path
    integer :: ival
    real(wp) :: rval
    logical :: found

    error_cnt = 0
    call json%initialize(   trailing_spaces_significant=.true.,&
                            case_sensitive_keys=.true.,&
                            use_rfc6901_paths=.true.)
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 23'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file '//dir//filename1

    call json%load_file(filename = dir//filename1)

    if (json%failed()) then    !if there was an error reading the file

      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1

    else

      ! print the parsed data to the console
      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'printing the file...'
      call json%print_file()
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      end if

      ! extract data from the parsed value
      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'get some data from the file...'

      write(error_unit,'(A)') ''
      key = '/version/svn'
      call json%get(key, ival)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A,I5)') key//' = ',ival
      end if

      write(error_unit,'(A)') ''
      key = '/data/0/array/1'
      call json%get(key, cval)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      key = '/files/0'
      call json%get(key, cval)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      key = '/files/1'
      call json%get(key, cval)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      key = '/files/2'
      call json%get(key, cval)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      key = '/data/1/real'
      call json%get(key, rval)
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A,E30.16)') key//' = ',rval
      end if

      write(error_unit,'(A)') ''
      key = '/files/3'
      call json%get(key, cval)      !has hex characters
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      write(error_unit,'(A)') ''
      key = '/files/4'
      call json%get(key, cval)      !string with spaces and no escape characters
      if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
      else
        write(error_unit,'(A)') key//' = '//trim(cval)
      end if

      !
      ! Test of values that aren't there:
      ! Note: when using the "found" output, the exceptions are cleared automatically.
      !

      write(error_unit,'(A)') ''
      key = '/files/9'
      call json%get(key, cval, found)      !value that isn't there
      if (.not. found) then
        write(error_unit,'(A)') key//' not in file.'
      else
        write(error_unit,'(1x,A)') key//' = '//trim(cval)
        error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      key = '/version/blah'
      call json%get(key, ival, found)      !value that isn't there
      if (.not. found) then
        write(error_unit,'(A)') key//' not in file.'
      else
        write(error_unit,'(A)') key//' = ',ival
        error_cnt = error_cnt + 1
      end if

    end if

    ! get the path to one of the variables:

    call core%initialize(   trailing_spaces_significant=.true.,&
                            case_sensitive_keys=.true.,&
                            use_rfc6901_paths=.true.)

    write(error_unit,'(A)') ''
    key = '/data/1/real'
    call json%get(key,p)
    call core%get_path(p, path, found)
    if (found) then
        if (key==path) then
            write(error_unit,'(A)') 'get_path test passed: '//path
        else
            write(error_unit,'(A)') 'Error: path does not match: '//path//' '//key
            error_cnt = error_cnt + 1
        end if
    else
        write(error_unit,'(A)') 'Error: could not find '//key
        error_cnt = error_cnt + 1
    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    end subroutine test_23

end module jf_test_23_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_23

    !! 23rd unit test.

    use jf_test_23_mod , only: test_23
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_23(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_23
!*****************************************************************************************
