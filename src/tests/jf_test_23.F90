!*****************************************************************************************
!>
! Module for the 23rd unit test.

module jf_test_23_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_23

    character(len=*),parameter :: dir = '../files/inputs/'    !! working directory
    character(len=*),parameter :: filename1 = 'test1.json'    !! file to read

contains

    subroutine test_23(error_cnt)

    !! Read a sample JSON file and retrieve some data from it
    !! using RFC 6901 paths.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json       !! the JSON structure read from the file
    type(json_value),pointer :: p
    type(json_core) :: core
    character(kind=json_CK,len=:),allocatable :: cval
    character(kind=json_CK,len=:),allocatable :: key
    character(kind=json_CK,len=:),allocatable :: path
    integer(IK) :: ival
    real(wp) :: rval
    logical(LK) :: found
    character(kind=json_CK,len=10),dimension(:),allocatable :: cval_array
    integer :: i !! counter

    error_cnt = 0
    call json%initialize(   trailing_spaces_significant=.true.,&
                            case_sensitive_keys=.true.,&
                            path_mode=2_IK) ! RFC6901 paths
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

    call json%load(filename = dir//filename1)

    if (json%failed()) then    !if there was an error reading the file

      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1

    else

      ! print the parsed data to the console
      write(error_unit,'(A)') ''
      write(error_unit,'(A)') 'printing the file...'
      call json%print(int(error_unit,IK))
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
      call check_i()

      write(error_unit,'(A)') ''
      key = '/data/0/array/1'
      call json%get(key, cval)
      call check_c()

      write(error_unit,'(A)') ''
      key = '/files/0'
      call json%get(key, cval)
      call check_c()

      write(error_unit,'(A)') ''
      key = '/files/1'
      call json%get(key, cval)
      call check_c()

      write(error_unit,'(A)') ''
      key = '/files/2'
      call json%get(key, cval)
      call check_c()

      write(error_unit,'(A)') ''
      key = '/data/1/real'
      call json%get(key, rval)
      call check_i()

      write(error_unit,'(A)') ''
      key = '/files/3'
      call json%get(key, cval)      !has hex characters
      call check_c()

      write(error_unit,'(A)') ''
      key = '/files/4'
      call json%get(key, cval)      !string with spaces and no escape characters
      call check_c()

      ! Test the examples in the RFC 6901 spec:

      write(error_unit,'(A)') ''
      key = ""
      call json%get(key, p) ! the whole document
      if (json%failed()) then
          write(error_unit,'(A)') 'Error: could not find '//key
          error_cnt = error_cnt + 1
      end if

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/foo"
      call json%get(key, cval_array) ! ["bar", "baz"]
      if (json%failed()) then
          write(error_unit,'(A)') 'Error: could not find '//key
          error_cnt = error_cnt + 1
      else
          write(error_unit,'(A)') key//' = ',cval_array
      end if

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/foo/0"
      call json%get(key, cval)       ! "bar"
      call check_c()   ! "bar"

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/  "
      call json%get(key, ival)
      call check_i()   ! 0

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/a~1b"
      call json%get(key, ival)
      call check_i()   ! 1

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/c%d"
      call json%get(key, ival)
      call check_i()   ! 2

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/e^f"
      call json%get(key, ival)
      call check_i()   ! 3

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/g|h"
      call json%get(key, ival)
      call check_i()   ! 4

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/i\j"
      call json%get(key, ival)
      call check_i()   ! 5

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/k""l"
      call json%get(key, ival)
      call check_i()   ! 6

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/ "
      call json%get(key, ival)
      call check_i()   ! 7

      write(error_unit,'(A)') ''
      key = "/rfc6901 tests/m~0n"
      call json%get(key, ival)
      call check_i()   ! 8



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
                            path_mode=2_IK) ! RFC6901 paths

    write(error_unit,'(A)') ''
    do i = 1, 4
        select case (i)
            case(1); key = '/data/1/real'
            case(2); key = '/rfc6901 tests/  '
            case(3); key = '/rfc6901 tests/m~0n'
            case(4); key = '/rfc6901 tests/a~1b'
        end select
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
    end do

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    contains

        subroutine check_c()

        !! check results of a character test

        implicit none

        if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
        else
          write(error_unit,'(A)') key//' = '//cval
        end if

        end subroutine check_c

        subroutine check_i()

        !! check results of an integer test

        implicit none

        if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
        else
          write(error_unit,'(A,I5)') key//' = ',ival
        end if

        end subroutine check_i

    end subroutine test_23

end module jf_test_23_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_23

    !! 23rd unit test.

    use jf_test_23_mod , only: test_23
    implicit none
    integer :: n_errors
    call test_23(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_23
!*****************************************************************************************
#endif
