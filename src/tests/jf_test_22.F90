!*****************************************************************************************
!>
! Module for the 22nd unit test

module jf_test_22_mod

    use json_module, CK => json_CK, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_22

    character(len=*),parameter :: dir = '../files/inputs/'      !! working directory
    character(len=*),parameter :: filename = 'comments.json'    !! file to read

contains

    subroutine test_22(error_cnt)

    !! Read a sample JSON file containing comments

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    type(json_file) :: json          !! the JSON structure read from the file

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 22'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(*,*) 'initialize...'

    error_cnt = 0
    call json%initialize(comment_char=CK_'!')  ! fortran-style comments
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

    ! parse the json file:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'parsing file '//dir//filename

    call json%load(filename = dir//filename)

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

    end if

    ! clean up
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'destroy...'
    call json%destroy()
    if (json%failed()) then
      call json%print_error_message(error_unit)
      error_cnt = error_cnt + 1
    end if

end subroutine test_22

end module jf_test_22_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_22

    !! 22nd unit test.

    use jf_test_22_mod , only: test_22
    implicit none
    integer :: n_errors
    call test_22(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_22
!*****************************************************************************************
#endif
