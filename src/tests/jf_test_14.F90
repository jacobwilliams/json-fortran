!*****************************************************************************************
!> author: Jacob Williams
!  date: 09/02/2015
!
! Module for the 14th unit test.

module jf_test_14_mod

    use json_module, IK => json_IK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_14

    character(len=*),parameter :: dir = '../files/inputs/'  !! working directory
    character(len=*),parameter :: filename1 = 'test1.json'  !! the file to read
    integer :: icount = 0  !! a count of the number of "name" variables found
    character(len=:),allocatable :: new_name  !! name to change to

contains

    subroutine test_14(error_cnt)

    !! Tests the traversal of a JSON structure
    !!
    !! It traverses the structure, looks for all "name" variables, and changes the name.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer  :: p
    type(json_file) :: f

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 14'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    icount = 0 !number of name changes (should be 2)
    new_name = 'Fred'  !change all names to this

    call json%initialize() !initialize the module

    call json%load(dir//filename1,p)  !read the file
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%traverse(p,rename) !traverse all nodes in the structure
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (icount/=2) then
        write(error_unit,'(A)') 'Error: should be 2 "name" variables in this file: '//filename1
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ' All names changed to '//new_name//':'
        write(error_unit,'(A)') ''
        call json%print(p,int(output_unit,IK))
        write(error_unit,'(A)') ''
    end if

    call json%destroy(p)  !clean up
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    ! now, test traversal from a json_file:
    new_name = 'Bob'
    icount = 0
    call f%initialize()
    call f%load(dir//filename1)  !read the file
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call f%traverse(rename) !traverse all nodes in the structure
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (icount/=2) then
        write(error_unit,'(A)') 'Error: should be 2 "name" variables in this file: '//filename1
        error_cnt = error_cnt + 1
    end if

    if (error_cnt==0) then
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ' All names changed to '//new_name//':'
        write(error_unit,'(A)') ''
        call f%print(int(output_unit,IK))
        write(error_unit,'(A)') ''
    end if

    call f%destroy()  ! clean up
    if (f%failed()) then
        call f%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (allocated(new_name)) deallocate(new_name)

    end subroutine test_14

    subroutine rename(json,p,finished)  !! change all "name" variable values to "Fred"

    implicit none

    class(json_core),intent(inout)      :: json
    type(json_value),pointer,intent(in) :: p
    logical(json_LK),intent(out)        :: finished

    integer(IK) :: var_type
    character(kind=json_CK,len=:),allocatable :: str
    logical(json_LK) :: found

    !get info about this variable:
    call json%info(p,var_type=var_type,name=str)

    !it must be a string named "name":
    if (var_type==json_string .and. str=='name') then
        call json%get(p,'@',str)               ! get original name
        call json%update(p,'@',new_name,found) ! change it
        write(error_unit,'(A)') str//' name changed to '//new_name
        icount = icount + 1
    end if

    !cleanup:
    if (allocated(str)) deallocate(str)

    !always false, since we want to traverse all nodes:
    finished = .false.

    end subroutine rename

end module jf_test_14_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_14

    !! 14th unit test.

    use jf_test_14_mod, only: test_14
    implicit none
    integer :: n_errors
    call test_14(n_errors)
    if ( n_errors /= 0) stop 1

end program jf_test_14
!*****************************************************************************************
#endif
