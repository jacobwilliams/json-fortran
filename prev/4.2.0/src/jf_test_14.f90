!*****************************************************************************************
!> author: Jacob Williams
!  date: 09/02/2015
!
! Module for the 14th unit test.

module jf_test_14_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none
    
    character(len=*),parameter :: dir = '../files/inputs/'  !! working directory
    character(len=*),parameter :: filename1 = 'test1.json'  !! the file to read
    integer :: icount = 0  !! a count of the number of "name" variables found

contains

    subroutine test_14(error_cnt)
    
    !! Tests the traversal of a JSON structure
    !!
    !! It traverses the structure, looks for all "name" variables, and changes the name.
    
    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller
    
    type(json_value),pointer  :: json
        
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 14'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0
    icount = 0 !number of name changes (should be 2)
        
    call json_initialize() !initialize the module
    
    call json_parse(dir//filename1,json)  !read the file
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    
    call json_traverse(json,rename) !traverse all nodes in the structure
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    if (icount/=2) then
        write(error_unit,'(A)') 'Error: should be 2 "name" variables in this file: '//filename1
        error_cnt = error_cnt + 1
    end if 
    
    if (error_cnt==0) then
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ' All names changed to Fred:'
        write(error_unit,'(A)') ''
        call json_print(json,output_unit)
        write(error_unit,'(A)') ''
    end if
    
    call json_destroy(json)  !clean up
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
        
    end subroutine test_14
    
    subroutine rename(p,finished)  !! change all "name" variable values to "Fred"
    
    implicit none
    
    type(json_value),pointer,intent(in) :: p
    logical,intent(out) :: finished
    
    integer :: var_type
    character(kind=CK,len=:),allocatable :: str
    logical :: found
    
    !get info about this variable:
    call json_info(p,var_type=var_type,name=str)
    
    !it must be a string named "name":
    if (var_type==json_string .and. str=='name') then
        call json_get(p,'@',str)             ! get original name
        call json_update(p,'@','Fred',found) !change it
        write(error_unit,'(A)') str//' name changed'
        icount = icount + 1
    end if
    
    !cleanup:
    if (allocated(str)) deallocate(str)
    
    !always false, since we want to traverse all nodes:
    finished = .false.
   
    end subroutine rename

end module jf_test_14_mod
!*****************************************************************************************

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
