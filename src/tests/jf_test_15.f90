!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/28/2015
!
! Module for the 15th unit test.
! This one is testing a lot of the error conditions.

module jf_test_15_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none
    
contains

    subroutine test_15(error_cnt)
    
    !! Test some of the edge cases, and incorrect usages.
    
    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller
    
    type(json_value),pointer  :: json,p
    type(json_file) :: file1, file2
    logical :: found,status_ok 
    integer :: var_type,i,n_children
    double precision :: d
    logical :: tf
    character(kind=CK,len=:),allocatable :: error_msg
    
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 15'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0
    
    nullify(json)
    nullify(p)
    
    call json_parse(json, '{"int": 1, "real": 2.0, "logical": true}')
    call json_get(json,'real',   i)
    call json_get(json,'logical',i)
    call json_get(json,'integer',d)
    call json_get(json,'logical',d)
    call json_get(json,'integer',tf)
    call json_get(json,'real',   tf)
        
    call json_check_for_errors(status_ok, error_msg)  !error condition true
    
    call json_initialize(print_signs=.true.)  !print signs flag
    
    call json_check_for_errors(status_ok, error_msg)  !error condition false
   
    call file1%move(file2) !should throw an exception since points are not associated
    
    call file1%print_file(-1)   !invalid input
    
    call file1%print_file(filename='') !invalid filename
    
    call file1%info('this path does not exist',found,var_type,n_children)
    
    call json_initialize()
    
    call json_get_child(json,-99,p)  !invalid index
    
    call json_initialize()  !clear exceptions
    
    call json_get_child(json,'this child does not exist',p)  !invalid index
    
    call json_initialize()  !clear exceptions

    call json_print(json,-1) !invalid input
    call json_print(json,filename='') !invalid input

    call json_initialize()  !clear exceptions        
            
    end subroutine test_15

end module jf_test_15_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_15

    !! 15th unit test.
    
    use jf_test_15_mod, only: test_15
    implicit none
    integer :: n_errors
    call test_15(n_errors)
    if ( n_errors /= 0) stop 1
    
end program jf_test_15
!*****************************************************************************************
