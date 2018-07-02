!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/28/2015
!
! Module for the 15th unit test.
! This one is testing a lot of the error conditions.

module jf_test_15_mod

    use json_module, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit,wp=>real64

    implicit none

    private
    public :: test_15

contains

    subroutine test_15(error_cnt)

    !! Test some of the edge cases, and incorrect usages.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,p2
    type(json_file) :: file1, file2
    logical :: found,status_ok
    integer :: var_type,i,n_children
    real(wp) :: d
    logical :: tf
    character(kind=CK,len=:),allocatable :: error_msg

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 15'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    nullify(p2)
    nullify(p)

    call json%parse(p2, '{"int": 1, "real": 2.0, "logical": true}')
    call json%get(p2,'real',   i)
    call json%get(p2,'logical',i)
    call json%get(p2,'integer',d)
    call json%get(p2,'logical',d)
    call json%get(p2,'integer',tf)
    call json%get(p2,'real',   tf)
    call json%check_for_errors(status_ok, error_msg)  !error condition true
    call json%initialize(print_signs=.true.)  !print signs flag

    call json%check_for_errors(status_ok, error_msg)  !error condition false

    call file1%move(file2) !should throw an exception since points are not associated
    call file1%initialize()

    call file1%print_file(-1)   !invalid input
    call file1%initialize()

    call file1%print_file(filename='') !invalid filename
    call file1%initialize()

    call file1%info('this path does not exist',found,var_type,n_children)
    call file1%initialize()

    call file1%check_for_errors(status_ok,error_msg)
    call file1%clear_exceptions()
    call file1%destroy()
    file1 = json_file(p2,json)  !constructor
    call file1%destroy(destroy_core=.true.)

    call json%initialize(   verbose=.false.,&
                            compact_reals=.true.,&
                            print_signs=.false.,&
                            real_format='E',&
                            spaces_per_tab=4,&
                            strict_type_checking=.true.,&
                            trailing_spaces_significant=.false.,&
                            case_sensitive_keys=.true.)

    call json%get_child(p2,-99,p)  !invalid index
    call json%initialize()  !clear exceptions

    call json%get_child(p2,'this child does not exist',p)  !invalid index
    call json%initialize()  !clear exceptions

    call json%print(p2,-1) !invalid input
    call json%initialize()  !clear exceptions

    call json%print(p2,filename='') !invalid input
    call json%initialize()  !clear exceptions

    end subroutine test_15

end module jf_test_15_mod
!*****************************************************************************************

#ifndef INTERGATED_TESTS
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
#endif
