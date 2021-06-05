!*****************************************************************************************
!> author: Jacob Williams
!  date: 10/28/2015
!
! Module for the 15th unit test.
! This one is testing a lot of the error conditions.

module jf_test_15_mod

    use json_module, CK => json_CK, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit,output_unit

    implicit none

    private
    public :: test_15

contains

    subroutine test_15(error_cnt)

    !! Test some of the edge cases, and incorrect usages.

    implicit none

    integer,intent(out) :: error_cnt !! report number of errors to caller

    type(json_core) :: json
    type(json_value),pointer :: p,p2,p3
    type(json_file) :: file1, file2
    logical(LK) :: status_ok
    integer(IK) :: var_type,i,n_children
    real(wp) :: d
    logical(LK) :: tf
    character(kind=CK,len=:),allocatable :: error_msg
    integer(IK),dimension(:),allocatable :: ivec
    real(wp),dimension(:),allocatable    :: rvec
    logical(LK),dimension(:),allocatable :: lvec
    character(kind=CK,len=:),allocatable :: name

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 15'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    nullify(p2)
    nullify(p)

    call json%initialize(strict_type_checking=.true.)
    call json%deserialize(p2, '{"int": 1, "real": 2.0, "logical": true, "vec": [1, 1.0, "1.0", false]}')
    if (json%failed()) then
        error_cnt=error_cnt+1
        call json%print_error_message(error_unit)
    else

        ! these should all raise exceptions:
        call json%get(p2,'real',   i)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: real'
        end if
        call json%initialize()

        call json%get(p2,'logical',i)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: logical'
        end if
        call json%initialize()

        call json%get(p2,'integer',d)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: integer'
        end if
        call json%initialize()

        call json%get(p2,'logical',d)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: logical'
        end if
        call json%initialize()

        call json%get(p2,'integer',tf)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: integer'
        end if
        call json%initialize()

        call json%get(p2,'real',   tf)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: real'
        end if

        !****************************************
        ! test exceptions when trying to get a vector:
        call json%get(p2,'vec',ivec)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: ivec'
        end if
        call json%initialize()

        call json%get(p2,'vec',rvec)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: rvec'
        end if
        call json%initialize()

        call json%get(p2,'vec',lvec)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: lvec'
        end if
        call json%initialize()

        call json%check_for_errors(status_ok, error_msg)  !error condition true
        call json%check_for_errors(error_msg=error_msg)   !error condition true
        call json%initialize(print_signs=.true.)  !print signs flag

        call json%check_for_errors(status_ok, error_msg)  !error condition false
        call json%check_for_errors(status_ok)             !error condition false
        call json%check_for_errors(error_msg=error_msg)   !error condition false - not allocated

        call file1%move(file2) !should throw an exception since pointers are not associated
        call file1%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: move'
        end if
        call file1%initialize()

        call file1%print(-1_IK)   !invalid input
        call file1%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: print to invalid unit'
        end if
        call file1%initialize()

        call file1%print(filename='') !invalid filename
        call file1%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: print to invalid filename'
        end if
        call file1%initialize()

        call file1%info('this path does not exist',var_type=var_type,n_children=n_children)
        call file1%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: path that does not exist'
        end if

        call file1%check_for_errors(status_ok,error_msg)
        call file1%clear_exceptions()
        call file1%destroy()

        call json%initialize( verbose                     = .false., &
                              compact_reals               = .true.,  &
                              print_signs                 = .false., &
                              real_format                 = 'E',     &
                              spaces_per_tab              = 4_IK,    &
                              strict_type_checking        = .true.,  &
                              trailing_spaces_significant = .false., &
                              case_sensitive_keys         = .true.   )

        call json%get_child(p2,-99_IK,p)  !invalid index
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: invalid index'
        end if
        call json%initialize()

        call json%get_child(p2,'this child does not exist',p)  !invalid index
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: invalid index'
        end if
        call json%initialize()

        call json%print(p2,-1_IK) !invalid input
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: invalid input'
        end if
        call json%initialize()

        call json%print(p2,filename='') !invalid input
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: invalid input'
        end if
        call json%initialize()

        !****************************************

        file1 = json_file(p2,json)  !constructor
        call file1%destroy(destroy_core=.true.)

        !****************************************

        ! try to get info for an unassociated pointer,
        ! this should raise an exception:
        p3 => null()
        call json%info(p3,var_type,n_children,name)
        call json%check_for_errors(status_ok)
        if (status_ok) then
            error_cnt=error_cnt+1
            write(error_unit,'(A)') 'Error: info for unassociated pointer'
        end if

        !****************************************

    end if

    if (error_cnt>0) then
        write(error_unit,'(A)') ' FAILED!'
    else
        write(error_unit,'(A)') ' Success!'
    end if

    end subroutine test_15

end module jf_test_15_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
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
