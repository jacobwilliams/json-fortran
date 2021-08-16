!*****************************************************************************************
!>
! Module for the 47th unit test

module jf_test_47_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_47

contains

    subroutine test_47(error_cnt)

    !! testing of `remove`.

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*), parameter :: STR = CK_'&
        &{&
        &  "object1": {&
        &    "a": 1,&
        &    "b": 2,&
        &    "move1": 3,&
        &    "move2": 4,&
        &    "e": 5 &
        &  },&
        &  "object2": {&
        &    "f": 10,&
        &    "g": 11,&
        &    "h": 12,&
        &    "i": 13,&
        &    "j": 14 &
        &  }&
        &}'

    character(kind=CK,len=:), allocatable :: errtxt
    type(json_core)               :: json
    type(json_value), pointer     :: inp, p, p2
    logical(LK)                   :: found, status_ok
    integer(IK)                   :: ival

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 47'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%deserialize(inp, STR)
    call json%check_for_errors(status_ok, errtxt)
    if (.not. status_ok) then
        write(error_unit,'(A)') errtxt
        error_cnt = error_cnt + 1
    else
        call json%print(inp)
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Move object1.move1 to object2 [clone]...'
        call json%get(inp, 'object1.move1', p, found)
        if (found) then
            call json%clone(p, p2)
            call json%remove(p, .true.)
            call json%add_by_path(inp, 'object2.move1', p2)
        end if
        write(error_unit,'(A)') ''
        call json%print(inp)
        write(error_unit,'(A)') ''
        call json%get(inp, 'object2.move1', ival, found)
        if (.not. found) then
            write(error_unit,'(A)') 'Error moving move1'
            error_cnt = error_cnt + 1
        else
            if (ival==3_IK) then
                write(error_unit,'(A)') '...Success'
            else
                write(error_unit,'(A,1X,I5)') 'Invalid move1 value: ', ival
                error_cnt = error_cnt + 1
            end if
        end if

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'Move object1.move2 to object2...'

        call json%get(inp, 'object1.move2', p, found)
        if (found) then
            call json%remove(p, .false.)
            call json%add_by_path(inp, 'object2.move2', p)
        end if
        write(error_unit,'(A)') ''
        call json%print(inp)
        write(error_unit,'(A)') ''
        call json%get(inp, 'object2.move2', ival, found)
        if (.not. found) then
            write(error_unit,'(A)') 'Error moving move2'
            error_cnt = error_cnt + 1
        else
            if (ival==4_IK) then
                write(error_unit,'(A)') '...Success'
            else
                write(error_unit,'(A,1X,I5)') 'Invalid move2 value: ', ival
                error_cnt = error_cnt + 1
            end if
        end if

        call json%check_for_errors(status_ok, errtxt)
        if (.not. status_ok) then
            write(error_unit,'(A)') errtxt
            error_cnt = error_cnt + 1
        end if

    end if

    call json%destroy(inp)

    write(error_unit,'(A)') ''
    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if
    write(error_unit,'(A)') ''

    end subroutine test_47

end module jf_test_47_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_47

    !! 47th unit test.

    use jf_test_47_mod , only: test_47
    implicit none
    integer :: n_errors
    call test_47(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_47
!*****************************************************************************************
#endif