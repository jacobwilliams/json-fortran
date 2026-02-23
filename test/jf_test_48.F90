!*****************************************************************************************
!>
!  Module for the 48th unit test.

module jf_test_48_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_48

    character(len=*),parameter :: filename_large = 'files/inputs/big.json' !! large file to open
    character(len=*),parameter :: filename_large2 = 'files/inputs/random1.json' !! large file to open

    character(len=*),parameter :: filename_small = 'files/inputs/test1.json' !! small file to open
    character(len=*),parameter :: filename_small2 = 'files/inputs/test2.json' !! small file to open
    character(len=*),parameter :: filename_small3 = 'files/inputs/test5.json' !! small file to open

contains

    subroutine test_48(error_cnt)

    !! Clone test

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json  !! factory for manipulating `json_value` pointers

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 48'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! recursive parser
    write(error_unit,'(A)') '.................................'
    write(error_unit,'(A)') ' Recursive Parser'
    write(error_unit,'(A)') '.................................'
    call json%initialize(trailing_spaces_significant=.true., parser_mode = 1_IK)
    call go()

    ! nonrecursive parser
    write(error_unit,'(A)') '.................................'
    write(error_unit,'(A)') ' Nonrecursive Parser'
    write(error_unit,'(A)') '.................................'
    call json%initialize(trailing_spaces_significant=.true., parser_mode = 2_IK)
    call go()

    contains

        subroutine go()
            ! only test the non-recursive clone function on large files,
            ! since the recursive function may hit the recursion limit
            ! and stack overflow.
            call test(filename_large,  use_nonrecursive=.true.)
            call test(filename_large2, use_nonrecursive=.true.)

            ! for smaller ones, can test both:
            call test(filename_small, use_nonrecursive=.false.)
            call test(filename_small, use_nonrecursive=.true.)

            call test(filename_small2, use_nonrecursive=.false.)
            call test(filename_small2, use_nonrecursive=.true.)

            call test(filename_small3, use_nonrecursive=.false.)
            call test(filename_small3, use_nonrecursive=.true.)
        end subroutine go

        subroutine test(filename, use_nonrecursive)
        character(len=*),intent(in) :: filename
        logical(LK),intent(in) :: use_nonrecursive

        type(json_value),pointer :: p, p_clone

        character(kind=CK,len=:),allocatable :: s1, s2

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') '--------------------------------'
        write(error_unit,'(A)') '*** Testing clone function with file: '//trim(filename)
        if (use_nonrecursive) then
            write(error_unit,'(A)') '    Using NON-RECURSIVE clone function'
        else
            write(error_unit,'(A)') '    Using RECURSIVE clone function'
        end if
        write(error_unit,'(A)') ''

        error_cnt = 0
        ! call json%initialize(trailing_spaces_significant=.true.)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        call json%load(filename, p)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        !test the deep copy routine:

        ! First test: does p equal itself?
        if (.not. json%equals(p, p)) then
            write(error_unit,'(A)') 'ERROR: structure does not equal itself!'
            error_cnt = error_cnt + 1
        end if

        call json%clone(p,p_clone,use_nonrecursive=use_nonrecursive)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        ! make sure both structures are the same:
        if (.not. json%equals(p, p_clone)) then
            write(error_unit,'(A)') 'ERROR: cloned structure is not equal to the original'
            if (json%failed()) then
                write(error_unit,'(A)') '  (equals function threw an exception)'
                call json%print_error_message(error_unit)
                call json%clear_exceptions()
            end if
            error_cnt = error_cnt + 1
            ! debugging: print both structures
            call json%serialize(p, s1)
            call json%serialize(p_clone, s2)
            write(error_unit,'(A)') 'Original:'
            write(error_unit,'(A)') trim(adjustl(s1))
            write(error_unit,'(A)') 'Clone:'
            write(error_unit,'(A)') trim(adjustl(s2))
        else
            write(error_unit,'(A)') 'SUCCESS: cloned structure is equal to the original'
        end if

        call json%destroy(p)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        call json%destroy(p_clone)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        if (error_cnt==0) then
            write(error_unit,'(A)') 'Success'
        else
            write(error_unit,'(A)') 'Failed'
        end if

        write(error_unit,'(A)') '--------------------------------'

        end subroutine test

    end subroutine test_48

end module jf_test_48_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_48

    !! clone test

    use jf_test_48_mod , only: test_48
    implicit none
    integer :: n_errors
    integer :: i !! counter

    integer,parameter :: n_repeat = 1 !! number of times to repeat the test

    do i = 1, n_repeat
        call test_48(n_errors)
        if (n_errors /= 0) stop 1
    end do

end program jf_test_48
#endif
!*****************************************************************************************
