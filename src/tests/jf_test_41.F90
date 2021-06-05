!*****************************************************************************************
!>
! Module for the 41st unit test.

module jf_test_41_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_41

contains

    subroutine test_41(error_cnt)

    !! Test finalizer

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p, p2
    type(json_core) :: json
    type(json_file) :: f, f2, f3, f4
    character(kind=CK,len=:),allocatable :: str

    character(kind=CK,len=*),parameter :: json_str = &
            '{"str_array": ["1","22","333"]}'

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 41'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(no_whitespace=.true.)

    write(error_unit,'(A)') 'parsing...'
    call json%deserialize(p,json_str)
    call json%deserialize(p2,json_str)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'printing...'
        call json%print(p,int(output_unit,IK))

        write(error_unit,'(A)') ''
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'copying to json_file...'

        f = json_file(p)

        call f2%add(p2)
        nullify(p2) ! data is now in f

        if (f%failed()) then
            call f%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,'(A)') ''
            write(error_unit,'(A)') 'printing...'
            call f%initialize(no_whitespace=.true.)
            call f%print() ! print to console
            if (f%failed()) then
                call f%print_error_message(error_unit)
                error_cnt = error_cnt + 1
            else

                write(error_unit,'(A)') ''
                write(error_unit,'(A)') ''
                write(error_unit,'(A)') 'make two deep copies and print...'

                f3 = f
                f4 = f

                call f%print()
                call f3%print()
                call f4%print()

                write(error_unit,'(A)') ''
                write(error_unit,'(A)') ''
                write(error_unit,'(A)') 'string assignment...'

                str = f3
                write(error_unit,'(A)') str

                if (f%failed()) then
                    call f%print_error_message(error_unit)
                    error_cnt = error_cnt + 1
                end if
                if (f3%failed()) then
                    call f3%print_error_message(error_unit)
                    error_cnt = error_cnt + 1
                end if
                if (f4%failed()) then
                    call f4%print_error_message(error_unit)
                    error_cnt = error_cnt + 1
                end if

            end if
        end if

    end if

    write(error_unit,'(A)') ''
    if (error_cnt==0) then
        write(error_unit,'(A)') 'finished: Success'
    else
        write(error_unit,'(A)') 'finished: Failed!'
    end if

    end subroutine test_41

end module jf_test_41_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_41

    !! 41st unit test.

    use jf_test_41_mod , only: test_41
    implicit none
    integer :: n_errors
    call test_41(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_41
!*****************************************************************************************
#endif
