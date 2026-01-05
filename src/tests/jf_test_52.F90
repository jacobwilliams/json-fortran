!*****************************************************************************************
!>
!  Module for the 52th unit test.
!  Testing `add_null_by_path`.

module jf_test_52_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_52

contains

    subroutine test_52(error_cnt)

    !! 52nd unit test.

    integer,intent(out) :: error_cnt

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 52'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    ! run it twice, once with default character kind, once with CK kind,
    ! so both routines are tested if using unicode.
    call go(error_cnt, 1)
    call go(error_cnt, 2)

    end subroutine test_52

    subroutine go(error_cnt, ichar)

        integer,intent(inout) :: error_cnt
        integer,intent(in) :: ichar

        type(json_core) :: json_c
        type(json_file) :: json
        type(json_value),pointer :: p1, p2

        character(kind=CK,len=*),parameter :: str = '{'//&
                                                    '  "null": null,'//&
                                                    '  "null_vec": ['//&
                                                    '    null,'//&
                                                    '    null'//&
                                                    '  ]'//&
                                                    '}'

        call json%initialize()
        select case (ichar)
        case(1)
            write(error_unit,'(A)') 'using default character kind...'
            call json%add_null('null')
            call json%add_null('null_vec', 2_IK)
        case(2)
            write(error_unit,'(A)') 'using CK kind...'
            call json%add_null(CK_'null')
            call json%add_null(CK_'null_vec', 2_IK)
        end select
        call json%print()
        if (json%failed()) then
            error_cnt = error_cnt + 1
            write(error_unit,'(A)') 'Error adding null values'
        end if

        ! compare to the string and verify they are the same:
        call json_c%deserialize(p1, str)
        call json%get(p2)
        if (.not. json_c%equals(p1, p2)) then
            error_cnt = error_cnt + 1
            write(error_unit,'(A)') 'Error: deserialized JSON does not equal JSON file content'
        else
            write(error_unit,'(A)') 'Success: deserialized JSON equals JSON file content'
        end if
        write(error_unit,'(A)') ''

    end subroutine go

end module jf_test_52_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_52

    use jf_test_52_mod , only: test_52

    implicit none
    integer :: n_errors

    call test_52(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_52
#endif
!*****************************************************************************************

