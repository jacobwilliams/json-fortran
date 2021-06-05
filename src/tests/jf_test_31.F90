!*****************************************************************************************
!>
!  Module for the 31st unit test.

module jf_test_31_mod

    use json_module, rk => json_rk, lk => json_lk, ik => json_ik, ck => json_ck, cdk => json_cdk
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_31

contains

    subroutine test_31(error_cnt)

    !! Some tests of the `strict_type_checking` mode.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_file) :: json
    integer(IK) :: ival
    real(RK)    :: rval
    logical(LK) :: lval
    character(kind=CK,len=:),allocatable :: cval

    character(kind=CK,len=*),parameter :: json_string = &
            '{ "int": 1, "double": 1.0, "int_false": 0, "double_false": 0.0, "logical": true, "logical_false": false, '//&
              '"string_int": "1", "string_double": "1.0", '//&
              '"int_str": "1", "double_str": "1.0", "logical_str": "true", "logical_str_false": "false" '//&
            '}'

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 31'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(strict_type_checking = .false.)

    call json%deserialize(json_string)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        ! get stuff:
        call json%get('int',               rval); if (.not. json%failed()) write(error_unit,*) rval
        call json%get('logical',           rval); if (.not. json%failed()) write(error_unit,*) rval
        call json%get('logical_false',     rval); if (.not. json%failed()) write(error_unit,*) rval
        call json%get('double',            rval); if (.not. json%failed()) write(error_unit,*) rval
        call json%get('string_double',     rval); if (.not. json%failed()) write(error_unit,*) rval

        call json%get('int',               ival); if (.not. json%failed()) write(error_unit,*) ival
        call json%get('logical',           ival); if (.not. json%failed()) write(error_unit,*) ival
        call json%get('logical_false',     ival); if (.not. json%failed()) write(error_unit,*) ival
        call json%get('double',            ival); if (.not. json%failed()) write(error_unit,*) ival
        call json%get('string_int',        ival); if (.not. json%failed()) write(error_unit,*) ival

        call json%get('logical',           lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('int',               lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('double',            lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('logical_str',       lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('logical_false',     lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('int_false',         lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('double_false',      lval); if (.not. json%failed()) write(error_unit,*) lval
        call json%get('logical_str_false', lval); if (.not. json%failed()) write(error_unit,*) lval

        call json%get('int',               cval); if (.not. json%failed()) write(error_unit,*) cval
        call json%get('logical',           cval); if (.not. json%failed()) write(error_unit,*) cval
        call json%get('logical_false',     cval); if (.not. json%failed()) write(error_unit,*) cval
        call json%get('double',            cval); if (.not. json%failed()) write(error_unit,*) cval
        call json%get('string_int',        cval); if (.not. json%failed()) write(error_unit,*) cval

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            write(error_unit,*) ''
            write(error_unit,*) 'Success!'
        end if

    end if

    call json%destroy()    ! free memory

    end subroutine test_31

end module jf_test_31_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_31

    !! 31st unit test.

    use jf_test_31_mod , only: test_31
    implicit none
    integer :: n_errors
    call test_31(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_31
!*****************************************************************************************
#endif
