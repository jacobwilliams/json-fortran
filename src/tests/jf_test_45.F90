!*****************************************************************************************
!>
! Module for the 45th unit test

module jf_test_45_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_45

contains

    subroutine test_45(error_cnt)

    !! testing of `json_get_path`

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{ "x": [[1], [1,2,3,[4]]] }'

    type(json_core) :: json
    type(json_value),pointer :: p, x
    logical(LK) :: found
    integer(IK) :: ival
    integer(IK) :: path_mode
    character(kind=CK,len=:),allocatable :: key

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 45'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%deserialize(p,str)
    call json%print(p)

    do path_mode = 1_IK, 3_IK

        write(output_unit,'(A)') ''
        write(output_unit,'(A)') '------------------------------'
        write(output_unit,'(A,1X,I2)') 'path_mode = ', path_mode
        write(output_unit,'(A)') '------------------------------'

        call json%initialize(path_mode=path_mode)

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            select case (path_mode)
            case(1_IK)
                key = CK_'x'
            case(2_IK)
                key = CK_'/x'
            case(3_IK)
                key = CK_"$['x']"
            end select
            call json%get(p,key,x)
            call json%traverse(x, callback)
        end if

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if

        ! now, try to get values using the path:
        write(output_unit,'(A)') ''
        select case (path_mode)
        case(1_IK)
            key = CK_'x[2][4][1]'
        case(2_IK)
            key = CK_'/x/1/3/0'
        case(3_IK)
            key = CK_"$['x'][2][4][1]"
        end select
        call json%get(p, key, ival, found)
        if (found) then
            if (ival == 4_IK) then
                write(output_unit,'(A)') 'Successfully got '//key//' = 4'
            else
                write(error_unit,'(A)') 'Error: '//key//' /= 4'
                error_cnt = error_cnt + 1
            end if
        else
            write(error_unit,'(A)') 'Error: could not find '//key
            error_cnt = error_cnt + 1
        end if

    end do

    call json%destroy(p)

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if
    write(error_unit,'(A)') ''

    end subroutine test_45

    subroutine callback(json,p,finished)
        !! Callback function used by [[json_traverse]]

        implicit none

        class(json_core),intent(inout)      :: json
        type(json_value),pointer,intent(in) :: p
        logical(LK),intent(out)             :: finished  !! set true to stop traversing

        integer(IK) :: var_type
        character(kind=CK,len=:),allocatable :: path
        integer(IK) :: ival

        call json%get_path(p, path)

        call json%info(p,var_type=var_type)

        if (var_type == json_array) then
            write(output_unit,'(A)') ''
            write(output_unit,'(A)') trim(path)
        else if (var_type == json_integer) then
            call json%get(p,ival)
            write(output_unit,'(A,1X,I2)') trim(path)//' = ', ival
        end if

        finished = .false.

    end subroutine callback

end module jf_test_45_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_45

    !! 45th unit test.

    use jf_test_45_mod , only: test_45
    implicit none
    integer :: n_errors
    call test_45(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_45
!*****************************************************************************************
#endif




