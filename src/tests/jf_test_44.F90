!*****************************************************************************************
!>
! Module for the 44th unit test

module jf_test_44_mod

    use json_module, CK => json_CK, IK => json_IK, RK => json_RK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_44

contains

    subroutine test_44(error_cnt)

    !! testing recursive `json_get_array`

    implicit none

    integer,intent(out) :: error_cnt !! error counter

    character(kind=CK,len=*),parameter :: str = CK_'{ "x": [0, [1,2,3], [4,5,6] ] }'

    type(json_core) :: json
    type(json_value),pointer :: p, x
    logical :: found

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 44'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0

    call json%initialize(path_mode=1_IK)
    call json%deserialize(p,str)

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        call json%get(p,CK_'x',x)
        call json%get(x, callback)
    end if

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%destroy(p)

    if (error_cnt==0) then
        write(error_unit,'(A)') 'Success!'
    else
        write(error_unit,'(A)') 'Failed!'
    end if
    write(error_unit,'(A)') ''

    end subroutine test_44

    recursive subroutine callback(json, element, i, count)
        !! Array element callback function.  Used by [[json_get_array]]

        implicit none

        class(json_core),intent(inout)       :: json
        type(json_value),pointer,intent(in)  :: element
        integer(IK),intent(in)               :: i        !! index
        integer(IK),intent(in)               :: count    !! size of array

        integer(IK) :: var_type
        character(kind=CK,len=:),allocatable :: path
        integer(IK) :: ival

        call json%get_path(element, path)

        call json%info(element,var_type=var_type)

        if (var_type == json_array) then
            write(output_unit,'(A)') 'array: '//trim(path)
            call json%get(element, callback)
        else if (var_type == json_integer) then
            call json%get(element,ival)
            write(output_unit,'(A,1X,I2)') trim(path)//' = ', ival
        end if

    end subroutine callback

end module jf_test_44_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_44

    !! 44th unit test.

    use jf_test_44_mod , only: test_44
    implicit none
    integer :: n_errors
    call test_44(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_44
!*****************************************************************************************
#endif




