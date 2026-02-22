!*****************************************************************************************
!>
!  Module for the 28th unit test.
!  Unit test for [[json_value_reverse]].
!
!@note This uses Fortran 2008 auto LHS assignments.

module jf_test_28_mod

    use json_module, IK => json_IK
    use iso_fortran_env

    implicit none

    private
    public :: test_28

contains

    subroutine test_28(error_cnt)

    implicit none

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p,vec
    integer(IK),dimension(:),allocatable :: ivec
    integer(IK),dimension(:),allocatable :: ivec_value
    integer(IK),dimension(:),allocatable :: ivec_value_reversed
    character(kind=json_CK,len=:),allocatable :: str
    integer :: i !! counter

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   TEST 28'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(compress_vectors=.true.)

    do i=1,4

        ! all the cases:
        select case (i)
        case(1)
            str = json_CK_'{"vec":[1,2,3,4,5]}'
            ivec_value = [1,2,3,4,5]
            ivec_value_reversed = [5,4,3,2,1]
        case(2)
            str = json_CK_'{"vec":[1]}'
            ivec_value = [1]
            ivec_value_reversed = [1]
        case(3)
            str = json_CK_'{"vec":[1,2]}'
            ivec_value = [1,2]
            ivec_value_reversed = [2,1]
        case(4)
            str = json_CK_'{"vec":[]}'
            !ivec_value = []
            !ivec_value_reversed = []
        end select

        call json%deserialize(p,str)
        call json%get(p,'vec',vec)

        write(output_unit,'(A)') ''
        write(output_unit,'(A)') 'Original:'
        write(output_unit,'(A)') ''
        call json%print(vec,int(output_unit,IK))

        call json%reverse(vec)

        write(output_unit,'(A)') ''
        write(output_unit,'(A)') 'Reversed:'
        write(output_unit,'(A)') ''
        call json%print(vec,int(output_unit,IK))

        call json%get(vec,ivec)
        call json%destroy(p)

        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else

            if (allocated(ivec)) then
                if (i/=4) then
                    if (all(ivec==ivec_value_reversed)) then
                        write(output_unit,'(A)') 'reverse test passed'
                    else
                        write(output_unit,'(A,*(I3,1X))') 'reverse test failed: ', ivec
                        error_cnt = error_cnt + 1
                    end if
                else
                    if (size(ivec)==0) then
                        write(output_unit,'(A)') 'reverse test passed'
                    else
                        write(output_unit,'(A,*(I3,1X))') 'reverse test failed: ', ivec
                        error_cnt = error_cnt + 1
                    end if
                end if
            else
                write(output_unit,'(A)') 'reverse test failed: error getting ivec'
                error_cnt = error_cnt + 1
            end if

        end if

    end do

    end subroutine test_28

    end module jf_test_28_mod
!*****************************************************************************************

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_28

    !! 28th unit test.

    use jf_test_28_mod , only: test_28
    implicit none
    integer :: n_errors
    call test_28(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_28
!*****************************************************************************************
#endif
