!*****************************************************************************************
!>
!  Unit test for [[json_value_reverse]].
!
!@note This uses Fortran 2008 auto LHS assignments.

    program jf_test_28

    use json_module
    use iso_fortran_env

    implicit none

    type(json_core) :: json
    type(json_value),pointer :: p,vec
    integer(json_IK),dimension(:),allocatable :: ivec
    integer(json_IK),dimension(:),allocatable :: ivec_value,ivec_value_reversed
    character(kind=json_CK,len=:),allocatable :: str
    integer :: i !! counter

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

        call json%parse(p,str)
        call json%get(p,'vec',vec)

        write(output_unit,'(A)') ''
        write(output_unit,'(A)') 'Original:'
        write(output_unit,'(A)') ''
        call json%print(vec,output_unit)

        call json%reverse(vec)

        write(output_unit,'(A)') ''
        write(output_unit,'(A)') 'Reversed:'
        write(output_unit,'(A)') ''
        call json%print(vec,output_unit)

        call json%get(vec,ivec)
        call json%destroy(p)

        if (json%failed()) then
            call json%print_error_message(error_unit)
            stop 1
        else

            if (allocated(ivec)) then
                if (i/=4) then
                    if (all(ivec==ivec_value_reversed)) then
                        write(output_unit,'(A)') 'reverse test passed'
                    else
                        write(output_unit,'(A,*(I3,1X))') 'reverse test failed: ', ivec
                        stop 1
                    end if
                else
                    if (size(ivec)==0) then
                        write(output_unit,'(A)') 'reverse test passed'
                    else
                        write(output_unit,'(A,*(I3,1X))') 'reverse test failed: ', ivec
                        stop 1
                    end if
                end if
            else
                write(output_unit,'(A)') 'reverse test failed: error getting ivec'
                stop 1
            end if

        end if

    end do

    end program jf_test_28
!*****************************************************************************************
