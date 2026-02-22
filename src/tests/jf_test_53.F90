!*****************************************************************************************
!>
!  Module for the 53rd unit test.
!  An example of iterating through a JSON object using get_child and get_next.

module jf_test_53_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_53

contains

    subroutine test_53(error_cnt)

    !! 53rd unit test.

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p_root, p_child
    character(kind=CK,len=:),allocatable :: key
    integer(IK) :: count, i

    character(kind=CK,len=*),parameter :: str = CK_'{'//&
                                                   '  "a": 1,'//&
                                                   '  "b": 2,'//&
                                                   '  "c": 3'//&
                                                   '}'

    character(kind=CK,len=1),dimension(3),parameter :: expected_keys = [CK_'a', CK_'b', CK_'c']

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 53'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize()
    call json%deserialize(p_root, str)
    if (json%failed()) then
        error_cnt = error_cnt + 1
        write(error_unit,'(A)') 'Error reading JSON string: '//trim(str)
    else
        call json%info(p_root,n_children=count)  ! there should be 3 children
        if (count /= 3) then
            error_cnt = error_cnt + 1
            write(error_unit,'(A,I0)') 'Error: expected 3 children, got ', count
        else
            write(output_unit,'(A,I0)') 'Success: expected 3 children, got ', count
            call json%get_child(p_root, 1, p_child) ! get the first one
            do i = 1, count
                call json%info(p_child, name=key) ! get the key name
                write(output_unit,'(A,I3,A,A)') 'Key ', i, ': ', trim(key)
                if (key /= expected_keys(i)) then
                    error_cnt = error_cnt + 1
                    write(error_unit,'(A,I3,A)') '  Error: expected key ', i, ' to be "'// &
                                                   trim(expected_keys(i))//'" but got "'//trim(key)//'"'
                end if
                ! get the next one (more efficient than calling get_child again)
                if (i<count) call json%get_next(p_child, p_child)
            end do
        end if

    end if

    call json%destroy(p_root)

    end subroutine test_53

end module jf_test_53_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_53

    use jf_test_53_mod , only: test_53

    implicit none
    integer :: n_errors

    call test_53(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_53
#endif
!*****************************************************************************************

