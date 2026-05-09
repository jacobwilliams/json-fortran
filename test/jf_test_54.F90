!*****************************************************************************************
!>
!  Module for the 54th unit test.
!  Tests for the hash module.

module jf_test_54_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK, RK => json_RK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    use json_string_utilities, only: integer_to_string

    implicit none

    private
    public :: test_54

contains

    subroutine test_54(error_cnt)

    !! 54th unit test.

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_hash_table) :: hash
    type(json_value),pointer :: p, p_val
    character(kind=CK, len=10) :: str
    integer(IK) :: i, j !! counter
    logical(LK) :: status_ok !! status flag for hash table operations
    integer(IK) :: var_type !! type of value retrieved from JSON
    integer(IK) :: ival !! value retrieved from JSON
    real(RK) :: t_start, t_end  !! timers for performance measurement

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 54'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(trailing_spaces_significant = .false., &
                         case_sensitive_keys = .false.)

    ! generate a JSON structure with 1000 members
    call json%create_object(p, CK_'root')
    do i = 1, 1000
        call integer_to_string(i,'(I5)',str)
        call json%add(p, str, i)  ! key is a string of the integer, value is the integer
    end do

    ! create a hash table for the root object
    call hash%create(json, p, status_ok)
    if (.not. status_ok) then
        write(error_unit,'(A)') 'Error creating hash table'
        error_cnt = error_cnt + 1
        return
    end if

    ! retrieve some values using the hash table
    call cpu_time(t_start)
    do i = 1, 10000

        ! compute a random integer between 1 and 1000 to use as a key for retrieval
        j = mod(i * 12345_IK, 1000_IK) + 1_IK

        call integer_to_string(j,'(I5)',str)
        p_val => hash%get(str, status_ok)
        if (.not. status_ok) then
            write(error_unit,'(A)') 'Error retrieving value from hash table for key: '// trim(str)
            error_cnt = error_cnt + 1
        else if (associated(p_val)) then
            call json%info(p_val, var_type = var_type)
            call json%get(p_val, ival)
            if (var_type /= json_integer) then
                write(error_unit,'(A)') 'Incorrect type retrieved from hash table for key: '// trim(str)
                write(error_unit,'(A, I5)') '  Expected type:  ', json_integer
                write(error_unit,'(A, I5)') '  Retrieved type: ', var_type
                error_cnt = error_cnt + 1
            else if (ival /= j) then
                write(error_unit,'(A)') 'Incorrect value retrieved from hash table for key: '// trim(str)
                error_cnt = error_cnt + 1
            end if
        else
            write(error_unit,'(A)') 'Null pointer retrieved from hash table for key: '// trim(str)
            error_cnt = error_cnt + 1
        end if
    end do
    call cpu_time(t_end)
    write(output_unit,'(A, F6.3, A)') 'Time taken for 10,000 hash retrievals:        ', t_end - t_start, ' seconds'

    ! now, compute the time to retrieve values without using the hash table
    ! (i.e. by searching through the JSON object members)
    call cpu_time(t_start)
    do i = 1, 10000

        ! compute a random integer between 1 and 1000 to use as a key for retrieval
        j = mod(i * 12345_IK, 1000_IK) + 1_IK

        call integer_to_string(j,'(I5)',str)
        call json%get(p, str, p_val, status_ok)
        if (.not. status_ok) then
            write(error_unit,'(A)') 'Error retrieving value from JSON object for key: '// trim(str)
            error_cnt = error_cnt + 1
        else if (associated(p_val)) then
            call json%info(p_val, var_type = var_type)
            call json%get(p_val, ival)
            if (var_type /= json_integer) then
                write(error_unit,'(A)') 'Incorrect type retrieved from JSON object for key: '// trim(str)
                write(error_unit,'(A, I5)') '  Expected type:  ', json_integer
                write(error_unit,'(A, I5)') '  Retrieved type: ', var_type
                error_cnt = error_cnt + 1
            else if (ival /= j) then
                write(error_unit,'(A)') 'Incorrect value retrieved from JSON object for key: '// trim(str)
                error_cnt = error_cnt + 1
            end if
        else
            write(error_unit,'(A)') 'Null pointer retrieved from JSON object for key: '// trim(str)
            error_cnt = error_cnt + 1
        end if
    end do
    call cpu_time(t_end)
    write(output_unit,'(A, F6.3, A)') 'Time taken for 10,000 JSON object retrievals: ', t_end - t_start, ' seconds'
    write(output_unit, '(A)') ''

    end subroutine test_54

end module jf_test_54_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_54

    use jf_test_54_mod , only: test_54

    implicit none
    integer :: n_errors

    call test_54(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_54
#endif
!*****************************************************************************************

