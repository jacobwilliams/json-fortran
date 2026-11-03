!*****************************************************************************************
!>
!  Module for the 55th unit test. - same as 54 except using json_file.
!  Tests for the hash module.

module jf_test_55_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK, RK => json_RK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    use json_string_utilities, only: integer_to_string

    implicit none

    private
    public :: test_55

contains

    subroutine test_55(error_cnt)

    !! 55th unit test.

    integer,intent(out) :: error_cnt

    type(json_core) :: json
    type(json_value),pointer :: p
    type(json_file) :: f
    character(kind=CK, len=10) :: str
    integer(IK) :: i, j, icase !! counter
    logical(LK) :: status_ok !! status flag for hash table operations
    integer(IK) :: var_type !! type of value retrieved from JSON
    integer(IK) :: ival !! value retrieved from JSON
    real(RK) :: t_start, t_end  !! timers for performance measurement

    error_cnt = 0

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 55'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    call json%initialize(trailing_spaces_significant = .false., &
                         case_sensitive_keys = .false.)

    ! note that case_sensitive_keys = .false. makes the non-hash lookups
    ! much slower, since it does case-insensitive string comparison for each key.

    ! generate a JSON structure with 1000 members
    call json%create_object(p, CK_'root')
    do i = 1, 1000
        call integer_to_string(i,'(I5)',str)
        call json%add(p, str, i)  ! key is a string of the integer, value is the integer
    end do
    call f%initialize(json) ! initialize the JSON file with the JSON core
    call f%add(p) ! add it to a JSON file

    ! create a hash table:
    call f%initialize_hash(status_ok)
    if (.not. status_ok) then
        write(error_unit,'(A)') 'Error creating hash table'
        error_cnt = error_cnt + 1
        return
    end if

    ! first time: with hash
    ! second time: without hash
    do icase = 1, 2

        ! retrieve some values using the hash table
        call cpu_time(t_start)
        do i = 1, 10000
            ! compute a random integer between 1 and 1000 to use as a key for retrieval
            j = mod(i * 12345_IK, 1000_IK) + 1_IK
            call integer_to_string(j,'(I5)',str)
            call f%get(str, ival, status_ok)
            if (.not. status_ok) then
                write(error_unit,'(A)') 'Error retrieving value from hash table for key: '// trim(str)
                error_cnt = error_cnt + 1
            else
                if (ival /= j) then
                    write(error_unit,'(A)') 'Incorrect value retrieved from hash table for key: '// trim(str)
                    error_cnt = error_cnt + 1
                end if
            end if
        end do
        call cpu_time(t_end)

        if (icase == 1) then
            write(output_unit,'(A, F6.3, A)') 'Time taken for 10,000 hash retrievals:        ', t_end - t_start, ' seconds'
            call f%destroy_hash()
        else
            write(output_unit,'(A, F6.3, A)') 'Time taken for 10,000 non-hash retrievals:    ', t_end - t_start, ' seconds'
        end if

    end do
    write(output_unit, '(A)') ''

    ! clean up:
    call f%destroy()

    end subroutine test_55

end module jf_test_55_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_55

    use jf_test_55_mod , only: test_55

    implicit none
    integer :: n_errors

    call test_55(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_55
#endif
!*****************************************************************************************

