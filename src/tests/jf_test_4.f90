!*****************************************************************************************
!>
! Module for the forth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_4_mod

    use json_kinds
    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/'         !! working directory
    character(len=*),parameter :: filename4 = 'test4.json'

contains

    subroutine test_4(error_cnt)

    !! Populate a JSON structure, write it to a file,
    !! then read it.
    !!
    !! Also tests the json_value_to_string routine to write
    !! the file to a character string.

    implicit none

    integer,intent(out) :: error_cnt
    type(json_value),pointer :: p,inp
    type(json_file) :: json
    type(json_core) :: core  !! factory for manipulating `json_value` pointers

    integer :: i
    character(kind=CK,len=10) :: istr
    character(kind=CK,len=:),allocatable :: string

    error_cnt = 0
    call core%initialize()
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 4'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'creating structure'

    call core%create_object(p,dir//filename4)     !create the value and associate the pointer
                                                  !add the file name as the name of the overall structure
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !config structure:
    call core%create_object(inp,'INPUTS')    !an object
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    !add just integers:
    do i=1,100
        write(istr,fmt='(I10)') i
        istr = adjustl(istr)
        call core%add(inp, 'x'//trim(istr),i)
        if (core%failed()) then
            call core%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
    end do
    call core%add(p, inp)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(inp)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'write to file'

    !write the file:
    call core%print(p,trim(dir//filename4))
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'write to string'
    write(error_unit,'(A)') ''
    !write it to a string, and print to console:
    call core%print_to_string(p, string)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') string
    deallocate(string)  !cleanup

    !cleanup:
    call core%destroy(p)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'read file'

    call json%load_file(filename = dir//filename4)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'cleanup'
    call json%destroy()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    end subroutine test_4

end module jf_test_4_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_4

    !! Fourth unit test.

    use jf_test_4_mod , only: test_4
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_4(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_4
!*****************************************************************************************
