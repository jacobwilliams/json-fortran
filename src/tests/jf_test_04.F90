!*****************************************************************************************
!>
! Module for the forth unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_4_mod

    use json_module, wp => json_RK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_4

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

    integer(json_IK) :: i
    character(kind=json_CK,len=10) :: istr
    character(kind=json_CK,len=:),allocatable :: string,name
    logical(json_LK) :: found
    integer(json_IK) :: var_type,n_children

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
    call core%serialize(p, string)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    write(output_unit,'(A)') string
    deallocate(string)  !cleanup

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'json_info_by_path'
    write(error_unit,'(A)') ''
    !get some info:
    call core%info(p,'INPUTS',found,var_type,n_children,name)
    if (found) then
        !test again with CDK path (for unicode wrapper)
        call core%info(p,json_cdk_'INPUTS',found,var_type,n_children,name)
    else
        write(error_unit,'(A)') 'Error getting info on INPUT'
        error_cnt = error_cnt + 1
    end if
    !test without found:
    call core%info(p,'INPUTS',var_type=var_type,n_children=n_children,name=name)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    !get with a variable that we know if not present:
    call core%info(p,'BLAHBLAH',found,var_type,n_children,name)
    if (found) then !should not be found
        write(error_unit,'(A)') 'Error: BLAHBLAH should not be there'
        error_cnt = error_cnt + 1
    end if

    !cleanup:
    call core%destroy(p)
    if (core%failed()) then
        call core%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'read file'

    call json%load(filename = dir//filename4)
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

#ifndef INTEGRATED_TESTS
!*****************************************************************************************
program jf_test_4

    !! Fourth unit test.

    use jf_test_4_mod , only: test_4
    implicit none
    integer :: n_errors
    call test_4(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_4
!*****************************************************************************************
#endif
