!*****************************************************************************************
!>
! Module for the second unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_2_mod

    use json_module
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit, wp => real64

    implicit none

    character(len=*),parameter :: dir = '../files/'    !! working directory
    character(len=*),parameter :: filename2 = 'test2.json'

contains

    subroutine test_2(error_cnt)

    !! Populate a JSON structure and write it to a file.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer    :: p, inp, traj

    integer :: iunit

    error_cnt = 0
    call json_initialize()
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 2'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    !root:
    call json_create_object(p,dir//filename2)    ! create the value and associate the pointer
                                                 ! add the file name as the name of the overall structure
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'initialize the structure...'

    !config structure:
    call json_create_object(inp,'inputs')   !an object
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(p, inp)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !trajectory structure:
    call json_create_array(traj,'trajectory')    !an array
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(p, traj)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'adding some data to structure...'

    !add some variables:

    !input variables:
    call json_add(inp, 't0', 0.1_wp)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'tf', 1.1_wp)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'x0', 9999.000_wp)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'integer_scalar', 1)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'integer_array', [2,4,99])
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'names', ['aaa','bbb','ccc'])
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'logical_scalar', .true.)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(inp, 'logical_vector', [.true., .false., .true.])
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    nullify(inp)

    !trajectory variables:
    call add_variables_to_input(traj, 'Rx', 'km', 'J2000', 'EARTH', [1.0_wp, 2.0_wp, 3.0_wp], error_cnt )
    call add_variables_to_input(traj, 'Ry', 'km', 'J2000', 'EARTH', [10.0_wp, 20.0_wp, 30.0_wp], error_cnt )
    call add_variables_to_input(traj, 'Rz', 'km', 'J2000', 'EARTH', [100.0_wp, 200.0d0, 300.0_wp], error_cnt )
    call add_variables_to_input(traj, 'Vx', 'km/s', 'J2000', 'EARTH', [1.0e-3_wp, 2.0e-3_wp, 3.0e-3_wp], error_cnt )
    call add_variables_to_input(traj, 'Vy', 'km/s', 'J2000', 'EARTH', [2.0e-3_wp, 20.0e-3_wp, 3.0e-3_wp], error_cnt )
    call add_variables_to_input(traj, 'Vz', 'km/s', 'J2000', 'EARTH', [3.0e-3_wp, 30.0e-3_wp, 40.0e-3_wp], error_cnt )
    nullify(traj)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'writing file '//trim(dir//filename2)//'...'

    open(newunit=iunit, file=dir//filename2, status='REPLACE')
    call json_print(p,iunit)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    close(iunit)

    !cleanup:
    call json_destroy(p)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''

    end subroutine test_2

    subroutine add_variables_to_input(me, variable, units, frame, center, rdata, error_cnt)
    !Used by test_2.

    implicit none

    type(json_value),pointer :: me
    character(len=*),intent(in) :: variable, units, frame, center
    real(wp),dimension(:),intent(in) :: rdata
    integer, intent(inout) :: error_cnt

    type(json_value),pointer :: var        !a variable in the trajectory:

    !initialize:
    nullify(var)

    !create the object before data can be added:
    call json_create_object(var,'')    !name does not matter
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !variable info:
    call json_add(var, 'VARIABLE',trim(variable))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(var, 'UNITS', trim(units))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(var, 'FRAME', trim(frame))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json_add(var, 'CENTER', trim(center))
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !trajectory [vector of reals]:
    call json_add(var, 'DATA', rdata)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !add this variable to trajectory structure:
    call json_add(me, var)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !cleanup:
    nullify(var)

    end subroutine add_variables_to_input

end module jf_test_2_mod
!*****************************************************************************************

!*****************************************************************************************
program jf_test_2

    !! Second unit test.
    
    use jf_test_2_mod , only: test_2
    implicit none
    integer :: n_errors
    n_errors = 0
    call test_2(n_errors)
    if (n_errors /= 0) stop 1
end program jf_test_2
!*****************************************************************************************
