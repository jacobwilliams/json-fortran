!*****************************************************************************************
!>
! Module for the second unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_2_mod

    use json_kinds
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

    type(json_value),pointer :: p, inp, traj, p_tmp, p_integer_array, p_clone

    integer :: iunit
    character(kind=CK,len=:),allocatable :: name
    integer :: ival,ival_clone
    logical :: found

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

    !test the deep copy routine:

    write(error_unit,'(A)') 'json_clone test'
    call json_clone(p,p_clone)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '============='
    write(error_unit,'(A)') ' p_clone'
    write(error_unit,'(A)') '============='
    call json_print(p_clone,error_unit)
    write(error_unit,'(A)') '============='
    write(error_unit,'(A)') ''

    if (.not. associated(p)) write(error_unit,'(A)') 'ERROR: p has become unassociated'
    if (.not. associated(p_clone)) write(error_unit,'(A)') 'ERROR: p_clone is not associated'

    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !now, change one and verify that they are independent:
        call json_update(p_clone,'inputs.integer_scalar',100,found)
        if (json_failed()) write(error_unit,'(A)') 'json_update Error for p_clone'
        call json_get(p,'inputs.integer_scalar',ival)
        if (json_failed()) write(error_unit,'(A)') 'json_get Error for p'
        call json_get(p_clone,'inputs.integer_scalar',ival_clone)
        if (json_failed()) write(error_unit,'(A)') 'json_get Error for p_clone'
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (ival==1 .and. ival_clone==100) then
                write(error_unit,'(A)') 'json_clone ... passed'
            else
                write(error_unit,'(A)') 'Error: ival /= ival_clone'
                error_cnt = error_cnt + 1
            end if
        end if
    end if

    !test some of the pointer routines:
    write(error_unit,'(A)') 'Pointer routine tests'
    call json_get(p,'inputs.integer_array',p_integer_array)
    if (json_failed()) then
        call json_print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        !get parent test:
        call json_get_parent(p_integer_array,p_tmp)  !should be "inputs"
        call json_info(p_tmp,name=name)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='inputs') then
                write(error_unit,'(A)') 'json_get_parent ... passed'
            else
                write(error_unit,'(A)') 'Error: parent should be "inputs", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get next test:
        call json_get_next(p_integer_array,p_tmp)  !should be "names"
        call json_info(p_tmp,name=name)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='names') then
                write(error_unit,'(A)') 'json_get_next ... passed'
            else
                write(error_unit,'(A)') 'Error: next should be "names", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get previous test:
        call json_get_previous(p_integer_array,p_tmp)  !should be "integer_scalar"
        call json_info(p_tmp,name=name)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='integer_scalar') then
                write(error_unit,'(A)') 'json_get_previous ... passed'
            else
                write(error_unit,'(A)') 'Error: next should be "integer_scalar", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get tail test:
        call json_get_tail(p_integer_array,p_tmp)  !should be 99, the last element in the array
        call json_get(p_tmp,ival)
        if (json_failed()) then
            call json_print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (ival==99) then
                write(error_unit,'(A)') 'json_get_tail ... passed'
            else
                write(error_unit,'(A,1X,I5)') 'Error: tail value should be 99, is actually: ',ival
                error_cnt = error_cnt + 1
            end if
        end if

    end if

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
