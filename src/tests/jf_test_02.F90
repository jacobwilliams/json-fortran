!*****************************************************************************************
!>
!  Module for the second unit test.
!
!# HISTORY
!  * Izaak Beekman : 2/18/2015 : Created (refactoried original json_example.f90 file)

module jf_test_2_mod

    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

    implicit none

    private
    public :: test_2

    character(len=*),parameter :: dir = '../files/'    !! working directory
    character(len=*),parameter :: filename2 = 'test2.json'

contains

    subroutine test_2(error_cnt)

    !! Populate a JSON structure and write it to a file.

    implicit none

    integer,intent(out) :: error_cnt

    type(json_value),pointer :: p, inp, traj, tmp1, tmp2, p_tmp, p_integer_array, p_clone
    type(json_core) :: json  !! factory for manipulating `json_value` pointers

    integer(IK) :: iunit
    character(kind=json_CK,len=:),allocatable :: name
    integer(IK) :: ival,ival_clone
    logical(LK) :: found
    logical(LK) :: is_valid
    character(kind=json_CK,len=:),allocatable :: error_msg

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') '   EXAMPLE 2'
    write(error_unit,'(A)') '================================='
    write(error_unit,'(A)') ''

    error_cnt = 0
    call json%initialize()
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    ! first we test an incorrect usage case:
    ! [trying to print a null json_value pointer]
    nullify(p)
    call json%print(p,int(error_unit,IK))
    if (.not. json%failed()) then
        write(error_unit,'(A)') 'Error: printing a null pointer should have raised an exception.'
        error_cnt = error_cnt + 1
    end if
    call json%initialize()  ! clears exceptions

    !root:
    call json%create_object(p,dir//filename2)    ! create the value and associate the pointer
                                                 ! add the file name as the name of the overall structure
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'initialize the structure...'

    !config structure:
    call json%create_object(inp,'inputs')   !an object
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(p, inp)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !test get_parent:
    call json%get_parent(inp,tmp1)  !will be root
    call json%get_parent(tmp1,tmp2) !has no parent -> null()

    !trajectory structure:
    call json%create_array(traj,'trajectory')    !an array
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(p, traj)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'adding some data to structure...'

    !add some variables:

    !input variables:
    call json%add(inp, 't0', 0.1_wp)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'tf', 1.1_wp)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'x0', 9999.000_wp)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'integer_scalar', 1_IK)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'integer_array', [2_IK,4_IK,99_IK])
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'names', ['aaa','bbb','ccc'])
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'logical_scalar', .true.)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'logical_vector', [.true., .false., .true.])
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'null_variable')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'special chars', '\ /')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'special chars in key \ /', '\ /')
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    call json%add(inp, 'bspace', achar(8,kind=json_CK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'horizontal_tab', achar(9,kind=json_CK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'newline', achar(10,kind=json_CK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'formfeed', achar(12,kind=json_CK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(inp, 'carriage_return', achar(13,kind=json_CK))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    nullify(inp)

    !trajectory variables:
    call add_variables_to_input(json, traj, 'Rx', 'km', 'J2000', 'EARTH', [1.0_wp, 2.0_wp, 3.0_wp], error_cnt )
    call add_variables_to_input(json, traj, 'Ry', 'km', 'J2000', 'EARTH', [10.0_wp, 20.0_wp, 30.0_wp], error_cnt )
    call add_variables_to_input(json, traj, 'Rz', 'km', 'J2000', 'EARTH', [100.0_wp, 200.0_wp, 300.0_wp], error_cnt )
    call add_variables_to_input(json, traj, 'Vx', 'km/s', 'J2000', 'EARTH', [1.0e-3_wp, 2.0e-3_wp, 3.0e-3_wp], error_cnt )
    call add_variables_to_input(json, traj, 'Vy', 'km/s', 'J2000', 'EARTH', [2.0e-3_wp, 20.0e-3_wp, 3.0e-3_wp], error_cnt )
    call add_variables_to_input(json, traj, 'Vz', 'km/s', 'J2000', 'EARTH', [3.0e-3_wp, 30.0e-3_wp, 40.0e-3_wp], error_cnt )
    nullify(traj)

    !validate it:
    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'validating...'
    call json%validate(p,is_valid,error_msg)
    if (.not. is_valid) then
        write(error_unit,'(A)') 'Error: p is not a valid JSON linked list: '//error_msg
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') 'writing file '//trim(dir//filename2)//'...'

    open(newunit=iunit, file=dir//filename2, status='REPLACE')
    call json%print(p,iunit)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    close(iunit)

    !test the deep copy routine:

    write(error_unit,'(A)') 'json_clone test'
    call json%clone(p,p_clone)

    write(error_unit,'(A)') ''
    write(error_unit,'(A)') '============='
    write(error_unit,'(A)') ' p_clone'
    write(error_unit,'(A)') '============='
    call json%print(p_clone,int(error_unit,IK))
    write(error_unit,'(A)') '============='
    write(error_unit,'(A)') ''

    if (.not. associated(p)) write(error_unit,'(A)') 'ERROR: p has become unassociated'
    if (.not. associated(p_clone)) write(error_unit,'(A)') 'ERROR: p_clone is not associated'

    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else
        !now, change one and verify that they are independent:
        call json%update(p_clone,'inputs.integer_scalar',100_IK,found)
        if (json%failed()) write(error_unit,'(A)') 'json%update Error for p_clone'
        call json%get(p,'inputs.integer_scalar',ival)
        if (json%failed()) write(error_unit,'(A)') 'json%get Error for p'
        call json%get(p_clone,'inputs.integer_scalar',ival_clone)
        if (json%failed()) write(error_unit,'(A)') 'json%get Error for p_clone'
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (ival==1_IK .and. ival_clone==100_IK) then
                write(error_unit,'(A)') 'json%clone ... passed'
            else
                write(error_unit,'(A)') 'Error: ival /= ival_clone'
                error_cnt = error_cnt + 1
            end if
        end if
    end if

    !test some of the pointer routines:
    write(error_unit,'(A)') 'Pointer routine tests'
    call json%get(p,'inputs.integer_array',p_integer_array)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    else

        !get parent test:
        call json%get_parent(p_integer_array,p_tmp)  !should be "inputs"
        call json%info(p_tmp,name=name)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='inputs') then
                write(error_unit,'(A)') 'json%get_parent ... passed'
            else
                write(error_unit,'(A)') 'Error: parent should be "inputs", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get next test:
        call json%get_next(p_integer_array,p_tmp)  !should be "names"
        call json%info(p_tmp,name=name)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='names') then
                write(error_unit,'(A)') 'json%get_next ... passed'
            else
                write(error_unit,'(A)') 'Error: next should be "names", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get previous test:
        call json%get_previous(p_integer_array,p_tmp)  !should be "integer_scalar"
        call json%info(p_tmp,name=name)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (name=='integer_scalar') then
                write(error_unit,'(A)') 'json%get_previous ... passed'
            else
                write(error_unit,'(A)') 'Error: next should be "integer_scalar", is actually: '//trim(name)
                error_cnt = error_cnt + 1
            end if
        end if

        !get tail test:
        call json%get_tail(p_integer_array,p_tmp)  !should be 99, the last element in the array
        call json%get(p_tmp,ival)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        else
            if (ival==99) then
                write(error_unit,'(A)') 'json%get_tail ... passed'
            else
                write(error_unit,'(A,1X,I5)') 'Error: tail value should be 99, is actually: ',ival
                error_cnt = error_cnt + 1
            end if
        end if

    end if

    !cleanup:
    call json%destroy(p)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%destroy(p_clone)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    write(error_unit,'(A)') ''

    end subroutine test_2

    subroutine add_variables_to_input(json, me, variable, units, frame, center, rdata, error_cnt)
    !Used by test_2.

    implicit none

    type(json_core),intent(inout) :: json
    type(json_value),pointer :: me
    character(len=*),intent(in) :: variable, units, frame, center
    real(wp),dimension(:),intent(in) :: rdata
    integer, intent(inout) :: error_cnt

    type(json_value),pointer :: var        !a variable in the trajectory:

    !initialize:
    nullify(var)

    !create the object before data can be added:
    call json%create_object(var,'')    !name does not matter
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !variable info:
    call json%add(var, 'VARIABLE',trim(variable))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(var, 'UNITS', trim(units))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(var, 'FRAME', trim(frame))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if
    call json%add(var, 'CENTER', trim(center))
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !trajectory [vector of reals]:
    call json%add(var, 'DATA', rdata)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !add this variable to trajectory structure:
    call json%add(me, var)
    if (json%failed()) then
        call json%print_error_message(error_unit)
        error_cnt = error_cnt + 1
    end if

    !cleanup:
    nullify(var)

    end subroutine add_variables_to_input

end module jf_test_2_mod
!*****************************************************************************************

!*****************************************************************************************
#ifndef INTEGRATED_TESTS
program jf_test_2

    !! Second unit test.

    use jf_test_2_mod , only: test_2
    implicit none
    integer :: n_errors
    call test_2(n_errors)
    if (n_errors /= 0) stop 1

end program jf_test_2
#endif
!*****************************************************************************************
