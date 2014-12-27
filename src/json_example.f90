!*******************************************************************************************************
    program json_test
!*******************************************************************************************************
!****u* JSON/json_test
!
!  NAME
!    json_test
!
!  DESCRIPTION
!    Unit tests for the json_module.
!
!  USES
!    json_module
!
!  HISTORY
!    Jacob Williams : 2/8/2014 : Created
!
!  COPYRIGHT
!
!    JSON-FORTRAN: A Fortran 2003/2008 JSON API
!    https://github.com/jacobwilliams/json-fortran
!
!    Copyright (c) 2014, Jacob Williams
!    All rights reserved.
!
!    Redistribution and use in source and binary forms, with or without modification,
!    are permitted provided that the following conditions are met:
!
!    * Redistributions of source code must retain the above copyright notice, this
!      list of conditions and the following disclaimer.
!
!    * Redistributions in binary form must reproduce the above copyright notice, this
!      list of conditions and the following disclaimer in the documentation and/or
!      other materials provided with the distribution.
!
!    * The names of its contributors may not be used to endorse or promote products 
!      derived from this software without specific prior written permission.
!
!    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
!    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
!    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!*******************************************************************************************************
    use,intrinsic :: iso_fortran_env, only: wp => real64    !double precision reals  

    use json_module
    
    implicit none
    
    character(len=*),parameter :: dir = '../files/'               !working directory
    
    character(len=*),parameter :: filename1 = 'test1.json'        !filenames
    character(len=*),parameter :: filename2 = 'test2.json'        !
    character(len=*),parameter :: filename4 = 'test4.json'        !
    character(len=*),parameter :: filename5 = 'test5.json'        !
    
    !initialize the module:
    call json_initialize()

    !run the tests:
    call test_1()
    call test_2()
    call test_3()
    call test_4()
    call test_5()
    
    call test_6()  !these are attempting to read invalid json files

    !call memory_leak_test()    
    
    contains
!*******************************************************************************************************


!**************************************************************
    subroutine test_6()
!**************************************************************
!
!    This example tries to read an invalid json file.
!
!**************************************************************
    implicit none

    type(json_file) :: json 
    integer :: i  
    
    character(len=*),dimension(2),parameter :: files = ['invalid.json ',&
                                                        'invalid2.json']

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 6 : invalid JSON files'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    do i=1,2
    
        ! parse the json file:
        write(*,'(A)') ''
        write(*,'(A)') 'load file: '//trim(files(i))
        write(*,'(A)') ''
        call json%load_file(filename = dir//trim(files(i)))
        if (json_failed()) then
            call print_error_message()
        end if
        ! clean up
        call json%destroy()

    end do
    
!**************************************************************
    end subroutine test_6
!**************************************************************

!**************************************************************
    subroutine test_5()
!**************************************************************
!
!    Github issue example: https://github.com/josephalevin/fson/issues/12
!
!    Read an existing file and extract some variables.
!
!**************************************************************
    implicit none

    integer :: vv
    integer,dimension(:),allocatable :: vvv
    real(wp) :: d
    type(json_file) :: json   
    logical :: found

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 5'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    ! parse the json file:
    write(*,'(A)') 'load file...'
    call json%load_file(filename = dir//filename5)
    if (json_failed()) then

        call print_error_message()

    else

        ! print the parsed data to the console:
        write(*,'(A)') 'print file...'
        call json%print_file()

        ! extract data from the parsed value:
        write(*,'(A)') ''
        write(*,'(A)') 'extract data...'

        write(*,'(A)') '--------------------------'
        call json%get('Correl.ID2', vv, found)
        if (found) write(*,'(A,I5)') 'vv = ',vv

        call json%get('Correl.ID1', vvv, found)
        if (found) write(*,'(A,*(I5,1X))') 'vvv= ',vvv

        call json%get('Prior[3].mode', d, found)
        if (found) write(*,'(A,E30.16)') 'd  = ',d

        write(*,'(A)') ''

    end if

    ! clean up
    call json%destroy()

!**************************************************************
    end subroutine test_5
!**************************************************************

!**************************************************************
   subroutine memory_leak_test()
!**************************************************************
!
!    This is to test for memory leaks.
!        --Monitor memory usage using "top"
!        --This routine contains an infinite loop.
!
!**************************************************************
    implicit none

    integer :: i

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   MEMORY LEAK TEST'
    write(*,'(A)') '================================='
    write(*,'(A)') ''
    
    i = 0

    do

        i=i+1
        write(*,'(A,1X,I5)') '***********************', i

        call test_4()

    end do

!**************************************************************
   end subroutine memory_leak_test
!**************************************************************

!**************************************************************
    subroutine test_4()
!**************************************************************
!
!    Populate a JSON structure, write it to a file,
!        then read it.  
!
!    Also tests the json_value_to_string routine to write
!     the file to a character string.
!
!**************************************************************
    implicit none

    type(json_value),pointer    :: p,inp
    type(json_file) :: json

    integer :: i
    character(len=10) :: istr
    character(len=:),allocatable :: string

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 4'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    write(*,'(A)') ''
    write(*,'(A)') 'creating structure'

    call json_value_create(p)            !create the value and associate the pointer
    call to_object(p,dir//filename4)     !add the file name as the name of the overall structure

    !config structure:
    call json_value_create(inp)          !an object
    call to_object(inp,'INPUTS')
    !add just integers:
    do i=1,100
        write(istr,fmt='(I10)') i
        istr = adjustl(istr)
        call json_value_add(inp, 'x'//trim(istr),i)
    end do
    call json_value_add(p, inp)
    nullify(inp)

    write(*,'(A)') ''
    write(*,'(A)') 'write to file'

    !write the file:
    call json_print(p,trim(dir//filename4))

    write(*,'(A)') ''
    write(*,'(A)') 'write to string'
    write(*,'(A)') ''
    !write it to a string, and print to console:
    call json_print_to_string(p, string)
    write(*,'(A)') string
    deallocate(string)  !cleanup
    
    !cleanup:
    call json_destroy(p)

    write(*,'(A)') ''
    write(*,'(A)') 'read file'

    call json%load_file(filename = dir//filename4)
    if (json_failed()) call print_error_message()

    write(*,'(A)') ''
    write(*,'(A)') 'cleanup'
    call json%destroy()

!**************************************************************
    end subroutine test_4
!**************************************************************

!**************************************************************
    subroutine test_2()
!**************************************************************
!
!    Populate a JSON structure and write it to a file.
!
!**************************************************************
    implicit none

    type(json_value),pointer    :: p, inp, traj

    integer :: iunit

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 2'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    !root:
    call json_value_create(p)           ! create the value and associate the pointer
    call to_object(p,dir//filename2)    ! add the file name as the name of the overall structure

    write(*,'(A)') ''
    write(*,'(A)') 'initialize the structure...'

    !config structure:
    call json_value_create(inp)             !an object
    call to_object(inp,'inputs')
    call json_value_add(p, inp)

    !trajectory structure:
    call json_value_create(traj)            !an array
    call to_array(traj,'trajectory')
    call json_value_add(p, traj)

    write(*,'(A)') ''
    write(*,'(A)') 'adding some data to structure...'

    !add some variables:

    !input variables:
    call json_value_add(inp, 't0', 0.1_wp)
    call json_value_add(inp, 'tf', 1.1_wp)
    call json_value_add(inp, 'x0', 9999.000_wp)
    call json_value_add(inp, 'integer_scalar', 1)
    call json_value_add(inp, 'integer_array', [2,4,99])
    call json_value_add(inp, 'names', ['aaa','bbb','ccc'])
    call json_value_add(inp, 'logical_scalar', .true.)
    call json_value_add(inp, 'logical_vector', [.true., .false., .true.])
    nullify(inp)

    !trajectory variables:
    call add_variables_to_input(traj, 'Rx', 'km', 'J2000', 'EARTH', [1.0_wp, 2.0_wp, 3.0_wp] )
    call add_variables_to_input(traj, 'Ry', 'km', 'J2000', 'EARTH', [10.0_wp, 20.0_wp, 30.0_wp] )
    call add_variables_to_input(traj, 'Rz', 'km', 'J2000', 'EARTH', [100.0_wp, 200.0d0, 300.0_wp] )
    call add_variables_to_input(traj, 'Vx', 'km/s', 'J2000', 'EARTH', [1.0e-3_wp, 2.0e-3_wp, 3.0e-3_wp] )
    call add_variables_to_input(traj, 'Vy', 'km/s', 'J2000', 'EARTH', [2.0e-3_wp, 20.0e-3_wp, 3.0e-3_wp] )
    call add_variables_to_input(traj, 'Vz', 'km/s', 'J2000', 'EARTH', [3.0e-3_wp, 30.0e-3_wp, 40.0e-3_wp] )
    nullify(traj)

    write(*,'(A)') ''
    write(*,'(A)') 'writing file '//trim(dir//filename2)//'...'

    open(newunit=iunit, file=dir//filename2, status='REPLACE')
    call json_print(p,iunit)
    close(iunit)

    !cleanup:
    call json_destroy(p)

    write(*,'(A)') ''

!**************************************************************
    end subroutine test_2
!**************************************************************

!**************************************************************
    subroutine add_variables_to_input(me, variable, units, frame, center, rdata)
!**************************************************************
!    Used by test_2.
!**************************************************************

    implicit none

    type(json_value),pointer :: me
    character(len=*),intent(in) :: variable, units, frame, center
    real(wp),dimension(:),intent(in) :: rdata

    type(json_value),pointer :: var        !a variable in the trajectory:

    !initialize:
    nullify(var)

    !create the object before data can be added:
    call json_value_create(var)
    call to_object(var,'')    !name does not matter

    !variable info:
    call json_value_add(var, 'VARIABLE',trim(variable))
    call json_value_add(var, 'UNITS', trim(units))
    call json_value_add(var, 'FRAME', trim(frame))
    call json_value_add(var, 'CENTER', trim(center))

    !trajectory [vector of reals]:
    call json_value_add(var, 'DATA', rdata)

    !add this variable to trajectory structure:
    call json_value_add(me, var)

    !cleanup:
    nullify(var)

!**************************************************************
    end subroutine add_variables_to_input
!**************************************************************

!**************************************************************
    subroutine test_3()
!**************************************************************
!
!    Read the file generated in test_2, and extract
!        some data from it.
!
!**************************************************************
    implicit none

    integer :: ival
    character(len=:),allocatable :: cval
    real(wp) :: rval
    type(json_file) :: json    !the JSON structure read from the file:
    integer :: i
    character(len=10) :: str
    real(wp),dimension(:),allocatable :: rvec

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 3'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    ! parse the json file:
    write(*,'(A)') ''
    write(*,'(A)') 'parsing file: '//dir//filename2

    call json%load_file(filename = dir//filename2)

    if (json_failed()) then    !if there was an error reading the file

        call print_error_message()

    else

        write(*,'(A)') ''
        write(*,'(A)') 'reading data from file...'
        !get scalars:
        write(*,'(A)') ''
        call json%get('inputs.integer_scalar', ival)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A,1X,I5)') 'inputs.integer_scalar = ',ival
        end if
        !get one element from a vector:
        write(*,'(A)') ''
        call json%get('trajectory(1).DATA(2)', rval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A,1X,F30.16)') 'trajectory(1).DATA(2) = ',rval
        end if
        !get vectors:
        do i=1,4

            write(str,fmt='(I10)') i
            str = adjustl(str)

            write(*,'(A)') ''
            call json%get('trajectory('//trim(str)//').VARIABLE', cval)
            if (json_failed()) then

                call print_error_message()

            else

                write(*,'(A)') 'trajectory('//trim(str)//').VARIABLE = '//trim(cval)

                !...get the vector using the callback method:
                call json%get('trajectory('//trim(str)//').DATA', rvec)
                if (json_failed()) then
                    call print_error_message()
                else
                    write(*,'(A,1X,*(F30.16,1X))') 'trajectory('//trim(str)//').DATA = ',rvec
                end if

            end if

        end do

    end if

    ! clean up
    write(*,'(A)') ''
    write(*,'(A)') 'destroy...'
    call json%destroy()

!**************************************************************
    end subroutine test_3
!**************************************************************

!**************************************************************
    subroutine test_1()
!**************************************************************
!
!    Read a sample JSON file and retrieve some data from it
!
!**************************************************************
    implicit none

    type(json_file) :: json    !the JSON structure read from the file:
    integer :: ival
    character(len=:),allocatable :: cval
    real(wp) :: rval
    logical :: found
    type(json_value),pointer :: p

    write(*,'(A)') ''
    write(*,'(A)') '================================='
    write(*,'(A)') '   EXAMPLE 1'
    write(*,'(A)') '================================='
    write(*,'(A)') ''

    ! parse the json file:
    write(*,'(A)') ''
    write(*,'(A)') 'parsing file...'

    call json%load_file(filename = dir//filename1)

    if (json_failed()) then    !if there was an error reading the file

        call print_error_message()

    else

        ! print the parsed data to the console
        write(*,'(A)') ''
        write(*,'(A)') 'printing the file...'
        call json%print_file()

        ! extract data from the parsed value
        write(*,'(A)') ''
        write(*,'(A)') 'get some data from the file...'

        write(*,'(A)') ''
        call json%get('version.svn', ival)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A,I5)') 'version.svn = ',ival
        end if

        write(*,'(A)') ''
        call json%get('data(1).array(2)', cval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'data(1).array(2) = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('files(1)', cval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'files(1) = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('files(2)', cval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'files(2) = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('files(3)', cval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'files(3) = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('data(2).real', rval)
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A,E30.16)') 'data(2).real = ',rval
        end if

        write(*,'(A)') ''
        call json%get('files[4]', cval)        !has hex characters
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'files[4] = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('files[5]', cval)        !string with spaces and no escape characters
        if (json_failed()) then
            call print_error_message()
        else
            write(*,'(A)') 'files[5] = '//trim(cval)
        end if

        !
        ! Test of values that aren't there:
        ! Note: when using the "found" output, the exceptions are cleared automatically.
        !
        
        write(*,'(A)') ''
        call json%get('files[10]', cval, found)        !value that isn't there
        if (.not. found) then
            write(*,'(A)') 'files[10] not in file.'
        else
            write(*,'(1x,A)') 'files[10] = '//trim(cval)
        end if

        write(*,'(A)') ''
        call json%get('version.blah', ival, found)        !value that isn't there
        if (.not. found) then
            write(*,'(A)') 'version.blah not in file.'
        else
            write(*,'(A)') 'version.blah = ',ival
        end if
        
        write(*,'(A)') ''
        write(*,'(A)') ' Test removing data from the json structure:'
        
        call json%get('files', p)           !in the middle of a list
        call json_remove(p)
        
        call json%get('data(1).array', p)   !at the end of a list
        call json_remove(p)
        
        call json%get('data(2).number', p)  !at the beginning of a list
        call json_remove(p)
        
        write(*,'(A)') ''
        write(*,'(A)') 'printing the modified structure...'
        call json%print_file()       

        write(*,'(A)') ''
        write(*,'(A)') ' Test replacing data from the json structure:'
        
        call json%get('data(1)', p)
        call json_update(p,'name','Cuthbert',found)

        !call json%get('data(2)', p)
        !call json_update(p,'real',[1.0_wp, 2.0_wp, 3.0_wp],found)   !don't have one like this yet...
        
        write(*,'(A)') ''
        write(*,'(A)') 'printing the modified structure...'
        call json%print_file()       
        
    end if

    ! clean up
    write(*,'(A)') ''
    write(*,'(A)') 'destroy...'
    call json%destroy()

!**************************************************************
    end subroutine test_1
!**************************************************************

!**************************************************************
    subroutine print_error_message()
!**************************************************************
!    Print the error message and clear all exceptions
!**************************************************************
    implicit none

    character(len=:),allocatable :: error_msg
    logical :: status_ok

    !get error message:
    call json_check_for_errors(status_ok, error_msg)

    !print it if there is one:
    if (.not. status_ok) then
        write(*,'(A)') error_msg
        deallocate(error_msg)
        call json_clear_exceptions()
    end if

!**************************************************************
    end subroutine print_error_message
!**************************************************************
    
!*******************************************************************************************************
    end program json_test
!*******************************************************************************************************
