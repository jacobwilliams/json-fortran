!*******************************************************************************************************
	program json_test
!*******************************************************************************************************
!****h* JSON/json_test
!
!  NAME
!	json_test
!
!  DESCRIPTION
!	Unit tests for the json_module.
!
!  HISTORY
!	Jacob Williams : 2/8/2014 : Created
!
!  LICENSE
!
!	JSON-FORTRAN: A Fortran 2003/2008 JSON API
!	https://github.com/jacobwilliams/json-fortran
!
!	Copyright (c) 2014, Jacob Williams
!	All rights reserved.
!
!	Redistribution and use in source and binary forms, with or without modification,
!	are permitted provided that the following conditions are met:
!
!	* Redistributions of source code must retain the above copyright notice, this
!	  list of conditions and the following disclaimer.
!
!	* Redistributions in binary form must reproduce the above copyright notice, this
!	  list of conditions and the following disclaimer in the documentation and/or
!	  other materials provided with the distribution.
!
!	* The names of its contributors may not be used to endorse or promote products 
!	  derived from this software without specific prior written permission.
!
!	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
!	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
!	ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!*******************************************************************************************************
	use json_module
	
	implicit none
	
	character(len=*),parameter :: dir = '../files/'				!working directory
	
	character(len=*),parameter :: filename1	= 'test1.json'		!filenames
	character(len=*),parameter :: filename2	= 'test2.json'		!
	character(len=*),parameter :: filename4	= 'test4.json'		!
	character(len=*),parameter :: filename5 = 'test5.json'		!
	
	!initialize the module:
	call json_initialize()

	!run the tests:
	call test_1()
	call test_2()
	call test_3()
	call test_4()
	call test_5()

	!call memory_leak_test()	
	
	contains
!*******************************************************************************************************

!**************************************************************
	subroutine test_5()
!**************************************************************
!
!  	Github issue example: https://github.com/josephalevin/fson/issues/12
!
!	Read an existing file and extract some variables.
!
!**************************************************************
	implicit none

	integer :: vv
	integer,dimension(:),allocatable :: vvv
	real(wp) :: d
	type(json_file) :: json   
	logical :: found

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   EXAMPLE 5'
	write(*,*) '================================='
	write(*,*) ''

	! parse the json file:
	write(*,*) 'load file...'
	call json%load_file(filename = dir//filename5)
	if (json_failed()) then

		call print_error_message()

	else

		! print the parsed data to the console:
		write(*,*) 'print file...'
		call json%print_file()

		! extract data from the parsed value:
		write(*,*) ''
		write(*,*) 'extract data...'

		write(*,*) '--------------------------'
		call json%get('Correl.ID2', vv, found)
		if (found) write(*,*) 'vv = ',vv

		call json%get('Correl.ID1', vvv, found)
		if (found) write(*,*) 'vvv= ',vvv

		call json%get('Prior[3].mode', d, found)
		if (found) write(*,*) 'd  = ',d

		write(*,*) ''

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
! 	This is to test for memory leaks.
! 		--Monitor memory usage using "top"
! 		--This routine contains an infinite loop.
!
!**************************************************************
	implicit none

	integer :: i

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   MEMORY LEAK TEST'
	write(*,*) '================================='
	write(*,*) ''

	do

		i=i+1
		write(*,*) '***********************', i

		call test_4()

	end do

!**************************************************************
   end subroutine memory_leak_test
!**************************************************************

!**************************************************************
	subroutine test_4()
!**************************************************************
!
!	Populate a JSON structure, write it to a file,
!		then read it.
!
!**************************************************************
	implicit none

	type(json_value),pointer	:: p,inp
	type(json_file) :: json

	integer :: i
	character(len=10) :: istr
	integer :: iunit

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   EXAMPLE 4'
	write(*,*) '================================='
	write(*,*) ''

	write(*,*) ''
	write(*,*) 'creating structure'

	call json_value_create(p)				! create the value and associate the pointer
	call to_object(p,dir//filename4)		    !add the file name as the name of the overall structure:

	!config structure:
	call json_value_create(inp)			    !an object
	call to_object(inp,'INPUTS')
	!add just integers:
	do i=1,100
		call integer_to_string(i,istr)
		call json_value_add(inp, 'x'//trim(istr),i)
	end do
	call json_value_add(p, inp)
	nullify(inp)

	write(*,*) ''
	write(*,*) 'write file'

	!write the file:
	open(newunit=iunit, file=dir//filename4, status='REPLACE')
	call json_print(p,iunit)
	close(iunit)

	!cleanup:
	call json_destroy(p)

	write(*,*) ''
	write(*,*) 'read file'

	call json%load_file(filename = dir//filename4)
	if (json_failed()) call print_error_message()

	write(*,*) ''
	write(*,*) 'cleanup'
	call json%destroy()

!**************************************************************
	end subroutine test_4
!**************************************************************

!**************************************************************
	subroutine test_2()
!**************************************************************
!
!	Populate a JSON structure and write it to a file.
!
!**************************************************************
	implicit none

	type(json_value),pointer	:: p, inp, traj

	integer :: iunit

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   EXAMPLE 2'
	write(*,*) '================================='
	write(*,*) ''

	!root:
	call json_value_create(p)		! create the value and associate the pointer
	call to_object(p,dir//filename2)	! add the file name as the name of the overall structure

	write(*,*) ''
	write(*,*) 'initialize the structure...'

	!config structure:
	call json_value_create(inp)			!an object
	call to_object(inp,'INPUTS')
	call json_value_add(p, inp)

	!trajectory structure:
	call json_value_create(traj)			!an array
	call to_array(traj,'TRAJECTORY')
	call json_value_add(p, traj)

	write(*,*) ''
	write(*,*) 'adding some data to structure...'

	!add some variables:

	!input variables:
	call json_value_add(inp, 't0', 				0.1_wp)
	call json_value_add(inp, 'tf', 				1.1_wp)
	call json_value_add(inp, 'x0', 				9999.000_wp)
	call json_value_add(inp, 'current_segment', 1)
	call json_value_add(inp, 'segment_groups', 	[2,4,99])
	call json_value_add(inp, 'names', 			['aaa','bbb','ccc'])
	call json_value_add(inp, 'logical_scalar',  .true.)
	call json_value_add(inp, 'logical_vector',  [.true., .false., .true.])
	nullify(inp)

	!trajectory variables:
	call add_variables_to_input(traj, 'Rx', 'km', 	'J2000', 'EARTH', [1.0_wp, 		2.0_wp, 	3.0_wp]		)
	call add_variables_to_input(traj, 'Ry', 'km', 	'J2000', 'EARTH', [10.0_wp, 	20.0_wp, 	30.0_wp]	)
	call add_variables_to_input(traj, 'Rz', 'km', 	'J2000', 'EARTH', [100.0_wp, 	200.0d0, 	300.0_wp]	)
	call add_variables_to_input(traj, 'Vx', 'km/s', 'J2000', 'EARTH', [1.0e-3_wp, 	2.0e-3_wp, 	3.0e-3_wp]	)
	call add_variables_to_input(traj, 'Vy', 'km/s', 'J2000', 'EARTH', [2.0e-3_wp, 	20.0e-3_wp, 3.0e-3_wp]	)
	call add_variables_to_input(traj, 'Vz', 'km/s', 'J2000', 'EARTH', [3.0e-3_wp, 	30.0e-3_wp, 40.0e-3_wp]	)
	nullify(traj)

	write(*,*) ''
	write(*,*) 'writing file '//trim(dir//filename2)//'...'

	open(newunit=iunit, file=dir//filename2, status='REPLACE')
	call json_print(p,iunit)
	close(iunit)

	!cleanup:
	call json_destroy(p)

	write(*,*) ''

!**************************************************************
	end subroutine test_2
!**************************************************************

!**************************************************************
	subroutine add_variables_to_input(me, variable, units, frame, center, rdata)
!**************************************************************
!	Used by test_2.
!**************************************************************

	implicit none

	type(json_value),pointer :: me
	character(len=*),intent(in) :: variable, units, frame, center
	real(wp),dimension(:),intent(in) :: rdata

	type(json_value),pointer :: var		!a variable in the trajectory:
	integer :: i

	!initialize:
	nullify(var)

	!create the object before data can be added:
	call json_value_create(var)
	call to_object(var,'VARIABLE')

	!variable info:
	call json_value_add(var, 'VARIABLE',trim(variable))
	call json_value_add(var, 'UNITS',   trim(units))
	call json_value_add(var, 'FRAME', 	trim(frame))
	call json_value_add(var, 'CENTER', 	trim(center))

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
!	Read the file generated in test_2, and extract
!		some data from it.
!
!**************************************************************
	implicit none

	integer :: ival
	character(len=:),allocatable :: cval
	real(wp) :: rval
	type(json_file) :: json	!the JSON structure read from the file:
	integer :: i
	character(len=10) :: str
	real(wp),dimension(:),allocatable :: rvec

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   EXAMPLE 3'
	write(*,*) '================================='
	write(*,*) ''

	! parse the json file:
	write(*,*) ''
	write(*,*) 'parsing file: '//dir//filename2

	call json%load_file(filename = dir//filename2)

	if (json_failed()) then	!if there was an error reading the file

		call print_error_message()

	else

		write(*,*) ''
		write(*,*) 'reading trajectory data...'
		!get scalars:
		write(*,*) ''
		call json%get('INPUTS.current_segment', ival)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(A,1X,I5)') 'INPUTS.current_segment = ',ival
		end if
		!get one element from a vector:
		write(*,*) ''
		call json%get('TRAJECTORY(1).DATA(2)', rval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(A,1X,F30.16)') 'TRAJECTORY(1).DATA(2) = ',rval
		end if
		!get vectors:
		do i=1,4

			call integer_to_string(i,str)

			write(*,*) ''
			call json%get('TRAJECTORY('//trim(str)//').VARIABLE', cval)
			if (json_failed()) then

				call print_error_message()

			else

				write(*,'(A)') 'TRAJECTORY('//trim(str)//').VARIABLE = '//trim(cval)

				!...get the vector using the callback method:
				call json%get('TRAJECTORY('//trim(str)//').DATA', rvec)
				if (json_failed()) then
					call print_error_message()
				else
					write(*,'(A,1X,*(F30.16,1X))') 'TRAJECTORY('//trim(str)//').DATA = ',rvec
				end if

			end if

		end do

	end if

	! clean up
	write(*,*) ''
	write(*,*) 'destroy...'
	call json%destroy()

!**************************************************************
	end subroutine test_3
!**************************************************************

!**************************************************************
	subroutine test_1()
!**************************************************************
!
!	Read a sample JSON file and retrieve some data from it
!
!**************************************************************
	implicit none

	type(json_file) :: json	!the JSON structure read from the file:
	integer :: ival
	character(len=:),allocatable :: cval
	real(wp) :: rval
	logical :: found

	write(*,*) ''
	write(*,*) '================================='
	write(*,*) '   EXAMPLE 1'
	write(*,*) '================================='
	write(*,*) ''

	! parse the json file:
	write(*,*) ''
	write(*,*) 'parsing file...'

	call json%load_file(filename = dir//filename1)

	if (json_failed()) then	!if there was an error reading the file

		call print_error_message()

	else

		! print the parsed data to the console
		write(*,*) ''
		write(*,*) 'printing the file...'
		call json%print_file()

		! extract data from the parsed value
		write(*,*) ''
		write(*,*) 'get some data from the file...'

		write(*,*) ''
		call json%get('VERSION_VARS.COP_SVN_REVISION', ival)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,*) 'VERSION_VARS.COP_SVN_REVISION = ',ival
		end if

		write(*,*) ''
		call json%get('SEGMENT_VARS(1).array(2)', cval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(1x,A)') 'SEGMENT_VARS(1).array(2) = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('SPICE_VARS(1)', cval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(1x,A)') 'SPICE_VARS(1) = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('SPICE_VARS(2)', cval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(1x,A)') 'SPICE_VARS(2) = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('SPICE_VARS(3)', cval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,*) 'SPICE_VARS(3) = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('SEGMENT_VARS(2).blah2', rval)
		if (json_failed()) then
			call print_error_message()
		else
			write(*,*) 'SEGMENT_VARS(2).blah2 = ',rval
		end if

		write(*,*) ''
		call json%get('SPICE_VARS[4]', cval)		!has hex characters
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(1x,A)') 'SPICE_VARS[4] = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('SPICE_VARS[5]', cval)		!string with spaces and no escape characters
		if (json_failed()) then
			call print_error_message()
		else
			write(*,'(1x,A)') 'SPICE_VARS[5] = '//trim(cval)
		end if

		!
		! Test of values that aren't there:
		! Note: when using the "found" output, the exceptions are cleared automatically.
		!
		
		write(*,*) ''
		call json%get('SPICE_VARS[10]', cval, found)		!value that isn't there
		if (.not. found) then
			write(*,*) 'SPICE_VARS[10] not in file.'
		else
			write(*,'(1x,A)') 'SPICE_VARS[10] = '//trim(cval)
		end if

		write(*,*) ''
		call json%get('VERSION_VARS.blah', ival, found)		!value that isn't there
		if (.not. found) then
			write(*,*) 'VERSION_VARS.blah not in file.'
		else
			write(*,*) 'VERSION_VARS.blah = ',ival
		end if

	end if

	! clean up
	write(*,*) ''
	write(*,*) 'destroy...'
	call json%destroy()

!**************************************************************
	end subroutine test_1
!**************************************************************

!**************************************************************
	subroutine print_error_message()
!**************************************************************
!	Print the error message and clear all exceptions
!**************************************************************
	implicit none

	character(len=:),allocatable :: error_msg
	logical :: status_ok

	!get error message:
	call json_check_for_errors(status_ok, error_msg)

	!print it if there is one:
	if (.not. status_ok) then
		write(*,*) error_msg
		deallocate(error_msg)
		call json_clear_exceptions()
	end if

!**************************************************************
	end subroutine print_error_message
!**************************************************************
	
!*******************************************************************************************************
	end program json_test
!*******************************************************************************************************