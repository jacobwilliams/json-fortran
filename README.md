json-fortran
============

A Fortran 2003/2008 JSON API

Brief Description
---------------

A mostly-complete API for reading and writing JSON files, written in modern Fortran.  The code requires a Fortran compiler that supports various Fortran 2003 and Fortran 2008 features such as: allocatable strings, associate, newunit, generic, class, and abstract interface.  I am using the Intel Fortran compiler 13.1.0 on Linux (the Mac and PC versions should also work fine).  It does not currently compile with the gnu gfortran compiler.

Reading a JSON file
---------------

Reading a JSON file and getting data from it is fairly straightforward.  Here is an exmaple.  See the json_example.f90 file for more examples.

    program example1

        use json_module

        type(json_file) :: json
        logical :: found
        integer :: i,j,k

        ! initialize the module
        call json_initialize()

        ! read the file
        call json%load_file(filename = 'test1.json')

        ! print the file to the console
        call json%print_file()

        ! extract data from the parsed value
        ! [found can be used to check if the data was really there]
        call json%get('version.major', i, found)
        call json%get('version.minor', j, found)
        call json%get('data(1).number', k, found)

        ! clean up
        call json%destroy()

    end program example1


Writing a JSON file
---------------

Writing a json file is slightly more complicated and involves the use of pointers.  See the json_example.f90 file for more examples.

    program example2

        use json_module

        type(json_value),pointer	:: p, inp
        logical :: found
        integer :: iunit

        ! initialize the module
        call json_initialize()

        ! initialize the structure:
        call json_value_create(p)
        call to_object(p,'test2.json')

        ! add an "inputs" object to the structure:
        call json_value_create(inp)
        call to_object(inp,'inputs')
        call json_value_add(p, inp) !add it to the root
    
        ! add some data to inputs:
        call json_value_add(inp, 't0', 0.1_wp)
        call json_value_add(inp, 'tf', 1.1_wp)
        call json_value_add(inp, 'x0', 9999.0000e0)
        call json_value_add(inp, 'integer_scalar',)
        call json_value_add(inp, 'integer_array', [2,4,99])
        call json_value_add(inp, 'names', ['aaa','bbb','ccc'])
        call json_value_add(inp, 'logical_scalar', .true.)
        call json_value_add(inp, 'logical_vector', [.true., .false., .true.])
        nullify(inp)  !don't need this anymore
      	
        ! write the file:
        open(newunit=iunit, file='test2.json', status='REPLACE')
        call json_print(p,iunit)
        close(iunit)

        !cleanup:
        call json_destroy(p)
    
    end program example2

Other Comments
---------------

This code is a fork and extensive upgrade of the FSON code that can be found at: <https://github.com/josephalevin/fson>.  It includes many features that the original code did not have, and fixes many of that code's bugs.

More About JSON
------------
For more information about JSON, see: <http://www.json.org/> 
