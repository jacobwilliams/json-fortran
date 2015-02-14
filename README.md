json-fortran [![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases)
============

A Fortran 2008 JSON API

Status
------
[![GitHub issues](https://img.shields.io/github/issues/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/issues)
[![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](https://waffle.io/jacobwilliams/json-fortran)
[![In Progress](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=In%20Progress&title=In%20Progress)](https://waffle.io/jacobwilliams/json-fortran)

Brief Description
---------------

An easy-to-use API for reading and writing JSON files, written in
modern Fortran.  The source code is a single Fortran module file ([json_module.f90](https://github.com/jacobwilliams/json-fortran/blob/master/src/json_module.f90)).

Download
--------------------

Download the official versioned releases [here](https://github.com/jacobwilliams/json-fortran/releases).  Or, get the latest development code from the master branch [here](https://github.com/jacobwilliams/json-fortran.git).

Building the Library
--------------------

The code requires a Fortran compiler that supports
various Fortran 2003 and Fortran 2008 features such as: allocatable
strings, newunit, generic, class, and abstract interface.
It has been successfully compiled with the [Intel Fortran compiler
13.1.0](https://software.intel.com/en-us/non-commercial-software-development) (and greater) and the recent [4.9 release of the GNU gfortran
compiler](http://gcc.gnu.org/wiki/GFortran/News#GCC4.9).  

Currently, several ways are provided to build the jsonfortran library
(libjsonfortran). 

* A build script, `build.sh` is provided in the project root directory. This script uses [FoBiS](https://github.com/szaghi/FoBiS) to build the json-fortran library and the example program.  Edit the script to use either the Intel compiler or gfortran.  Note that version 1.2.5 of FoBiS (or later) is required.

* A Visual Studio 2010 project is included for building the library (and example program) on Windows with the Intel compiler.  The project has been tested with Visual Studio 2010 and 2013.

* An [SCons](http://www.scons.org) `SConstruct` file.  The library and example program are built by typing `scons` and installed by `scons install` or `sudo scons install`. 

* Additionally, a [CMake](http://www.cmake.org) build
system is provided. This build system has been tested on Mac and Linux
using the Intel Fortran Compiler and gfortran 4.9. It has not been
tested on Windows. This CMake based build provides an install target,
and exports from both the install location and the build location so
that building and using json-fortran in another CMake based project is
trivial. To get started with the CMake based build, set the
environment variable `FC` to point to your Fortran compiler, and
create a build directory. Then `(cmake-gui|ccmake|cmake)
/path/to/json-fortran-root` to configure, `make` to build and `make
install` to optionally install. As long as the project is built with
CMake, other CMake projects can find it and link against it:

```CMake
cmake_minimum_required ( VERSION 2.8 FATAL_ERROR )
enable_language ( Fortran )
project ( jf_test NONE )

find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 3.0.0 REQUIRED )

add_executable ( json_example src/json_example.f90 )
target_include_directories ( json_example BEFORE PUBLIC ${jsonfortran_INCLUDE_DIRS} )
target_link_libraries ( json_example jsonfortran-static )
# or for linking against the dynamic/shared library:
# target_link_libraries ( json_example jsonfortran ) # instead
```

Reading JSON from a file
---------------

Reading a JSON file and getting data from it is fairly
straightforward using the ```json_file``` class.  Here is an example.  See the json_example.f90 file
for more examples.  

```fortran
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

        ! extract data from the file
        ! [found can be used to check if the data was really there]
        call json%get('version.major', i, found)
        call json%get('version.minor', j, found)
        call json%get('data(1).number', k, found)

        ! clean up
        call json%destroy()

    end program example1
```

Reading JSON from a string
---------------
JSON can also be read directly from a character string like so:
```fortran 
    call json%load_from_string('{"name", "Leonidas"}')
```

Modifying variables in a JSON file
---------------

After reading a JSON file, if you want to change the values of some of the variables, you can use the ```update``` method.  For the example above:

```fortran
    ! [found can be used to check if the data was really there]
    call json%update('version.major',9,found)  !change major version to 9
    call json%update('version.minor',0,found)  !change minor version to 0
    call json%update('version.patch',0,found)  !change patch to 0
```

Writing a JSON file
---------------

To print the JSON file (either to a file or the console), the ```print_file``` method can be used.  For the above example:

```fortran
    call json%print_file()         !prints to the console
    call json%print_file(iunit)    !prints to the file connnected to iunit
```

Building a JSON file from scratch
---------------

Constructing a JSON file element by element is slightly more complicated and involves the use
of ```json_value``` pointers.  See the json_example.f90 file for more examples.

```fortran
    program example2
    
        use,intrinsic :: iso_fortran_env, only: wp => real64 

        use json_module

        type(json_value),pointer :: p, inp
        logical :: found

        ! initialize the module
        call json_initialize()

        ! initialize the structure:
        call json_create_object(p)

        ! add an "inputs" object to the structure:
        call json_create_object(inp,'inputs')
        call json_add(p, inp) !add it to the root

        ! add some data to inputs:
        call json_add(inp, 't0', 0.1_wp)
        call json_add(inp, 'tf', 1.1_wp)
        call json_add(inp, 'x0', 9999.0000d0)
        call json_add(inp, 'integer_scalar', 787)
        call json_add(inp, 'integer_array', [2,4,99])
        call json_add(inp, 'names', ['aaa','bbb','ccc'])
        call json_add(inp, 'logical_scalar', .true.)
        call json_add(inp, 'logical_vector', [.true., .false., .true.])
        nullify(inp)  !don't need this anymore

        ! write the file:
        call json_print(p,'test2.json')

        !cleanup:
        call json_destroy(p)

    end program example2
```

The code above produces the file:

```Python
{
  "inputs": {
    "t0": 0.1000000000000000E+000,
    "tf": 0.1100000000000000E+001,
    "x0": 0.9999000000000000E+004,
    "integer_scalar": 787,
    "integer_array": [
      2,
      4,
      99
    ],
    "names": [
      "aaa",
      "bbb",
      "ccc"
    ],
    "logical_scalar": true,
    "logical_vector": [
      true,
      false,
      true
    ]
  }
}
```

Documentation
--------------

The API documentation for the latest release version can be found [here](http://jacobwilliams.github.io/json-fortran).  The documentation can also be generated by processing the source files with [RoboDoc](http://rfsber.home.xs4all.nl/Robo/).  Note that both the shell script, CMake, and SCons will also generate these files automatically in the documentation folder, assuming you have RoboDoc installed. 

License
--------

The json-fortran sourcecode and related files and documentation are distributed under a permissive free software license (BSD-style).  See the [LICENSE](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE) file for more details.

Other Comments
---------------

This code is a fork and extensive upgrade of the Fortran 95 [FSON](https://github.com/josephalevin/fson) code.  It
includes many features that the original code did not have, and fixes many of that code's bugs.

More About JSON
------------
For more information about JSON, see: <http://www.json.org/>
