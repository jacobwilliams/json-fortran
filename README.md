json-fortran [![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases/latest)
============

A Fortran 2008 JSON API

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [json-fortran [![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases/latest)](#json-fortran-github-releasehttpsimgshieldsiogithubreleasejacobwilliamsjson-fortransvgstyleplastichttpsgithubcomjacobwilliamsjson-fortranreleaseslatest)
    - [Status](#status)
    - [Brief description](#brief-description)
    - [Download [![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases)](#download-github-releasehttpsimgshieldsiogithubreleasejacobwilliamsjson-fortransvgstyleplastichttpsgithubcomjacobwilliamsjson-fortranreleases)
    - [Building the library](#building-the-library)
    - [Reading JSON from a file](#reading-json-from-a-file)
    - [Reading JSON from a string](#reading-json-from-a-string)
    - [Modifying variables in a JSON file](#modifying-variables-in-a-json-file)
    - [Writing a JSON file](#writing-a-json-file)
    - [Building a JSON file from scratch](#building-a-json-file-from-scratch)
    - [Documentation](#documentation)
    - [Contributing [![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](CONTRIBUTING.md)](#contributing-ready-in-backloghttpsbadgewaffleiojacobwilliamsjson-fortranpnglabelreadytitlereadycontributingmd)
    - [License](#license)
    - [Miscellaneous](#miscellaneous)

<!-- markdown-toc end -->


Status
------
[![Build Status](https://img.shields.io/travis/jacobwilliams/json-fortran/master.svg?style=plastic)](https://travis-ci.org/jacobwilliams/json-fortran)
[![Coveralls branch](https://img.shields.io/coveralls/jacobwilliams/json-fortran/master.svg?style=plastic)](https://coveralls.io/r/jacobwilliams/json-fortran) <br/>
[![GitHub issues](https://img.shields.io/github/issues/jacobwilliams/json-fortran.png?style=plastic)](https://github.com/jacobwilliams/json-fortran/issues)
[![Blocked by Vendor Bug](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=vendor%20bug&title=Blocked%20by%20Vendor%20Bug)](https://waffle.io/jacobwilliams/json-fortran)
[![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](https://github.com/jacobwilliams/json-fortran/#contributing-)
[![In Progress](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=In%20Progress&title=In%20Progress)](https://waffle.io/jacobwilliams/json-fortran)
[![Needs Review](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Needs%20Review&title=Needs%20Review)](https://waffle.io/jacobwilliams/json-fortran)

Brief description
---------------

A user-friendly and object-oriented API for reading and writing JSON files, written in
modern Fortran.  The source code is a single Fortran module file ([json_module.F90](https://github.com/jacobwilliams/json-fortran/blob/master/src/json_module.F90)).

Download [![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases)
--------------------

Download the official versioned releases [here](https://github.com/jacobwilliams/json-fortran/releases/latest).  Or, get the latest development code from the master branch [here](https://github.com/jacobwilliams/json-fortran.git).

Building the library
--------------------

The code requires a Fortran compiler that supports
various Fortran 2003 and Fortran 2008 features such as: allocatable
strings, `newunit`, `generic`, `class`, and `abstract interface`.
It has been successfully compiled with the [Intel Fortran compiler
13.1.0](https://software.intel.com/en-us/non-commercial-software-development) (and greater) and the recent [4.9 release of the GNU gfortran
compiler](http://gcc.gnu.org/wiki/GFortran/News#GCC4.9). It has also
been reported that the library can be built (using the CMake build
script) with the [NAG Fortran compiler 6.0](http://www.nag.com/nagware/NP/NP_desc.asp)

Currently, several ways are provided to build the jsonfortran library
(libjsonfortran).

* A build script, `build.sh` is provided in the project root directory. This script uses [FoBiS](https://github.com/szaghi/FoBiS) to build the json-fortran library and the unit tests.  Edit the script to use either the [Intel Fortran Compiler](https://software.intel.com/en-us/fortran-compilers) or [Gfortran](https://gcc.gnu.org/wiki/GFortran).  Note that version 1.2.5 of FoBiS (or later) is required.

* A [Visual Studio](http://www.visualstudio.com) project is included for building the library (and unit tests) on Windows with the Intel Fortran Compiler.  The project has been tested with Visual Studio 2010 and 2013.

* An [SCons](http://www.scons.org) `SConstruct` file.  The library and unit tests are built by typing `scons` and tested by typing `scons test`. The library may be optionally installed by `scons install` or `sudo scons install`.

* Additionally, a [CMake](http://www.cmake.org) build
system is provided. This build system has been tested on Mac and Linux
using the Intel Fortran Compiler, gfortran 4.9, and NAG Fortran 6.0. It has not been
tested on Windows. This CMake based build provides an install target,
and exports from both the install location and the build location so
that building and using json-fortran in another CMake based project is
trivial. To get started with the CMake based build, set the
environment variable `FC` to point to your Fortran compiler, and
create a build directory. Then `(cmake-gui|ccmake|cmake)
/path/to/json-fortran-root` to configure, `make` to build and `make
install` to optionally install. As long as the project is built with
CMake, other CMake projects can find it and link against it. For example,
if you have a second copy of the json-fortran project tree, and want to build the unit tests
linking against those compiled/installed by the first copy:

```CMake
cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )
enable_language ( Fortran )
project ( jf_test NONE )

find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 4.1.0 REQUIRED )
include_directories ( "${jsonfortran_INCLUDE_DIRS}" )

file ( GLOB JF_TEST_SRCS "src/tests/jf_test_*.f90" )
foreach ( UNIT_TEST ${JF_TEST_SRCS} )
  get_filename_component ( TEST ${UNIT_TEST} NAME_WE )
  add_executable ( ${TEST} ${UNIT_TEST} )
  target_link_libraries ( ${TEST} jsonfortran-static )
  # or for linking against the dynamic/shareed library:
  # target_link_libraries ( ${TEST} jsonfortran ) # instead
endforeach()
```

Reading JSON from a file
---------------

Reading a JSON file and getting data from it is fairly
straightforward using the `json_file` class.  Here is an example.  See unit tests 1 and 3-6
for more examples. The source files may be found in `src/tests/`.

```fortran
    program example1

        use json_module

        type(json_file) :: json
        logical :: found
        integer :: i,j,k

        ! initialize the module
        call json_initialize()

        ! read the file
        call json%load_file(filename = '../files/inputs/test1.json')

        ! print the file to the console
        call json%print_file()

        ! extract data from the file
        ! [found can be used to check if the data was really there]
        call json%get('version.major', i, found)
        if ( .not. found ) stop 1
        call json%get('version.minor', j, found)
        if ( .not. found ) stop 1
        call json%get('data(1).number', k, found)
        if ( .not. found ) stop 1

        ! clean up
        call json%destroy()
        if (json_failed()) stop 1

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

After reading a JSON file, if you want to change the values of some of the variables, you can use the `update` method.  For the example above:

```fortran
    ! [found can be used to check if the data was really there]
    call json%update('version.major',9,found)  !change major version to 9
    call json%update('version.minor',0,found)  !change minor version to 0
    call json%update('version.patch',0,found)  !change patch to 0
```

Writing a JSON file
---------------

To print the JSON file (either to a file or the console), the `print_file` method can be used.  For the above example:

```fortran
    call json%print_file()         !prints to the console
    call json%print_file(iunit)    !prints to the file connected to iunit
```

Building a JSON file from scratch
---------------

Constructing a JSON file element by element is slightly more complicated and involves the use
of `json_value` pointers.  For more examples see unit tests 2, 4 and 7 in `src/tests/`.

```fortran
    program example2

        use,intrinsic :: iso_fortran_env, only: wp => real64

        use json_module

        type(json_value),pointer :: p, inp

        ! initialize the module
        call json_initialize()

        ! initialize the structure:
        call json_create_object(p,'')

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
        call json_print(p,'../files/example2.json')

        !cleanup:
        call json_destroy(p)
        if (json_failed()) stop 1

    end program example2
```

The code above produces the file:

```JSON
{
  "inputs": {
    "t0": 0.1E+0,
    "tf": 0.11E+1,
    "x0": 0.9999E+4,
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

Contributing [![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](CONTRIBUTING.md)
------------
Want to help?  Take a quick look at our [contributing guidelines](CONTRIBUTING.md) then claim something in [the "ready" column on our Waffle.io](https://waffle.io/jacobwilliams/json-fortran) and [Fork. Commit. Pull request.](https://help.github.com/articles/fork-a-repo).

License
--------
The json-fortran source code and related files and documentation are distributed under a permissive free software license (BSD-style).  See the [LICENSE](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE) file for more details.

Miscellaneous
---------------

* This code is a fork and extensive upgrade of the Fortran 95 [FSON](https://github.com/josephalevin/fson) code. The reason for the split was to be able to incorporate object-oriented and other nice features of the Fortran 2003 and 2008 standards.  Many thanks to the original authors of FSON.
* For more information about JSON, see: <http://www.json.org/>
* [json-fortran on Travis CI](https://travis-ci.org/jacobwilliams/json-fortran)
* [json-fortran on Waffle.IO](https://waffle.io/jacobwilliams/json-fortran)
* [json-fortran on Coveralls.IO](https://coveralls.io/r/jacobwilliams/json-fortran)
