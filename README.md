JSON-Fortran
============

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases/latest)
A Fortran 2008 JSON API

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [JSON-Fortran](#json-fortran)
    - [Status](#status)
    - [Brief description](#brief-description)
    - [Download](#download)
    - [Building the library](#building-the-library)
    - [Documentation](#documentation)
    - [Contributing](#contributing)
    - [License](#license)
    - [Miscellaneous](#miscellaneous)

<!-- markdown-toc end -->

Status
------
[![Build Status](https://img.shields.io/travis/jacobwilliams/json-fortran/master.svg?style=plastic)](https://travis-ci.org/jacobwilliams/json-fortran)
[![Codecov](https://img.shields.io/codecov/c/github/jacobwilliams/json-fortran.svg?style=plastic)](https://codecov.io/github/jacobwilliams/json-fortran?branch=master)

[![GitHub issues](https://img.shields.io/github/issues/jacobwilliams/json-fortran.png?style=plastic)](https://github.com/jacobwilliams/json-fortran/issues)
[![Blocked by Vendor Bug](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=vendor%20bug&title=Blocked%20by%20Vendor%20Bug)](https://waffle.io/jacobwilliams/json-fortran)
[![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](https://github.com/jacobwilliams/json-fortran/#contributing)
[![In Progress](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=In%20Progress&title=In%20Progress)](https://waffle.io/jacobwilliams/json-fortran)
[![Needs Review](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Needs%20Review&title=Needs%20Review)](https://waffle.io/jacobwilliams/json-fortran)

Take a look at the
[CHANGELOG](https://github.com/jacobwilliams/json-fortran/blob/master/CHANGELOG.md#unreleased)
for a list of changes since the latest release.

[top](#json-fortran)

Brief description
---------------

A user-friendly and object-oriented API for reading and writing JSON files, written in
modern Fortran.  The source code is a single Fortran module file ([json_module.F90](https://github.com/jacobwilliams/json-fortran/blob/master/src/json_module.F90)).

[top](#json-fortran)

Download
--------------------

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg?style=plastic)](https://github.com/jacobwilliams/json-fortran/releases)

Download the official versioned releases
[here](https://github.com/jacobwilliams/json-fortran/releases/latest).
Or, get the latest development code from the master branch
[here](https://github.com/jacobwilliams/json-fortran.git).

__NEWS:__ As of June 7, 2015,
[json-fortran](https://github.com/jacobwilliams/json-fortran) can be
downloaded and installed with the [homebrew](http://brew.sh) package
manager on Mac OS X. Once [homebrew](http://brew.sh) is installed,
make sure that the formulae are up to date, view the package options
and caveats, and install the
[json-fortran formula](http://braumeister.org/formula/json-fortran):

```bash
brew update
brew info json-fortran
brew install --with-unicode-support json-fortran
```

_Please note_, if you wish to support usage of json-fortran with
multiple Fortran compilers, please follow the CMake installation
instructions below, as the homebrew installation is only intended to
support a single Fortran compiler. Cheers!

[top](#json-fortran)

Building the library
--------------------

The code requires a Fortran compiler that supports
various Fortran 2003 and Fortran 2008 features such as: allocatable
strings, `newunit`, `generic`, `class`, and `abstract interface`.
It has been successfully compiled with the [Intel Fortran compiler
13.1.0](https://software.intel.com/en-us/articles/non-commercial-software-development) (and greater) and the recent [4.9 release of the GNU gfortran
compiler](http://gcc.gnu.org/wiki/GFortran/News#GCC4.9). It has also
been reported that the library can be built (using the CMake build
script) with the [NAG Fortran compiler 6.0](http://www.nag.com/nagware/NP/NP_desc.asp)

Currently, several ways are provided to build the jsonfortran library
(libjsonfortran).

* A build script, `build.sh` is provided in the project root directory. This script uses [FoBiS](https://github.com/szaghi/FoBiS) to build the json-fortran library and the unit tests.  Edit the script to use either the [Intel Fortran Compiler](https://software.intel.com/en-us/fortran-compilers) or [Gfortran](https://gcc.gnu.org/wiki/GFortran).  Note that version 1.2.5 of FoBiS (or later) is required.

* A [Visual Studio](https://www.visualstudio.com) project is included for building the library (and unit tests) on Windows with the Intel Fortran Compiler.  The project has been tested with Visual Studio 2010 and 2013.

* A [CMake](http://www.cmake.org) build
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

find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 4.2.0 REQUIRED )
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

[top](#json-fortran)

Documentation
--------------

The API documentation for the latest release version can be found
[here](http://jacobwilliams.github.io/json-fortran/).  The
documentation can also be generated by processing the source files
with [FORD](https://github.com/cmacmackin/ford).  Note that both the
shell script and CMake will also generate these files automatically in the documentation folder, assuming you have FORD installed.

Some examples can also be found on the [wiki](https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage).

[top](#json-fortran)

Contributing
------------

[![Ready in backlog](https://badge.waffle.io/jacobwilliams/json-fortran.png?label=Ready&title=Ready)](https://github.com/jacobwilliams/json-fortran/blob/master/CONTRIBUTING.md)

Want to help?  Take a quick look at our [contributing guidelines](https://github.com/jacobwilliams/json-fortran/blob/master/CONTRIBUTING.md) then claim something in [the "ready" column on our Waffle.io](https://waffle.io/jacobwilliams/json-fortran) and [Fork. Commit. Pull request.](https://help.github.com/articles/fork-a-repo/)

[top](#json-fortran)

License
--------
The json-fortran source code and related files and documentation are distributed under a permissive free software license (BSD-style).  See the [LICENSE](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE) file for more details.

[top](#json-fortran)

Miscellaneous
---------------

* This code is a fork and extensive upgrade of the Fortran 95 [FSON](https://github.com/josephalevin/fson) code. The reason for the split was to be able to incorporate object-oriented and other nice features of the Fortran 2003 and 2008 standards.  Many thanks to the original authors of FSON.
* For more information about JSON, see: <http://www.json.org/>
* [json-fortran on Travis CI](https://travis-ci.org/jacobwilliams/json-fortran)
* [json-fortran on Waffle.IO](https://waffle.io/jacobwilliams/json-fortran)
* [json-fortran on Codecov.IO](https://codecov.io/github/jacobwilliams/json-fortran?branch=master)

[top](#json-fortran)
