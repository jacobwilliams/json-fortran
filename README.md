![JSON-Fortran](/media/json-fortran-logo-2.png)
============

JSON-Fortran: A Modern Fortran JSON API

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
[![Build Status](https://github.com/jacobwilliams/json-fortran/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/json-fortran/actions)
[![GitHub issues](https://img.shields.io/github/issues/jacobwilliams/json-fortran.png)](https://github.com/jacobwilliams/json-fortran/issues)
[![Codecov](https://codecov.io/gh/jacobwilliams/json-fortran/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/json-fortran)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/json-fortran)](https://github.com/jacobwilliams/json-fortran/commits/master)


Take a look at the
[CHANGELOG](https://github.com/jacobwilliams/json-fortran/blob/master/CHANGELOG.md#unreleased)
for a list of changes since the latest release.

[top](#json-fortran)

Brief description
---------------

JSON-Fortran is a user-friendly, thread-safe, and object-oriented API for reading and writing [JSON](http://json.org) files, written in modern Fortran.

[top](#json-fortran)

Download
--------------------

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/json-fortran.svg)](https://github.com/jacobwilliams/json-fortran/releases)
[![homebrew version](https://img.shields.io/homebrew/v/json-fortran.svg)](https://formulae.brew.sh/formula/json-fortran)
[![Conda (channel only)](https://img.shields.io/conda/vn/conda-forge/json-fortran)](https://github.com/conda-forge/json-fortran-feedstock)

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
[json-fortran formula](https://formulae.brew.sh/formula/json-fortran):

```bash
brew update
brew info json-fortran
brew install --with-unicode-support json-fortran
```

_Please note_, if you wish to support usage of JSON-Fortran with
multiple Fortran compilers, please follow the CMake installation
instructions below, as the homebrew installation is only intended to
support a single Fortran compiler. Cheers!

__NEWS:__ As of January 20, 2022,
[json-fortran](https://github.com/jacobwilliams/json-fortran) can be
downloaded and installed via the [conda](https://docs.conda.io/en/latest/)
package manager on Mac OS X and Linux. Once a conda-distribution, like
[miniforge](https://github.com/conda-forge/miniforge), is installed
the [json-fortran package](https://anaconda.org/conda-forge/json-fortran)
can be installed.

```bash
conda install json-fortran
```

_Note:_ Packages on conda-forge are build with GCC 9.4 which is upwards
compatible with newer GCC versions, but not with other Fortran compilers.

[top](#json-fortran)

Building the library
--------------------

The code requires a Fortran compiler that supports
various Fortran 2003 and Fortran 2008 features such as: allocatable
strings, `newunit`, `generic`, `class`, and `abstract interface`.
It has been successfully compiled with the [Intel Fortran compiler
13.1.0](https://software.intel.com/en-us/articles/non-commercial-software-development) (and greater) and the [GNU gfortran
compiler](http://gcc.gnu.org/wiki/GFortran) [4.9 and greater]. It has also
been reported that the library can be built (using the CMake build
script) with the [NAG Fortran compiler 6.0](http://www.nag.com/nagware/NP/NP_desc.asp)

Currently, several ways are provided to build the JSON-fortran library
(libjsonfortran).

* A build script, `build.sh` is provided in the project root directory. This script uses [FoBiS](https://github.com/szaghi/FoBiS) to build the JSON-Fortran library and the unit tests on Unix-like systems.  Edit the script to use either the [Intel Fortran Compiler](https://software.intel.com/en-us/fortran-compilers) or [Gfortran](https://gcc.gnu.org/wiki/GFortran).  Note that version 1.2.5 of FoBiS (or later) is required.

* A [FoBiS](https://github.com/szaghi/FoBiS) configuration file (`json-fortran.fobis`) is also provided that can also build the library and examples. Use the `mode` flag to indicate what to build. For example:

  * To build all the examples using gfortran: `FoBiS.py build -f json-fortran.fobis -mode tests-gnu`
  * To build all the examples using ifort: `FoBiS.py build -f json-fortran.fobis -mode tests-intel`
  * To build a static library using gfortran: `FoBiS.py build -f json-fortran.fobis -mode static-gnu`
  * To build a static library using ifort: `FoBiS.py build -f json-fortran.fobis -mode static-intel`

  The full set of modes are: `static-gnu`, `static-gnu-debug`, `static-intel`, `static-intel-debug`, `shared-gnu`, `shared-gnu-debug`, `shared-intel`, `shared-intel-debug`, `tests-gnu`, `tests-gnu-debug`, `tests-intel`, `tests-intel-debug`

  To generate the documentation using [ford](https://github.com/Fortran-FOSS-Programmers/ford), run: ```FoBis.py rule --execute makedoc -f json-fortran.fobis```

  To run all the tests, run: ```FoBis.py rule --execute tests -f json-fortran.fobis```

* A [Visual Studio](https://www.visualstudio.com) project is included for building the library (and unit tests) on Windows with the Intel Fortran Compiler.  The project has been tested with Visual Studio 2010 and 2013.

* A [CMake](http://www.cmake.org) build
system is provided. This build system has been tested on Mac and Linux
using the Intel Fortran Compiler, gfortran 4.9, and NAG Fortran 6.0. It does also work on Windows (but note that the Visual Studio project it generates is not quite the same as the one mentioned above). This CMake based build provides an install target,
and exports from both the install location and the build location so
that building and using JSON-Fortran in another CMake based project is
trivial. To get started with the CMake based build, set the
environment variable `FC` to point to your Fortran compiler, and
create a build directory. Then `(cmake-gui|ccmake|cmake)
/path/to/json-fortran-root` to configure, `make` to build and `make
install` to optionally install. You can also use `make check` to build and run the unit tests.
As long as the project is built with
CMake, other CMake projects can find it and link against it. For example,
if you have a second copy of the JSON-Fortran project tree, and want to build the unit tests
linking against those compiled/installed by the first copy:

```CMake
cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )
enable_language ( Fortran )
project ( jf_test NONE )

find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 8.3.0 REQUIRED )

file ( GLOB JF_TEST_SRCS "src/tests/jf_test_*.F90" )
foreach ( UNIT_TEST ${JF_TEST_SRCS} )
  get_filename_component ( TEST ${UNIT_TEST} NAME_WE )
  add_executable ( ${TEST} ${UNIT_TEST} )
  target_link_libraries ( ${TEST} jsonfortran::jsonfortran-static )
  # or for linking against the dynamic/shared library:
  # target_link_libraries ( ${TEST} jsonfortran::jsonfortran ) # instead
endforeach()
```

* A [Fortran Package Manager](https://github.com/fortran-lang/fpm) file is also included, so that JSON-Fortran can be compiled with FPM.

[top](#json-fortran)

Documentation
--------------

The API documentation for the latest release version can be found
[here](https://jacobwilliams.github.io/json-fortran/).  The
documentation can also be generated by processing the source files
with [FORD](https://github.com/Fortran-FOSS-Programmers/ford).  Note that both the
shell script and CMake will also generate these files automatically in the documentation folder, assuming you have FORD installed.

Some examples can also be found on the [wiki](https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage).

[top](#json-fortran)

Contributing
------------

Want to help?  Take a quick look at our [contributing guidelines](https://github.com/jacobwilliams/json-fortran/blob/master/.github/CONTRIBUTING.md) then claim something and [Fork. Commit. Pull request.](https://help.github.com/articles/fork-a-repo/)

[top](#json-fortran)

License
--------
The JSON-Fortran source code and related files and documentation are distributed under a permissive free software license (BSD-style).  See the [LICENSE](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE) file for more details.

[top](#json-fortran)

Miscellaneous
---------------

* JSON-Fortran is a fork and extensive upgrade of the Fortran 95 [FSON](https://github.com/josephalevin/fson) code. The reason for the split was to be able to incorporate object-oriented and other nice features of the Fortran 2003 and 2008 standards.  Many thanks to the original authors of FSON.
* For more information about JSON, see: <http://www.json.org/>
* [json-fortran on Codecov.IO](https://codecov.io/gh/jacobwilliams/json-fortran)

[top](#json-fortran)
