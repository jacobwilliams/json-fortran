## Change Log

**Table of Contents**

- [Change Log](#change-log)
    - [Unreleased](#unreleased)
    - [5.0.1 (2016-06-11)](#501-2016-06-11)
    - [5.0.0 (2016-05-09)](#500-2016-05-09)
    - [4.3.0 (2015-12-05)](#430-2015-12-05)
    - [4.2.0 (2015-08-03)](#420-2015-08-03)
    - [4.1.1 (2015-05-27)](#411-2015-05-27)
    - [4.1.0 (2015-05-05)](#410-2015-05-05)
    - [4.0.0 (2015-03-16)](#400-2015-03-16)
    - [3.1.0 (2015-02-28)](#310-2015-02-28)
    - [3.0.0 (2015-01-18)](#300-2015-01-18)
    - [2.0.0 (2014-12-27)](#200-2014-12-27)
    - [1.0.0 (2014-06-23)](#100-2014-06-23)

### [Unreleased](https://github.com/jacobwilliams/json-fortran/tree/HEAD)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/5.0.1...HEAD)

### [5.0.1](https://github.com/jacobwilliams/json-fortran/tree/5.0.1) (2016-06-11)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/5.0.0...5.0.1)
or [Download v5.0.1](https://github.com/jacobwilliams/json-fortran/releases/tag/5.0.1)

*Note: This release contains no changes to the library, only the build system and documentation.*

**Fixed issues:**

- FORD now uses the Intel preprocessor when library is built with the Intel compiler [\#205](https://github.com/jacobwilliams/json-fortran/issues/205)
- ShellCheck script files [\#181](https://github.com/jacobwilliams/json-fortran/issues/181)

**Merged pull requests:**

- De-lint shell scripts with ShellCheck, debug FORD upgrade, and various updates to documentation and build system.  [\#201](https://github.com/jacobwilliams/json-fortran/pull/201) ([zbeekman](https://github.com/zbeekman))

### [5.0.0](https://github.com/jacobwilliams/json-fortran/tree/5.0.0) (2016-05-09)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.3.0...5.0.0)
or [Download v5.0.0](https://github.com/jacobwilliams/json-fortran/releases/tag/5.0.0)

See also [this Wiki page](https://github.com/jacobwilliams/json-fortran/wiki/5.0-Changes) on how to update your code to be compatible with this release.

**Enhancements:**

- Split `json_module` into multiple files. [\#168](https://github.com/jacobwilliams/json-fortran/issues/168) [\#170](https://github.com/jacobwilliams/json-fortran/pull/170) ([jacobwilliams](https://github.com/jacobwilliams))
- The library is now thread safe [\#36](https://github.com/jacobwilliams/json-fortran/issues/36) [\#170](https://github.com/jacobwilliams/json-fortran/pull/170) ([jacobwilliams](https://github.com/jacobwilliams))
- Added new `initialize()`, `failed()`, `print_error_message()`, `check_to_errors()`, and `clear_exceptions()` to the `json_file` class. [\#170](https://github.com/jacobwilliams/json-fortran/pull/170) ([jacobwilliams](https://github.com/jacobwilliams))
- The number of spaces for indenting can now be user specified. [\#170](https://github.com/jacobwilliams/json-fortran/pull/170) ([jacobwilliams](https://github.com/jacobwilliams))
- Updates for Visual Studio project [\#170](https://github.com/jacobwilliams/json-fortran/pull/170) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a `validate()` method for validating JSON linked lists [\#196](https://github.com/jacobwilliams/json-fortran/issues/196) [\#197](https://github.com/jacobwilliams/json-fortran/pull/197) ([jacobwilliams](https://github.com/jacobwilliams))
- Added some additional error checks for malformed JSON linked lists [\#182](https://github.com/jacobwilliams/json-fortran/pull/182) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a new routine `is_child_of` to check if one `json_value` is a descendant of another [\#182](https://github.com/jacobwilliams/json-fortran/pull/182) ([jacobwilliams](https://github.com/jacobwilliams))
- Added new options for case-insensitive searching of names/paths, as well as the option to consider trailing space significant [\#185](https://github.com/jacobwilliams/json-fortran/issues/185) [\#192](https://github.com/jacobwilliams/json-fortran/pull/192) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a routine to enable swapping of elements in a `json_value` linked list [\#177](https://github.com/jacobwilliams/json-fortran/issues/177) [\#182](https://github.com/jacobwilliams/json-fortran/pull/182) ([jacobwilliams](https://github.com/jacobwilliams))
- Rename parameters `RK`, `IK`, `LK`, `CK`, and `CDK` using less generic names (`json_RK`, `json_IK`, `json_LK`, `json_CK`, and `json_CDK`) [\#172](https://github.com/jacobwilliams/json-fortran/issues/172) ([jacobwilliams](https://github.com/jacobwilliams))
- Calling the `initialize()` method (which is now in `json_core` and    `json_file`) is no longer mandatory [\#171](https://github.com/jacobwilliams/json-fortran/issues/171) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a `rename()` method for renaming a `json_value` variable [\#184](https://github.com/jacobwilliams/json-fortran/issues/184) [\#187](https://github.com/jacobwilliams/json-fortran/pull/187) ([jacobwilliams](https://github.com/jacobwilliams))
- Added some compiler directives so that workarounds for Gfortran bugs are not used for other compilers [\#190](https://github.com/jacobwilliams/json-fortran/issues/190) ([jacobwilliams](https://github.com/jacobwilliams))
- Added option for strict typing for `get` routines [\#173](https://github.com/jacobwilliams/json-fortran/issues/173) ([jacobwilliams](https://github.com/jacobwilliams))
- Various cosmetic changes and renaming of some of the dummy arguments in some procedures. [\#198](https://github.com/jacobwilliams/json-fortran/pull/198) ([jacobwilliams](https://github.com/jacobwilliams))
- Added checks to avoid unnecessary looping when traversing arrays if an exception is thrown [\#183](https://github.com/jacobwilliams/json-fortran/pull/183) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a new `get_child` method to get the first child. [9d4f736] ([jacobwilliams](https://github.com/jacobwilliams))
- Added some additional error checks for unassociated pointers [9d4f736] ([jacobwilliams](https://github.com/jacobwilliams))
- Ensure null pointers are returned for some error cases [838f476] ([jacobwilliams](https://github.com/jacobwilliams))

**Fixed Issues**

- Fixed a bug in the `traverse()` routine, where the `finished` output flag was not being correctly checked [\#182](https://github.com/jacobwilliams/json-fortran/pull/182) ([jacobwilliams](https://github.com/jacobwilliams))

### [4.3.0](https://github.com/jacobwilliams/json-fortran/tree/4.3.0) (2015-12-05)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.2.0...4.3.0)
or [Download v4.3.0](https://github.com/jacobwilliams/json-fortran/releases/tag/4.3.0)

**Enhancements:**

- Added routines for getting the `parent`, `next`, `previous`, and `tail` pointers [\#161](https://github.com/jacobwilliams/json-fortran/issues/161) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a deep copy routine for `json_value` structures [\#160](https://github.com/jacobwilliams/json-fortran/issues/160) ([jacobwilliams](https://github.com/jacobwilliams))
- Updated the Visual Studio solution to VS 2013. Also removed the test projects (it now just compiles the library) ([jacobwilliams](https://github.com/jacobwilliams))

**Fixed issues:**

- Rewrote logic for decoding strings, and fixed bugs related to parsing strings with certain escape character combinations [\#164](https://github.com/jacobwilliams/json-fortran/issues/164) ([jacobwilliams](https://github.com/jacobwilliams))
- Fixed a bug where some real numbers weren't being read in properly. Also added an option to output real numbers with `fmt=*` formatting [\#157](https://github.com/jacobwilliams/json-fortran/issues/157) ([jacobwilliams](https://github.com/jacobwilliams))

**Merged pull requests:**

- Minor documentation change. [\#153](https://github.com/jacobwilliams/json-fortran/pull/153) ([jacobwilliams](https://github.com/jacobwilliams))
- Enabled FORD graphs in documentation. [\#149](https://github.com/jacobwilliams/json-fortran/pull/149) ([jacobwilliams](https://github.com/jacobwilliams))
- Tag documentation fix [\#147](https://github.com/jacobwilliams/json-fortran/pull/147) ([zbeekman](https://github.com/zbeekman))
- Fix url in 4.2.0 tag documentation [\#146](https://github.com/jacobwilliams/json-fortran/pull/146) ([zbeekman](https://github.com/zbeekman))

### [4.2.0](https://github.com/jacobwilliams/json-fortran/tree/4.2.0) (2015-08-03)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.1.1...4.2.0)
or [Download v4.2.0](https://github.com/jacobwilliams/json-fortran/releases/tag/4.2.0)

**Enhancements**

- The ```exception_thrown``` flag is now true by default before the ```json_initialize``` routine is called to initialize the module.  [\#142](https://github.com/jacobwilliams/json-fortran/issues/142) ([jacobwilliams](https://github.com/jacobwilliams))
- Updated ```json_info``` to get name of ```json_value``` variable.
[\#141](https://github.com/jacobwilliams/json-fortran/issues/141) ([jacobwilliams](https://github.com/jacobwilliams))
- Added a new routine (```json_traverse```) to traverse all nodes of a JSON structure [\#140](https://github.com/jacobwilliams/json-fortran/issues/140)
via [PR \#144](https://github.com/jacobwilliams/json-fortran/pull/144) from  ([jacobwilliams](https://github.com/jacobwilliams))
- Added FORD documentation automatic deployment via
  [PR \#137](https://github.com/jacobwilliams/json-fortran/pull/137)
  from [zbeekman](https://github.com/zbeekman)
- Added new version release checklist,
  [\#122](https://github.com/jacobwilliams/json-fortran/issues/122)
  via
  [PR \#137](https://github.com/jacobwilliams/json-fortran/pull/137)
  from [zbeekman](https://github.com/zbeekman)
- Added finer control of real format printing,
  [\#117](https://github.com/jacobwilliams/json-fortran/issues/117)
  via
  [PR \#137](https://github.com/jacobwilliams/json-fortran/pull/137)
  from [zbeekman](https://github.com/zbeekman), and [\#143](https://github.com/jacobwilliams/json-fortran/issues/143) from [jacobwilliams](https://github.com/jacobwilliams)
- Removed official SCons support to facilitate faster development
  cycle,
  [\#121](https://github.com/jacobwilliams/json-fortran/issues/121)
  via
  [PR \#130](https://github.com/jacobwilliams/json-fortran/pull/130)
  from [zbeekman](https://github.com/zbeekman)
- Add a
  [CHANGELOG](https://github.com/jacobwilliams/json-fortran/blob/master/CHANGELOG.md)
  [\#120](https://github.com/jacobwilliams/json-fortran/issues/120)
  via
  [PR \#123](https://github.com/jacobwilliams/json-fortran/pull/123)
  from [zbeekman](https://github.com/zbeekman)
- Spell 'Fortran' correctly
  [\#118](https://github.com/jacobwilliams/json-fortran/issues/118)
  via
  [PR \#124](https://github.com/jacobwilliams/json-fortran/pull/124)
  from [zbeekman](https://github.com/zbeekman)
- Migrate to
  [Codecov.io](https://codecov.io/github/jacobwilliams/json-fortran?branch=master)
  [\#106](https://github.com/jacobwilliams/json-fortran/issues/106)
  via [PR \#107](https://github.com/jacobwilliams/json-fortran/pull/107)
  from [zbeekman](https://github.com/zbeekman)
- [CMake](http://www.cmake.org) emulate `make check` and exclude tests
  from `all` target
  [\#103](https://github.com/jacobwilliams/json-fortran/issues/103)
  implemented via
  [PR \#104](https://github.com/jacobwilliams/json-fortran/pull/104)
  from [zbeekman](https://github.com/zbeekman)

**Fixed issues:**

- Fixed bad gcov reports being generated due to a gcov/gfortran bug,
  [\#131](https://github.com/jacobwilliams/json-fortran/issues/131)
  via
  [PR \#137](https://github.com/jacobwilliams/json-fortran/pull/137)
  from [zbeekman](https://github.com/zbeekman)
- Fixed inaccurate coverage reports via
  [PR \#109](https://github.com/jacobwilliams/json-fortran/pull/109)
  from [zbeekman](https://github.com/zbeekman)
- Fixed a small consistency issue when outputting floating point
  numbers via
  [PR \#125](https://github.com/jacobwilliams/json-fortran/pull/125)
- Problems writing JSON to `error_unit` (0) due to JSON-Fortran's
  special interpretation of `unit=0`
  [\#85](https://github.com/jacobwilliams/json-fortran/issues/85)
  fixed via
  [PR \#111](https://github.com/jacobwilliams/json-fortran/pull/111)
  from [zbeekman](https://github.com/zbeekman)
- Broken parsing error message for empty line
  [PR \#110](https://github.com/jacobwilliams/json-fortran/pull/110)
  ([jacobwilliams](https://github.com/jacobwilliams))

**Merged pull requests:**

- Documentation fixes & coverage improvements
  [\#112](https://github.com/jacobwilliams/json-fortran/pull/112)
  ([zbeekman](https://github.com/zbeekman))
- Speedier container based
  [Travis-CI](https://travis-ci.org/jacobwilliams/json-fortran) builds
  [\#130](https://github.com/jacobwilliams/json-fortran/pull/130)
  ([zbeekman](https://github.com/zbeekman))

### [4.1.1](https://github.com/jacobwilliams/json-fortran/tree/4.1.1) (2015-05-27)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.1.0...4.1.1)
or [Download v4.1.1](https://github.com/jacobwilliams/json-fortran/releases/tag/4.1.1)

**Enhancements**

- Installation now possible with a [Homebrew](http://brew.sh)
  [package](http://braumeister.org/formula/json-fortran) on Mac OS X
- [CMake](http://www.cmake.org) install option to use a more traditional
  Gnu install structure (to support Homebrew installation)
  [PR \#101](https://github.com/jacobwilliams/json-fortran/pull/101)
  from [zbeekman](https://github.com/zbeekman)

**Fixed issues:**

- Broken
  [Travis-CI](https://travis-ci.org/jacobwilliams/json-fortran/builds)
  tests
  [\#99](https://github.com/jacobwilliams/json-fortran/issues/99)
  fixed by
  [PR \#100](https://github.com/jacobwilliams/json-fortran/pull/100)
  from [zbeekman](https://github.com/zbeekman)

- CMakeLists.txt accidentally defaulting to Unicode build due to small
  typo
  [\#96](https://github.com/jacobwilliams/json-fortran/issues/96),
  fixed by
  [PR \#97](https://github.com/jacobwilliams/json-fortran/pull/97)
  from [zbeekman](https://github.com/zbeekman)

**Merged pull requests:**




### [4.1.0](https://github.com/jacobwilliams/json-fortran/tree/4.1.0) (2015-05-05)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.0.0...4.1.0)
or [Download v4.1.0](https://github.com/jacobwilliams/json-fortran/releases/tag/4.1.0)

**Enhancements:**

- Run-time speed improvements
  [\#75](https://github.com/jacobwilliams/json-fortran/issues/75)
  implemented in
  [PR \#93](https://github.com/jacobwilliams/json-fortran/pull/93)
  from [jacobwilliams](https://github.com/jacobwilliams)

**Merged pull requests:**

- Small formatting and addition of a unit test
  [\#95](https://github.com/jacobwilliams/json-fortran/pull/95)
  ([jacobwilliams](https://github.com/jacobwilliams))



### [4.0.0](https://github.com/jacobwilliams/json-fortran/tree/4.0.0) (2015-03-16)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/3.1.0...4.0.0)
or [Download v4.0.0](https://github.com/jacobwilliams/json-fortran/releases/tag/4.0.0)

**Enhancements**

- Unicode support
  [\#35](https://github.com/jacobwilliams/json-fortran/issues/35)
  added in
  [PR \#84](https://github.com/jacobwilliams/json-fortran/pull/84)
  from [zbeekman](https://github.com/zbeekman)
- Automate testing of the stand alone programs listed in the README.md
  [\#67](https://github.com/jacobwilliams/json-fortran/issues/67)
  added in
  [PR \#71](https://github.com/jacobwilliams/json-fortran/pull/71)
  from [zbeekman](https://github.com/zbeekman)
- Automatically analyze and report test coverage info with
  [coveralls.io](https://coveralls.io/github/jacobwilliams/json-fortran)
  and
  [Travis-CI](https://travis-ci.org/jacobwilliams/json-fortran/builds)
  [\#63](https://github.com/jacobwilliams/json-fortran/issues/63)
  implemented in
  [PR \#72](https://github.com/jacobwilliams/json-fortran/pull/72)
  from [zbeekman](https://github.com/zbeekman)
- Enhanced command line user interface for `build.sh` build script
  [PR \#82](https://github.com/jacobwilliams/json-fortran/pull/82)
  ([zbeekman](https://github.com/zbeekman))
- Additional options and improvements made to command line interface
  of the `build.sh` build script
  [PR \#88](https://github.com/jacobwilliams/json-fortran/pull/88)
  ([zbeekman](https://github.com/zbeekman))
- Added unit tests for improved unit test coverage
  [PR \#83](https://github.com/jacobwilliams/json-fortran/pull/83)
  ([jacobwilliams](https://github.com/jacobwilliams)) and
  [PR \#73](https://github.com/jacobwilliams/json-fortran/pull/73)
  ([jacobwilliams](https://github.com/jacobwilliams))

**Fixed issues:**

- Stack overflow in destroy for large structure
  [\#76](https://github.com/jacobwilliams/json-fortran/issues/76)
  fixed in
  [PR \#77](https://github.com/jacobwilliams/json-fortran/pull/77)
  from [jacobwilliams](https://github.com/jacobwilliams)
- Discrepancy with dummy argument attributes for
  path between `get` and `get_..._vec` procedures
  [\#79](https://github.com/jacobwilliams/json-fortran/issues/79)
- Small bug fix for [CMake](http://www.cmake.org) install
  [\#68](https://github.com/jacobwilliams/json-fortran/pull/68)
  ([zbeekman](https://github.com/zbeekman))
- JSON output files from tests are overwriting expected outputs and
  outputs from other tests
  [\#86](https://github.com/jacobwilliams/json-fortran/pull/86)
  ([zbeekman](https://github.com/zbeekman))
- NAG Fortran Errors on `module procedure ::` change to `module
  procedure` with no double colon
  [PR \#78](https://github.com/jacobwilliams/json-fortran/pull/78)
  from [kmanalo](https://github.com/kmanalo)
- [CMake](http://www.cmake.org) for `jf_test_9.f90` broke
  [\#74](https://github.com/jacobwilliams/json-fortran/pull/74)
  ([zbeekman](https://github.com/zbeekman))

**Merged pull requests:**

- Very small changes and fixes:
  [\#81](https://github.com/jacobwilliams/json-fortran/pull/81)
  ([zbeekman](https://github.com/zbeekman))



### [3.1.0](https://github.com/jacobwilliams/json-fortran/tree/3.1.0) (2015-02-28)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/3.0.0...3.1.0)
or [Download v3.1.0](https://github.com/jacobwilliams/json-fortran/releases/tag/3.1.0)

**Enhancements:**

- Compact real number printing (`0.0` instead of `0.00000...E000`)
  with ability to revert to old behavior
  [\#39](https://github.com/jacobwilliams/json-fortran/issues/39)
  (implemented in
  [PR \#60](https://github.com/jacobwilliams/json-fortran/pull/60)
  from [zbeekman](https://github.com/zbeekman) and
  [PR \#61](https://github.com/jacobwilliams/json-fortran/pull/61)
  from [jacobwilliams](https://github.com/jacobwilliams))
- Read JSON structure from a character string
  [\#5](https://github.com/jacobwilliams/json-fortran/issues/5)
- Adopted official contributing guidelines and added a
  `CONTRIBUTING.md`
  [\#47](https://github.com/jacobwilliams/json-fortran/issues/47)

**Fixed issues:**

- Build script (`build.sh`) failing with Intel's ifort
  [\#53](https://github.com/jacobwilliams/json-fortran/issues/53),
  fixed by
  [PR \#54](https://github.com/jacobwilliams/json-fortran/pull/54)
  (from [zbeekman](https://github.com/zbeekman))
- Segfaults and unexpected behavior due to optional dummy arguments
  being dereferenced outside of `if (present(…))`
  [\#52](https://github.com/jacobwilliams/json-fortran/issues/52)
- Documentation not being produced for tests because tests directory
  should be sub-directory of src
  [\#49](https://github.com/jacobwilliams/json-fortran/issues/49)
- Example 8 failing on gfortran 4.9.2
  [\#44](https://github.com/jacobwilliams/json-fortran/issues/44)
- [CMake](http://www.cmake.org) Tests 3 and 8 fail due to new test
  case, example 6, added in
  [e636f96](https://github.com/jacobwilliams/json-fortran/commit/e636f9)
  [\#30](https://github.com/jacobwilliams/json-fortran/issues/30)
- [CMake](http://www.cmake.org) build using makefiles fails if
  parallel build is specified
  [\#8](https://github.com/jacobwilliams/json-fortran/issues/8), fixed
  by [PR \#50](https://github.com/jacobwilliams/json-fortran/pull/50)
  from [zbeekman](https://github.com/zbeekman)

**Merged pull requests:**

- Unit test updates
  [\#65](https://github.com/jacobwilliams/json-fortran/pull/65)
  ([jacobwilliams](https://github.com/jacobwilliams))
- Unit test cleanup as discussed in
  [\#30](https://github.com/jacobwilliams/json-fortran/issues/30)
  [\#64](https://github.com/jacobwilliams/json-fortran/pull/64)
  ([zbeekman](https://github.com/zbeekman))
- Stops PRs by [@jacobwilliams](https://github.com/jacobwilliams) from
  deploying master docs
  [\#62](https://github.com/jacobwilliams/json-fortran/pull/62)
  ([zbeekman](https://github.com/zbeekman))
- Auto deploy script
  [\#59](https://github.com/jacobwilliams/json-fortran/pull/59)
  ([zbeekman](https://github.com/zbeekman))
- Getting ready for auto-deployment
  [\#57](https://github.com/jacobwilliams/json-fortran/pull/57)
  ([zbeekman](https://github.com/zbeekman))
- [Travis-CI](https://travis-ci.org/jacobwilliams/json-fortran/builds)
  build matrix
  [\#56](https://github.com/jacobwilliams/json-fortran/pull/56)
  ([zbeekman](https://github.com/zbeekman))



### [3.0.0](https://github.com/jacobwilliams/json-fortran/tree/3.0.0) (2015-01-18)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/2.0.0...3.0.0)
or [Download v3.0.0](https://github.com/jacobwilliams/json-fortran/releases/tag/3.0.0)

**Fixed issues:**

- Intel compiler bug triggered by commit
  [d2622a8](https://github.com/jacobwilliams/json-fortran/commit/d2622a8)
  [\#41](https://github.com/jacobwilliams/json-fortran/issues/41)
- Unaligned data warnings with Intel compiler
  [\#34](https://github.com/jacobwilliams/json-fortran/issues/34)
- [CMake](http://www.cmake.org) fails due to bad path to `robodoc.rc`
  [\#31](https://github.com/jacobwilliams/json-fortran/issues/31)
- Indentation of output wrong for some nested objects
  [\#29](https://github.com/jacobwilliams/json-fortran/issues/29)
- Intel build broke
  [\#28](https://github.com/jacobwilliams/json-fortran/issues/28)
- CMake misbehaving when [ROBODoc](https://github.com/gumpu/ROBODoc)
  absent
  [\#26](https://github.com/jacobwilliams/json-fortran/issues/26)
- Differentiate internal (low level, private) procedures/types/etc
  from public API in documentation
  [\#21](https://github.com/jacobwilliams/json-fortran/issues/21)

**Merged pull requests:**

- [CMake](http://www.cmake.org) Fixes for issues
  [\#31](https://github.com/jacobwilliams/json-fortran/issues/31) and
  [\#26](https://github.com/jacobwilliams/json-fortran/issues/26).
  [\#32](https://github.com/jacobwilliams/json-fortran/pull/32)
  ([zbeekman](https://github.com/zbeekman))



### [2.0.0](https://github.com/jacobwilliams/json-fortran/tree/2.0.0) (2014-12-27)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/1.0.0...2.0.0)
or [Download v2.0.0](https://github.com/jacobwilliams/json-fortran/releases/tag/2.0.0)

**Enhancements:**
- Significant changes to the API including new procedures
- Code re-factoring
- SCons build added thanks to [@bruceravel](https://github.com/bruceravel)

**Fixed issues:**

- enable wiki contributions from the public
  [\#23](https://github.com/jacobwilliams/json-fortran/issues/23)



### [1.0.0](https://github.com/jacobwilliams/json-fortran/tree/1.0.0) (2014-06-23)

[Download v1.0.0](https://github.com/jacobwilliams/json-fortran/releases/tag/1.0.0)

**Enhancements:**

- Validate hex values in strings
  [\#14](https://github.com/jacobwilliams/json-fortran/issues/14)
- Add [CMake](http://www.cmake.org) build infrastructure
  [\#6](https://github.com/jacobwilliams/json-fortran/pull/6)
  (pull request from [zbeekman](https://github.com/zbeekman))
- [CMake](http://www.cmake.org) build now builds the example program
  and runs tests
  [\#9](https://github.com/jacobwilliams/json-fortran/issues/9)
- Added [CMake](http://www.cmake.org) logic to build
  [ROBODoc](https://github.com/gumpu/ROBODoc) documentation
  [\#12](https://github.com/jacobwilliams/json-fortran/pull/12) (pull
  request from [zbeekman](https://github.com/zbeekman))
- Upload [ROBODoc](https://github.com/gumpu/ROBODoc) documentation to
  gh-pages project page
  [\#7](https://github.com/jacobwilliams/json-fortran/issues/7)
- Add [CMake](http://www.cmake.org) testing infrastructure
  [\#18](https://github.com/jacobwilliams/json-fortran/pull/18)
  ([zbeekman](https://github.com/zbeekman))
- Implement better support for Intel compiler,
  [DPD200247629](https://software.intel.com/en-us/forums/topic/405706)
  `associate` construct work around, via
  [PR \#2](https://github.com/jacobwilliams/json-fortran/pull/2)
  ([zbeekman](https://github.com/zbeekman))

**Fixed issues:**

- Unit tests fail when compiled with GFortran
  [\#19](https://github.com/jacobwilliams/json-fortran/issues/19)
- [CMake](http://www.cmake.org) outputs of
  [ROBODoc](https://github.com/gumpu/ROBODoc) must be specified in
  CMakeLists.txt manually
  [\#17](https://github.com/jacobwilliams/json-fortran/issues/17)
- Invalid JSON input files (`test1.json`, `test5.json`)
  [\#10](https://github.com/jacobwilliams/json-fortran/issues/10),
  fixed by
  [PR \#11](https://github.com/jacobwilliams/json-fortran/pull/11)
  ([zbeekman](https://github.com/zbeekman))
- `char()` is processor dependent, use `achar()`
  [\#20](https://github.com/jacobwilliams/json-fortran/pull/20)
  (pull request from [zbeekman](https://github.com/zbeekman))
- Bug-fix for [ROBODoc](https://github.com/gumpu/ROBODoc) documentation
  generation with [CMake](http://www.cmake.org)
  [\#16](https://github.com/jacobwilliams/json-fortran/pull/16)
  (pull request from [zbeekman](https://github.com/zbeekman))
- Change library file from `libjson` to `libjsonfortran`
[\#4](https://github.com/jacobwilliams/json-fortran/issues/4)



\* *This Change Log was generated with help from
[github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
