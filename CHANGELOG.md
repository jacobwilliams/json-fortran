# Change Log

**Table of Contents**

- [Change Log](#change-log)
    - [Unreleased](#unreleased)
    - [4.1.1 (2015-05-27)](#411-2015-05-27)
    - [4.1.0 (2015-05-05)](#410-2015-05-05)
    - [4.0.0 (2015-03-16)](#400-2015-03-16)
    - [3.1.0 (2015-02-28)](#310-2015-02-28)
    - [3.0.0 (2015-01-18)](#300-2015-01-18)
    - [2.0.0 (2014-12-27)](#200-2014-12-27)
    - [1.0.0 (2014-06-23)](#100-2014-06-23)

## [Unreleased](https://github.com/jacobwilliams/json-fortran/tree/HEAD)

[Full Changelog](https://github.com/jacobwilliams/json-fortran/compare/4.1.1...HEAD)

**Enhancements**
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
- Small FoBiS coverage fix
  [\#109](https://github.com/jacobwilliams/json-fortran/pull/109)
  ([zbeekman](https://github.com/zbeekman))



## [4.1.1](https://github.com/jacobwilliams/json-fortran/tree/4.1.1) (2015-05-27)

[Full Changelog](https://github.com/jacobwilliams/json-fortran/compare/4.1.0...4.1.1)

**Enhancements**

- Installation now possible with a [Homebrew](http://brew.sh)
  [package](http://braumeister.org/formula/json-fortran) on Mac OS X
- [CMake](http://cmake.org) install option to use a more traditional
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




## [4.1.0](https://github.com/jacobwilliams/json-fortran/tree/4.1.0) (2015-05-05)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/4.0.0...4.1.0)

**Enhancements:**

- Runtime speed improvements
  [\#75](https://github.com/jacobwilliams/json-fortran/issues/75)
  implemented in
  [PR \#93](https://github.com/jacobwilliams/json-fortran/pull/93)
  from [jacobwilliams](https://github.com/jacobwilliams)

**Merged pull requests:**

- Small formatting and addition of a unit test
  [\#95](https://github.com/jacobwilliams/json-fortran/pull/95)
  ([jacobwilliams](https://github.com/jacobwilliams))



## [4.0.0](https://github.com/jacobwilliams/json-fortran/tree/4.0.0) (2015-03-16)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/3.1.0...4.0.0)

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
- Small bugfix for [CMake](http://www.cmake.org) install
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



## [3.1.0](https://github.com/jacobwilliams/json-fortran/tree/3.1.0) (2015-02-28)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/3.0.0...3.1.0)

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
- Adopeted official contributing guidelines and added a
  `CONTRIBUTING.md`
  [\#47](https://github.com/jacobwilliams/json-fortran/issues/47)

**Fixed issues:**

- Build script (`build.sh`) failing with Intel's ifort
  [\#53](https://github.com/jacobwilliams/json-fortran/issues/53),
  fixed by
  [PR \#54](https://github.com/jacobwilliams/json-fortran/pull/54)
  (from [zbeekman](https://github.com/zbeekman))
- Segfaults and unexpexted behavior due to optional dummy arguments
  being dereferenced outside of `if (present(…))`
  [\#52](https://github.com/jacobwilliams/json-fortran/issues/52)
- Documentation not being produced for tests because tests directory
  should be subdirectory of src
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



## [3.0.0](https://github.com/jacobwilliams/json-fortran/tree/3.0.0) (2015-01-18)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/2.0.0...3.0.0)

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



## [2.0.0](https://github.com/jacobwilliams/json-fortran/tree/2.0.0) (2014-12-27)

[Complete Changeset](https://github.com/jacobwilliams/json-fortran/compare/1.0.0...2.0.0)

**Enhancements:**
- Significant changes to the API including new procedures
- Code refactoring
- SCons build added thanks to [@bruceravel](https://github.com/bruceravel)

**Fixed issues:**

- enable wiki contributions from the public
  [\#23](https://github.com/jacobwilliams/json-fortran/issues/23)



## [1.0.0](https://github.com/jacobwilliams/json-fortran/tree/1.0.0) (2014-06-23)

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
- Invalid json input files (`test1.json`, `test5.json`)
  [\#10](https://github.com/jacobwilliams/json-fortran/issues/10),
  fixed by
  [PR \#11](https://github.com/jacobwilliams/json-fortran/pull/11)
  ([zbeekman](https://github.com/zbeekman))
- `char()` is processor dependent, use `achar()`
  [\#20](https://github.com/jacobwilliams/json-fortran/pull/20)
  (pull request from [zbeekman](https://github.com/zbeekman))
- Bugfix for [ROBODoc](https://github.com/gumpu/ROBODoc) documentation
  generation with [CMake](http://www.cmake.org)
  [\#16](https://github.com/jacobwilliams/json-fortran/pull/16)
  (pull request from [zbeekman](https://github.com/zbeekman))
- Change library file from `libjson` to `libjsonfortran`
[\#4](https://github.com/jacobwilliams/json-fortran/issues/4)



\* *This Change Log was generated with help from
[github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
