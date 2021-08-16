# Contributing to JSON-Fortran

Looking to contribute something to [JSON-Fortran](https://github.com/jacobwilliams/json-fortran)? **Here's how you can help.**

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

  - [Key Branches](#key-branches)
  - [Filing issues](#filing-issues)
  - [Outstanding Work](#outstanding-work)
  - [Pull Requests](#pull-requests)
  - [Coding Standards](#coding-standards)

<!-- markdown-toc end -->

## Key Branches

- `master` is the latest, development version and all efforts should be made to keep it stable.

[top](#contributing-to-json-fortran)
## Filing issues

- Before filing a new [issue](https://github.com/jacobwilliams/json-fortran/issues), please perform a search to see if that issue has already been filed by someone else, and whether or not a solution exists. If you are experiencing the same issue as one that's already posted, please leave any additional comments and information under the existing issue. If your issue is related to a previous issue, but substantively different, file a new issue and include a mention of the related issue in text, using Jib's `#<issue-number>` syntax.
- When filing an issue please try to include all of the following information
  1. Problem description: What behavior are you seeing that you think is erroneous
  1. Installation method: Did you build the code with one of the included build scripts? Some other way? Install from a binary package?
  1. What type of system you are on: E.g., 64 bit Intel Mac OS X 10.10.2 (Yosemite) or x86_64 Ubuntu 14.04 LTS (Trusty Tahr)
  1. If applicable, what compiler you used, and any non-standard options or configurations that were used.
  1. All steps required to reproduce the problem

[top](#contributing-to-json-fortran)
## Outstanding Work

- Take a look at the [issues](https://github.com/jacobwilliams/json-fortran/issues) to see if there is an issue you'd like to help address. [Issues](https://github.com/jacobwilliams/json-fortran/issues) with the [ready label](https://github.com/jacobwilliams/json-fortran/issues?q=is%3Aopen+is%3Aissue+label%3A%22ready%22) are issues that are ready to be dealt with. (i.e., They are not blocked by other dependencies and are higher priority.)
- Increasing the test coverage is another helpful way to
  contribute. Please take a look at
  [the test coverage page](https://jacobwilliams.github.io/json-fortran/page/development-resources/json_module.F90.gcov.html)
  for a list of uncovered procedures, and at the
  [Codecov.io coverage status](https://codecov.io/gh/jacobwilliams/json-fortran)
  to find procedures and lines of code that are still uncovered by the
  [unit tests](https://github.com/jacobwilliams/json-fortran/tree/master/src/tests). It
  is worth pointing out that the majority of uncovered procedures are
  internal, overloaded procedures and can only be exercised by calling
  the corresponding generic procedure with input arguments that will
  resolve to the specific procedure in question. Let's keep the
  following graph trending upwards!
  ![Codecov.io](https://codecov.io/gh/jacobwilliams/json-fortran/branch/master/graphs/commits.svg)

[top](#contributing-to-json-fortran)
## Pull Requests

- Try not to pollute your pull request with unintended changes--keep them simple and small
- Pull requests should address one issue at a time, and each commit should be a set of self contained, related changes. If you forget something in a commit, please use `git rebase -i <ref>^` to amend and/or squash erroneous commits. Here `<ref>` is the reference to to oldest commit needing to be modified (SHA, or `HEAD~4`, etc.)
- Each commit should compile, and ideally pass the tests. Very complicated new features or fixes, may have commits that don't pass tests, if otherwise the commit history would include far to many changes in any given commit. Use an interactive rebase to fix any of these issues, as described above.
- Each commit should have a concise, descriptive message following the
  guidelines laid out
  [here](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
- Make sure to document your changes in the
  [CHANGELOG](https://github.com/jacobwilliams/json-fortran/blob/master/CHANGELOG.md)
  under the
  ['unreleased'](https://github.com/jacobwilliams/json-fortran/blob/master/CHANGELOG.md#unreleased)
  heading.
- Pull requests should always be based on the upstream master,
`jacobwilliams/json-fortran:master`. Please `rebase` your branch on top
of the latest upstream master. Assuming you are on your branch and you've added the upstream remote by running something like:
```
git remote add upstream git://github.com/jacobwilliams/json-fortran.git
```
You can accomplish this by running:
```
git rebase upstream/master
```
- Create a branch in your fork with a descriptive name that also includes the [issue number](https://github.com/jacobwilliams/json-fortran/issues), if applicable. For example, after forking the repo, you can run something like `git checkout -b Unicode-support-issue-35` before starting work on [issue #35 : Unicode support](https://github.com/jacobwilliams/json-fortran/issues/35)
- When you're content with your changes, your commits are clean, self contained, with concise descriptive messages, and your changes compile and pass the tests, submit a pull request. We will review your changes, and may ask for certain modifications to be made.
- Pull requests are tested by our [GitHub Actions](https://github.com/jacobwilliams/json-fortran/actions) continuous integration system, and any errors uncovered will need to be fixed before the pull request can be merged into master.
- The JSON-Fortran library and associated documentation is released under a BSD style [license](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE).  By submitting a pull request, you are agreeing to release your code under the same license.  Note that code with GPL or other "copyleft" style licenses will not be accepted.

[top](#contributing-to-json-fortran)
## Coding Standards

- Each commit should address a single logical change and code base transformation.
- Each commit **must** compile.
- Each commit should pass the tests unless the feature being implemented or bug being fixed requires extensive changes that would result in a commit with too many different changes.
- Each pull request should ensure that the proper unit tests have
  been added to cover at least 90% of new or changed code and that the
  overall coverage continues to increase.
- New tests are added to the `src/tests` directory and are named
  `jf_test_<#>.[Ff]90`. Each test is a stand alone Fortran program
  which will automatically be compiled and linked against the
  JSON-Fortran library so long as it follows this naming
  convention. If any of the tests fail, the test program should exit
  with a non-zero return status, using the `stop 2` intrinsic
  statement. (The Fortran standard does not require the 'processor' to
  have or set a return value, but in practice all compilers respect
  this convention. An integer other than `2` may be used, so long as
  it is non-zero and supported by the processor.)
- No extraneous white spaces are allowed to be introduced at line endings and all non binary files should end with a single new line. Run `git config core.whitespace trailing-space,space-before-tab,blank-at-eol,blank-at-eof` to setup the whitespace rules from within your fork, and then check for white space errors with `git diff --check` to see if you have accidentally introduced white space errors before committing. (You can also enable the sample `pre-commit` hook that ships with git, to prevent you from committing changes that introduce white space errors. See [this stackoverflow question](http://stackoverflow.com/questions/591923/make-git-automatically-remove-trailing-whitespace-before-committing/28446440)) for some tips on preventing the introduction of whitespace errors.
- Please adhere to the code indentation and formatting as it currently exists, aligning common elements vertically, etc. Tab characters are not allowed. Indentations should be done with *4* space characters.
- *Do NOT* allow your editor to make a bunch of indentation or white space changes, that will introduce non-substantive changes on lines that you have not actually edited.
- The coding style is modern free-form Fortran, consistent with the Fortran 2008 standard.  Note that the two supported compilers (ifort and gfortran) do not currently include the entire Fortran 2008 standard. Therefore, only those language features supported by Gfortran 4.9 and Intel 13.1.0 are currently allowed.  This also means that previous versions of these compilers are not supported, and major changes to the code to support earlier compilers (or Fortran 95) will not be accepted.  At some point in the future (when compiler support has improved), all Fortran 2008 features will be allowed.
- All subroutines and functions *must* be properly documented.  This includes useful inline comments as well as comment blocks using the [FORD](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation) syntax.

[top](#contributing-to-json-fortran)
