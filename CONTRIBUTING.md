# Contributing to [json-fortran](README.md)

Looking to contribute something to [json-fortran](README.md)? **Here's how you can help.**

## Key Branches

- `master` is the latest, development version and all efforts should be made to keep it stable.

## Filing issues

- Before filing a new [issue](https://github.com/jacobwilliams/json-fortran/issues), please perform a search to see if that issue has already been filed by someone else, and whether or not a solution exists. If you are experiencing the same issue as one that's already posted, please leave any additional comments and information under the existing issue. If your issue is related to a previous issue, but substantively different, file a new issue and include a mention of the related issue in text, using GitHub's `#<issue-number>` syntax.
- When filing an issue please try to include all of the following information
  1. Problem description: What behavior are you seeing that you think is erroneous
  1. Installation method: Did you build the code with one of the included build scripts? Some other way? Install from a binary package?
  1. What type of system you are on: E.g., 64 bit Intel Mac OS X 10.10.2 (Yosemite) or x86_64 Ubuntu 14.04 LTS (Trusty Tahr)
  1. If applicable, what compiler you used, and any non-standard options or configurations that were used.
  1. All steps required to reproduce the problem

## Outstanding Work

- Take a look at the [issues](https://github.com/jacobwilliams/json-fortran/issues) to see if there is an issue you'd like to help address. [Issues](https://github.com/jacobwilliams/json-fortran/issues) with the [ready label](https://github.com/jacobwilliams/json-fortran/issues?q=is%3Aopen+is%3Aissue+label%3A%22ready%22) or in the [ready column on waffle.io](https://waffle.io/jacobwilliams/json-fortran) are issues that are ready to be dealt with. (i.e., They are not blocked by other dependencies and are higher priority.)

## Pull Requests

- Try not to pollute your pull request with unintended changes--keep them simple and small
- Pull requests should address one issue at a time, and each commit should be a set of self contained, related changes. If you forget something in a commit, please use `git rebase -i <ref>^` to amend and/or squash erroneous commits. Here `<ref>` is the reference to to oldest commit needing to be modified (SHA, or `HEAD~4`, etc.)
- Each commit should compile, and ideally pass the tests. Very complicated new features or fixes, may have commits that don't pass tests, if otherwise the commit history would include far to many changes in any given commit. Use an interactive rebase to fix any of these issues, as described above.
- Pull requests should always be based on the upstream master, jacobwilliams/json-fortran:master. Please `rebase` your branch on top of the latest upstream master. Assuming you've added the upstream remote by running something like:
```
git remote add upstream git://github.com/jacobwilliams/json-fortran.git
```
You can accomplish this by running:
```
git rebase upstream
```
- Create a branch in your fork with a descriptive name that also includes the [issue number](https://github.com/jacobwilliams/json-fortran/issues), if applicable. For example, after forking the repo, you can run something like `git checkout -b Unicode-support-issue-35` before starting work on [issue #35 : Unicode support](https://github.com/jacobwilliams/json-fortran/issues/35)
- When you're content with your changes, your commits are clean, self contained, with concise descriptive messages, and your changes compile and pass the tests, submit a pull request. We will review your changes, and may ask for certain modifications to be made.
- Pull requests are tested by our [travis-ci](https://travis-ci.org/jacobwilliams/json-fortran) continuous integration system, and any errors uncovered will need to be fixed before the pull request can be merged into master.
- The json-fortran library and associated documentation is released under a BSD style [license](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE).  By submitting a pull request, you are agreeing to release your code under the same license.  Note that code with GPL or other "copyleft" style licenses will not be accepted.

## Coding Standards

- Each commit should address a single logical change and code base transformation.
- Each commit **must** compile.
- Each commit should pass the tests unless the feature being implemented or bug being fixed requires extensive changes that would result in a commit with too many different changes.
- No extraneous white spaces are allowed to be introduced at line endings and all non binary files should end with a single new line. Run `git config core.whitespace trailing-space,space-before-tab,blank-at-eol,blank-at-eof` to setup the whitespace rules from within your fork, and then check for white space errors with `git diff --check` to see if you have accidentally introduced white space errors before committing. (You can also enable the sample `pre-commit` hook that ships with git, to prevent you from committing changes that introduce white space errors. See [this stackoverflow question](http://stackoverflow.com/questions/591923/make-git-automatically-remove-trailing-whitespace-before-committing/28446440)) for some tips on preventing the introduction of whitespace errors.
- Please adhere to the code indentation and formatting as it currently exists, aligning common elements vertically, etc. Tab characters are not allowed. Indentations should be done with *4* space characters.
- *Do NOT* allow your editor to make a bunch of indentation or white space changes, that will introduce non-substantive changes on lines that you have not actually edited.
- The coding style is modern free-form Fortran, consistent with the Fortran 2008 standard.  Note that the two supported compilers (ifort and gfortran) do not currently include the entire Fortran 2008 standard. Therefore, only those language features supported by Gfortran 4.9 and Intel 13.1.0 are currently allowed.  This also means that previous versions of these compilers are not supported, and major changes to the code to support earlier compilers (or Fortran 95) will not be accepted.  At some point in the future (when compiler support has improved), all Fortran 2008 features will be allowed.
- All subroutines and functions *must* be properly documented.  This includes useful inline comments as well as comment blocks using the [ROBODoc](http://rfsber.home.xs4all.nl/Robo/manual.html) syntax.
- For simplicity, json-fortran currently consists of one module file. It is not envisioned that it will ever need to expand to include multiple files (if it does, there would need to be a very good reason).