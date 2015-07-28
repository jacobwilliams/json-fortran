project: JSON-Fortran
favicon: ./media/json-fortran-32x32.png
project_dir: ./src
output_dir: ./doc
media_dir: ./media
project_github: https://github.com/jacobwilliams/json-fortran
project_download: https://github.com/jacobwilliams/json-fortran/releases/latest
summary: JSON-Fortran -- A Fortran 2008 JSON API
author: Jacob Williams
github: https://github.com/jacobwilliams
website: http://degenerateconic.com
twitter: https://twitter.com/degenerateconic
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
exclude_dir: tests
exclude_dir: introspection
display: public
         protected
         private
source: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty(smart_quotes=False)

[TOC]

# Brief description

A user-friendly and object-oriented API for reading and writing JSON files, written in
modern Fortran (Fortran 2003+).  The source code is a single Fortran
module file
([json_module.F90](|url|/sourcefile/json_module.f90.html)).

# License

The JSON-Fortran source code and related files and documentation are
distributed under a permissive free software license (BSD-style).  See
the
[LICENSE](|url|/page/development-resources/LICENSE.html)
file for more details.

# Official Releases

The **current stable release** is **{!__VERSION__!}** and can be [downloaded
on GitHub](https://github.com/jacobwilliams/json-fortran/releases/latest)
or installed with [Homebrew](http://brew.sh) on Mac OSX.

Browse the documentation for current and past releases
[here](|url|/page/releases/index.html) or download the latest official
release
[here](https://github.com/jacobwilliams/json-fortran/releases/latest).

# Miscellaneous

* For more information about JSON, see: <http://www.json.org/>

*[API]: Application Programmable Interface
*[JSON]: JavaScript Object Notation
