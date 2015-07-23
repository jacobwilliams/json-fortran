project: json-fortran
favicon: ./media/json-fortran-32x32.png
project_dir: ./src
macro: USE_UCS4
output_dir: ./doc
media_dir: ./media
project_github: https://github.com/jacobwilliams/json-fortran
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
modern Fortran (Fortran 2003+).  The source code is a single Fortran module file ([json_module.F90](https://github.com/jacobwilliams/json-fortran/blob/master/src/json_module.F90)).

# License

The json-fortran source code and related files and documentation are distributed under a permissive free software license (BSD-style).  See the [LICENSE](https://raw.githubusercontent.com/jacobwilliams/json-fortran/master/LICENSE) file for more details.

# Miscellaneous

* For more information about JSON, see: <http://www.json.org/>

*[API]: Application Programmable Interface
*[JSON]: JavaScript Object Notation
