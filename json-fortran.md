---
project: JSON-Fortran
summary: JSON-Fortran -- A Fortran 2008 JSON API
author: Jacob Williams
src_dir: ./src
output_dir: ./doc
media_dir: ./media
exclude_dir: ./src/tests
             ./src/tests/introspection
favicon: ./media/json-fortran-32x32.png
project_github: https://github.com/jacobwilliams/json-fortran
project_download: https://github.com/jacobwilliams/json-fortran/releases/latest
github: https://github.com/jacobwilliams
website: http://degenerateconic.com
twitter: https://twitter.com/degenerateconic
preprocessor: {!.PREPROCESSOR!}
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         protected
         private
source: true
graph: true
sort: alpha
coloured_edges: true
extra_filetypes: .inc !
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            ifcore:https://software.intel.com/en-us/node/525900
md_extensions: markdown.extensions.toc(anchorlink=False)
               markdown.extensions.smarty(smart_quotes=False)
---

--------------------

[TOC]

Brief description
-----------------

A user-friendly, thread-safe, and object-oriented API for reading and writing [JSON](http://json.org) files, written in modern Fortran.

License
-------

The JSON-Fortran source code and related files and documentation are
distributed under a permissive free software license (BSD-style).  See
the
[LICENSE](http://jacobwilliams.github.io/json-fortran/page/development-resources/LICENSE.html)
file for more details.

Official Releases
-----------------

The **current stable release** is **{!.VERSION!}** and can be [downloaded
on GitHub](https://github.com/jacobwilliams/json-fortran/releases/latest)
or installed with [Homebrew](http://brew.sh) on Mac OSX. The
documentation for the current version, **{!.VERSION!}**, can be
found [here](http://jacobwilliams.github.io/json-fortran/{!.VERSION!}/index.html)[^1], and a
list of changes from the previous version are
[here](http://jacobwilliams.github.io/json-fortran/page/releases/index.html#change-log).

A list of all past releases, links to their documentation, and the
change log can be found on the
[releases page](http://jacobwilliams.github.io/json-fortran/page/releases/index.html).

Miscellaneous
-------------

* For more information about JSON, see: <http://www.json.org/>

*[API]: Application Programming Interface: a set of routines, protocols, and tools for building software applications
*[JSON]: JavaScript Object Notation: A human friendly syntax for storing and exchanging data
*[current stable release]: {!.VERSION!}
*[latest stable release]: {!.VERSION!}

[^1]:
    Documentation for a particular release does not contain links
    back to *general* documentation; use the browser's back button to
    navigate back to <http://jacobwilliams.github.io/json-fortran/>
