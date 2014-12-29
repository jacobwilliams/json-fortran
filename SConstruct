# -*- python -*-
#
#  SConstruct file for building json-fortran using scons
#
#  This builds using the same commands as are used in json-fortran's
#  "build.sh" script, but with proper dependency checking.  It also
#  deals gracefully with robodoc not being installed on the system.
#

from distutils.spawn import find_executable
import os
from os.path import join

env = Environment()

if env['FORTRAN'] == 'gfortran':
    env = Environment(F90FLAGS = '-O2 -fbacktrace -g -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008 -Jlib',)
elif env['FORTRAN'] == 'ifort':
    env = Environment(F90FLAGS = '-O2 -warn -stand f08 -diag-disable 7601 -traceback -module lib',)

src = join('src','json_module.f90')
obj = join('build','json_module.o')
ar  = join('lib','libjsonfortran.a')
mod = join('lib','json_module.mod')
exe = join('bin','json')

env.Library(ar, src,
            FORTRANMODDIR='lib', # this tells scons that we want the .mod file to be installed in to lib/
            LIBPREFIX='',        # this tells scons /not/ to assume that the mod file should be libjson_module.mod
        )


## this builds the example program and places it in bin/
env.Program(exe, join('src','json_example.f90'), LIBS=['jsonfortran',], LIBPATH=['lib'])

env.Requires(exe, mod)

## ------ installation ----------

if os.name == 'nt':
    # these might not be the right place for your Windows machine....
    libinstall = 'C:\MinGW\lib'
    docinstall = 'C:\MinGW\share\doc\json-fortran'
else:
    libinstall = '/usr/local/lib'
    docinstall = '/usr/local/share/doc/json-fortran'

env.Install(libinstall, [obj,ar,mod])
env.Alias('install', libinstall)


## ------ documentation ---------

if find_executable('robodoc'):

    docfiles = [join('documentation','json_example_f90.html'), 
                join('documentation','json_module_f90.html'),
                join('documentation','masterindex.html'),
                join('documentation','robo_classes.html'),   
                join('documentation','robo_functions.html'),
                join('documentation','robo_modules.html'),
                join('documentation','robo_sourcefiles.html'),
                join('documentation','robo_unittests.html'),
                join('documentation','robodoc.css'), 
                join('documentation','toc_index.html'), ]

    bld = Builder(action='robodoc --rc robodoc.rc --src src/ --doc documentation/ --multidoc --html'+
                  ' --ignore_case_when_linking --syntaxcolors --source_line_numbers --index --tabsize 4 --documenttitle jsonfortran --sections')
    env = Environment(BUILDERS = {'RoboDoc' : bld})
    env.RoboDoc(docfiles, src)
    env.Depends(docfiles, src)
    env.Install(docinstall, docfiles)
    env.Alias('install', docinstall)
