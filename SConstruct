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
from os.path import join, basename
import glob
import subprocess
import sys

env = Environment()

if env['FORTRAN'] == 'gfortran':
    env = Environment(F90FLAGS = '-O2 -fbacktrace -g -Wall -Wextra -Wno-maybe-uninitialized -pedantic -std=f2008 -J',)
elif env['FORTRAN'] == 'ifort':
    env = Environment(F90FLAGS = '-O2 -warn -stand f08 -diag-disable 7601 -traceback -module lib',)

src = join('src','json_module.F90')
ar  = join('lib','libjsonfortran'+env['LIBSUFFIX'])
sl  = join('lib','libjsonfortran'+env['SHLIBSUFFIX'])
mod = join('lib','json_module.mod')  ## FORTRANMODSUFFIX



## make a list of test files and their resulting executables
tests = []
obj   = []
exe   = []
here = os.getcwd()
os.chdir(join('src', 'tests'))
for f in sorted(glob.glob("*")):
    if not f.endswith('f90'): continue
    tests.append(join('src', 'tests', f))
    e = f[:-4]
    obj.append(join('src','tests',e+env['OBJSUFFIX']))
    exe.append(join('bin',e+env['PROGSUFFIX']))
os.chdir(here)


## make the static library
a=env.Library(ar, src,
              FORTRANMODDIR='lib', # this tells scons that we want the .mod file to be installed in to lib/
              LIBPREFIX='',        # this tells scons /not/ to assume that the mod file should be libjson_module.mod
          )

## make the shared object library (or dll or what have you....)
#env.SharedLibrary(sl, src,
#                  FORTRANMODDIR='lib', # this tells scons that we want the .mod file to be installed in to lib/
#                  LIBPREFIX='',        # this tells scons /not/ to assume that the mod file should be libjson_module.mod
#              )


## this builds the example programs and places them in bin/
for i, t in enumerate(tests):
    e=env.Program(exe[i], t, LIBS=['jsonfortran',], LIBPATH=['lib'], FORTRANMODDIR='lib')
env.Requires(exe, [mod]+obj)

## ------ testing ----------

if "test" in COMMAND_LINE_TARGETS:
    os.chdir('bin')
    FNULL = open(os.devnull, 'w')
    count = 0
    which = []
    open('tests.ran', 'w').close()
    for e in exe:
        ee = basename(e)
        this = subprocess.check_call(join(os.getcwd(), ee), stdout=FNULL, stderr=FNULL)
        count = count + this
        if this: which.append(ee)
    if count > 0:
        print "failing: " + ", ".join(which)
    else:
        print 'all tests passed'
    sys.exit()


## ------ installation ----------

if os.name == 'nt':
    # these might not be the right place for your Windows machine....
    libinstall = 'C:\MinGW\lib'
    docinstall = 'C:\MinGW\share\doc\json-fortran'
else:
    libinstall = '/usr/local/lib'
    docinstall = '/usr/local/share/doc/json-fortran'

env.Install(libinstall, [sl,ar,mod])
env.Alias('install', libinstall)


## ------ documentation ---------

if find_executable('robodoc'):

    docfiles = glob.glob('documentation/*.*')   #main directory
    docfiles.append(glob.glob('documentation/tests/*.*')) #tests directory

    bld = Builder(action='robodoc --rc robodoc.rc --src src/ --doc documentation/ --documenttitle jsonfortran')
    env = Environment(BUILDERS = {'RoboDoc' : bld})
    env.RoboDoc(docfiles, src)
    env.Depends(docfiles, src)
    env.Install(docinstall, docfiles)
    env.Alias('install', docinstall)
