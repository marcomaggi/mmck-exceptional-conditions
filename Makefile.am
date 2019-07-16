## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-exceptional-conditions.texi
doc_mmck_exceptional_conditions_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-exceptional-conditions.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-exceptional-conditions.html/
	@: > doc/mmck-exceptional-conditions.html/$(am__dirstamp)

doc/mmck-exceptional-conditions.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-exceptional-conditions.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-exceptional-conditions.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-exceptional-conditions.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-exceptional-conditions.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-exceptional-conditions.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-exceptional-conditions.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_EXCEPTIONAL_CONDITIONS_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.exceptional-conditions.import.$(MMCK_SOEXT)
#
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the  shared library "(mmck exceptional-conditions)".  We  need to build
# and install all of them.
#
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	=

# This is the "(mmck exceptional-conditions)" shared library file pathname.
#
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_SHARED_LIBS	= \
	lib/mmck.exceptional-conditions.$(MMCK_SOEXT)		\
	lib/mmck.exceptional-conditions.helpers.$(MMCK_SOEXT)

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		=


#### library (mmck exceptional-conditions): module version

MMCK_EXCEPTIONAL_CONDITIONS_MODULES		+= lib/mmck.exceptional-conditions.version.scm
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	+= lib/mmck.exceptional-conditions.version.$(OBJEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	+= lib/mmck.exceptional-conditions.version.import.scm
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS		+= lib/mmck.exceptional-conditions.version.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	+= lib/mmck.exceptional-conditions.version.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		+= \
	lib/mmck.exceptional-conditions.version.temp				\
	lib/mmck.exceptional-conditions.version.lock				\
	lib/mmck.exceptional-conditions.version.stamp

# A compiler  invokation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.exceptional-conditions.version.stamp: lib/mmck.exceptional-conditions.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.exceptional-conditions.version.temp
	@touch lib/mmck.exceptional-conditions.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.exceptional-conditions.version.$(OBJEXT) $(<) -prologue lib/config.scm
	@$(MV) mmck.exceptional-conditions.version.import.scm lib/mmck.exceptional-conditions.version.import.scm
	@mv -f lib/mmck.exceptional-conditions.version.temp $@

lib/mmck.exceptional-conditions.version.import.scm lib/mmck.exceptional-conditions.version.$(OBJEXT): lib/mmck.exceptional-conditions.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.exceptional-conditions.version.lock lib/mmck.exceptional-conditions.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.exceptional-conditions.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.exceptional-conditions.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.exceptional-conditions.version.stamp; \
	    $$dry rmdir lib/mmck.exceptional-conditions.version.lock; \
	  else \
	    while test -d lib/mmck.exceptional-conditions.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.exceptional-conditions.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.exceptional-conditions.version.import.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck exceptional-conditions): module helpers

MMCK_EXCEPTIONAL_CONDITIONS_MODULES		+= lib/mmck.exceptional-conditions.helpers.scm
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	+= lib/mmck.exceptional-conditions.helpers.$(OBJEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	+= lib/mmck.exceptional-conditions.helpers.import.scm
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS		+= lib/mmck.exceptional-conditions.helpers.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	+= lib/mmck.exceptional-conditions.helpers.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		+= \
	lib/mmck.exceptional-conditions.helpers.temp				\
	lib/mmck.exceptional-conditions.helpers.lock				\
	lib/mmck.exceptional-conditions.helpers.stamp

lib/mmck.exceptional-conditions.helpers.stamp: lib/mmck.exceptional-conditions.helpers.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.exceptional-conditions.helpers.temp
	@touch lib/mmck.exceptional-conditions.helpers.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.exceptional-conditions.helpers.$(OBJEXT) $(<) -prologue lib/config.scm
	@$(MV) mmck.exceptional-conditions.helpers.import.scm lib/mmck.exceptional-conditions.helpers.import.scm
	@mv -f lib/mmck.exceptional-conditions.helpers.temp $@

lib/mmck.exceptional-conditions.helpers.import.scm lib/mmck.exceptional-conditions.helpers.$(OBJEXT): lib/mmck.exceptional-conditions.helpers.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.exceptional-conditions.helpers.lock lib/mmck.exceptional-conditions.helpers.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.exceptional-conditions.helpers.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.exceptional-conditions.helpers.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.exceptional-conditions.helpers.stamp; \
	    $$dry rmdir lib/mmck.exceptional-conditions.helpers.lock; \
	  else \
	    while test -d lib/mmck.exceptional-conditions.helpers.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.exceptional-conditions.helpers.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.exceptional-conditions.helpers.import.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.helpers.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)

## --------------------------------------------------------------------

# We build this libraries to use  its facilities at expand-time in the
# other modules; we would like not to install it!
#
# But this  library is also needed  to run the test  suite; this means
# the  rule "make  installcheck" will  work  only if  we install  this
# library too.  So we do it.  In the end it is a very small library...
#
# Maybe we could do some trick with GNU Automake's substitution symbol
# @MAINTAINER_MODE_TRUE@ which (it is my understanding):
#
# * Expands to the empty string when maintainer mode is enabled.
#
# * Expands to the comment character when maintainer mode is disabled.
#
# With this we could include/exclude  the installation of this library
# depending on the mode.  We are not doing it right now.
#
# Life is hard!  (Marco Maggi; May 30, 2019)
#
lib/mmck.exceptional-conditions.helpers.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.helpers.$(OBJEXT)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### library (mmck exceptional-conditions): module condition-objects

MMCK_EXCEPTIONAL_CONDITIONS_MODULES		+= lib/mmck.exceptional-conditions.condition-objects.scm
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	+= lib/mmck.exceptional-conditions.condition-objects.$(OBJEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	+= lib/mmck.exceptional-conditions.condition-objects.import.scm
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS		+= lib/mmck.exceptional-conditions.condition-objects.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	+= lib/mmck.exceptional-conditions.condition-objects.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		+= \
	lib/mmck.exceptional-conditions.condition-objects.temp	\
	lib/mmck.exceptional-conditions.condition-objects.lock	\
	lib/mmck.exceptional-conditions.condition-objects.stamp

MMCK_EXCEPTIONAL_CONDITIONS_CONDITION_OBJECTS_DEPS = \
	lib/mmck.exceptional-conditions.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)						\
	\
	lib/mmck.exceptional-conditions.helpers.$(MMCK_SOEXT)

lib/mmck.exceptional-conditions.condition-objects.stamp: \
		lib/mmck.exceptional-conditions.condition-objects.scm \
		$(MMCK_EXCEPTIONAL_CONDITIONS_CONDITION_OBJECTS_DEPS)
	@rm -f lib/mmck.exceptional-conditions.condition-objects.temp
	@touch lib/mmck.exceptional-conditions.condition-objects.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.exceptional-conditions.condition-objects.$(OBJEXT) $(<)
	@$(MV) mmck.exceptional-conditions.condition-objects.import.scm lib/mmck.exceptional-conditions.condition-objects.import.scm
	@mv -f lib/mmck.exceptional-conditions.condition-objects.temp $@

lib/mmck.exceptional-conditions.condition-objects.import.scm lib/mmck.exceptional-conditions.condition-objects.$(OBJEXT): \
		lib/mmck.exceptional-conditions.condition-objects.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.exceptional-conditions.condition-objects.lock lib/mmck.exceptional-conditions.condition-objects.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.exceptional-conditions.condition-objects.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.exceptional-conditions.condition-objects.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.exceptional-conditions.condition-objects.stamp; \
	    $$dry rmdir lib/mmck.exceptional-conditions.condition-objects.lock; \
	  else \
	    while test -d lib/mmck.exceptional-conditions.condition-objects.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.exceptional-conditions.condition-objects.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.exceptional-conditions.condition-objects.import.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.condition-objects.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck exceptional-conditions): module handlers

MMCK_EXCEPTIONAL_CONDITIONS_MODULES		+= lib/mmck.exceptional-conditions.handlers.scm
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	+= lib/mmck.exceptional-conditions.handlers.$(OBJEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	+= lib/mmck.exceptional-conditions.handlers.import.scm
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS		+= lib/mmck.exceptional-conditions.handlers.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	+= lib/mmck.exceptional-conditions.handlers.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		+= \
	lib/mmck.exceptional-conditions.handlers.temp	\
	lib/mmck.exceptional-conditions.handlers.lock	\
	lib/mmck.exceptional-conditions.handlers.stamp

MMCK_EXCEPTIONAL_CONDITIONS_HANDLERS_DEPS = \
	lib/mmck.exceptional-conditions.helpers.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)						\
	\
	lib/mmck.exceptional-conditions.helpers.$(MMCK_SOEXT)

lib/mmck.exceptional-conditions.handlers.stamp: \
		lib/mmck.exceptional-conditions.handlers.scm \
		$(MMCK_EXCEPTIONAL_CONDITIONS_HANDLERS_DEPS)
	@rm -f lib/mmck.exceptional-conditions.handlers.temp
	@touch lib/mmck.exceptional-conditions.handlers.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.exceptional-conditions.handlers.$(OBJEXT) $(<)
	@$(MV) mmck.exceptional-conditions.handlers.import.scm lib/mmck.exceptional-conditions.handlers.import.scm
	@mv -f lib/mmck.exceptional-conditions.handlers.temp $@

lib/mmck.exceptional-conditions.handlers.import.scm lib/mmck.exceptional-conditions.handlers.$(OBJEXT): \
		lib/mmck.exceptional-conditions.handlers.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.exceptional-conditions.handlers.lock lib/mmck.exceptional-conditions.handlers.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.exceptional-conditions.handlers.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.exceptional-conditions.handlers.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.exceptional-conditions.handlers.stamp; \
	    $$dry rmdir lib/mmck.exceptional-conditions.handlers.lock; \
	  else \
	    while test -d lib/mmck.exceptional-conditions.handlers.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.exceptional-conditions.handlers.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.exceptional-conditions.handlers.import.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.handlers.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck exceptional-conditions): main module

MMCK_EXCEPTIONAL_CONDITIONS_MODULES		+= lib/mmck.exceptional-conditions.scm
MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS	+= lib/mmck.exceptional-conditions.$(OBJEXT)
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES	+= lib/mmck.exceptional-conditions.import.scm
MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS	+= lib/mmck.exceptional-conditions.import.$(MMCK_SOEXT)
MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES		+= \
	lib/mmck.exceptional-conditions.temp				\
	lib/mmck.exceptional-conditions.lock				\
	lib/mmck.exceptional-conditions.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.exceptional-conditions.stamp: lib/mmck.exceptional-conditions.scm $(MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.exceptional-conditions.temp
	@touch lib/mmck.exceptional-conditions.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.exceptional-conditions.$(OBJEXT) $(<)
	@$(MV) mmck.exceptional-conditions.import.scm lib/mmck.exceptional-conditions.import.scm
	@mv -f lib/mmck.exceptional-conditions.temp $@

lib/mmck.exceptional-conditions.import.scm lib/mmck.exceptional-conditions.$(OBJEXT): lib/mmck.exceptional-conditions.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.exceptional-conditions.lock lib/mmck.exceptional-conditions.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.exceptional-conditions.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.exceptional-conditions.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.exceptional-conditions.stamp; \
	    $$dry rmdir lib/mmck.exceptional-conditions.lock; \
	  else \
	    while test -d lib/mmck.exceptional-conditions.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.exceptional-conditions.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.exceptional-conditions.import.$(MMCK_SOEXT): lib/mmck.exceptional-conditions.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck exceptional-conditions): wrapping up

EXTRA_DIST	+= $(MMCK_EXCEPTIONAL_CONDITIONS_MODULES)

CLEANFILES	+= \
	$(MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_SHARED_LIBS)	\
	$(MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS)		\
	$(MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS)	\
	$(MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SOURCES)		\
	$(MMCK_EXCEPTIONAL_CONDITIONS_TEMP_FILES)

lib/mmck.exceptional-conditions.$(MMCK_SOEXT): $(MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_EXCEPTIONAL_CONDITIONS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_EXCEPTIONAL_CONDITIONS_RUNTIME_SHARED_LIBS)

# These variables are parsed by GNU Automake to build the main library
# targets and install them under the directory $(MMUX_CHICKEN_LIBDIR).
#
# We are shamelessly cheating by  installing them using GNU Automake's
# primary  suffix  "_SCRIPTS"; these  are  not  scripts!  Scripts  are
# installed with the permissions 0755, which, for some reason, is what
# we want  for shared libraries.
#
# We  could use  the  primary suffix  "_DATA", but  we  would get  the
# permissions 0644.
#
sharedlibdir		= $(MMUX_CHICKEN_LIBDIR)
sharedlib_SCRIPTS	= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_SHARED_LIBS)


#### interface to "make check"
#
# Read "Parallel Test Harness" in the documentation of GNU Automake to
# understand how to use this interface for "make check".
#

# The list of object files.  We will store it in CLEANFILES.
#
MMCK_CHECK_OBJECTS	=

# The list of test programs.  We will set TESTS to it.
#
MMCK_CHECK_PROGS	=

# Dependencies common to all the test programs.
#
MMCK_CHECK_DEPS		+= $(MMCK_ALL_RUNTIME_SHARED_LIBS) $(MMCK_ALL_IMPORT_LIBS)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-version.scm
MMCK_CHECK_OBJECTS	+= tests/test-version.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-version.exe

tests/test-version.$(OBJEXT): tests/test-version.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-version.exe: tests/test-version.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-condition-objects.scm
MMCK_CHECK_OBJECTS	+= tests/test-condition-objects.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-condition-objects.exe

tests/test-condition-objects.$(OBJEXT): tests/test-condition-objects.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-condition-objects.exe: tests/test-condition-objects.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)

## --------------------------------------------------------------------

EXTRA_DIST		+= tests/test-errors.scm
MMCK_CHECK_OBJECTS	+= tests/test-errors.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-errors.exe

tests/test-errors.$(OBJEXT): tests/test-errors.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-errors.exe: tests/test-errors.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
