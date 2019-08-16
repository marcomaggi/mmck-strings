## Process this file with automake to produce Makefile.in

include meta/automake/mmck-rules.make


#### documentation

EXTRA_DIST		+= doc/mmck-chicken-docs.style.css
CLEANFILES		+= doc/mmck-chicken-docs.css
AM_MAKEINFOHTMLFLAGS	+= --css-ref=mmck-chicken-docs.css

info_TEXINFOS		= doc/mmck-strings.texi
doc_mmck_strings_TEXINFOS = \
	doc/macros.texi			\
	doc/lgpl-3.0.texi		\
	doc/fdl-1.3.texi

## --------------------------------------------------------------------

doc/mmck-strings.html/$(am__dirstamp): doc/$(am__dirstamp)
	@$(MKDIR_P) doc/mmck-strings.html/
	@: > doc/mmck-strings.html/$(am__dirstamp)

doc/mmck-strings.html/mmck-chicken-docs.css: doc/mmck-chicken-docs.style.css doc/mmck-strings.html/$(am__dirstamp)
	$(INSTALL) -m 0444 "$(top_srcdir)/doc/mmck-chicken-docs.style.css" "$(builddir)/doc/mmck-strings.html/mmck-chicken-docs.css"

## --------------------------------------------------------------------

html-local: doc/mmck-strings.html/mmck-chicken-docs.css

install-html-local:
	$(MKDIR_P) "$(DESTDIR)$(htmldir)/mmck-strings.html/"
	$(INSTALL) -m 0444 $(builddir)/doc/mmck-strings.html/mmck-chicken-docs.css "$(DESTDIR)$(htmldir)/mmck-strings.html/"


#### library build targets

# All the source files ".scm" of the extension library.
#
MMCK_STRINGS_MODULES		=

# All  the object  files ".o"  that must  be linked  in the  extension
# library.
#
MMCK_STRINGS_RUNTIME_OBJECTS	=

# All  the  source files  ".import.scm"  of  all the  modules'  import
# libraries.
#
MMCK_STRINGS_IMPORT_SOURCES	=

# The  module's  import  libraries ".import.$(MMCK_SOEXT)"  needed  to
# compile main  module's object  file.  It does  not include  the main
# import library:
#
#   lib/mmck.strings.import.$(MMCK_SOEXT)
#
MMCK_STRINGS_IMPORT_DEPS		=

# The import  libraries ".import.$(MMCK_SOEXT)" needed to  compile new
# code using the  shared library "(mmck strings)".  We  need to build
# and install all of them.
#
MMCK_STRINGS_IMPORT_SHARED_LIBS	=

# This is the "(mmck strings)" shared library file pathname.
#
MMCK_STRINGS_RUNTIME_SHARED_LIBS	= \
	lib/mmck.strings.$(MMCK_SOEXT)

## --------------------------------------------------------------------

# This is  a list of types  specification files, one for  each module.
# They are  *not* installed.   They are  concatenated to  generate the
# globl types file.
#
MMCK_STRINGS_TYPES_FILES_MODULES	=

# This is  a list of the  types specification files.  To  be installed
# along with shared libraries.
#
MMCK_STRINGS_TYPES_FILES_SHARED_LIBS	= \
	lib/mmck.strings.types

## --------------------------------------------------------------------

# Temporary  files  we need  for  parallel  builds, ".temp",  ".lock",
# ".stamp".
#
MMCK_STRINGS_TEMP_FILES		=


#### library (mmck strings): module version

MMCK_STRINGS_MODULES		+= lib/mmck.strings.version.scm
MMCK_STRINGS_RUNTIME_OBJECTS	+= lib/mmck.strings.version.$(OBJEXT)
MMCK_STRINGS_IMPORT_SOURCES	+= lib/mmck.strings.version.import.scm
MMCK_STRINGS_IMPORT_DEPS		+= lib/mmck.strings.version.import.$(MMCK_SOEXT)
MMCK_STRINGS_IMPORT_SHARED_LIBS	+= lib/mmck.strings.version.import.$(MMCK_SOEXT)
MMCK_STRINGS_TYPES_FILES_MODULES	+= lib/mmck.strings.version.types.scm
MMCK_STRINGS_TEMP_FILES		+= \
	lib/mmck.strings.version.temp		\
	lib/mmck.strings.version.lock		\
	lib/mmck.strings.version.stamp

# A compiler  invocation to build  an object file, also  generates the
# source  file for  the import  library; a  single rule  generates two
# targets.  We implement  the rules as suggested  in the documentation
# of GNU Automake, node "Multiple Outputs".
#
# It is ugly, I know.  But it  is supposed to support all the cases of
# parallel build and dry runs.  (Marco Maggi; May  4, 2019)
#
lib/mmck.strings.version.stamp: lib/mmck.strings.version.scm $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.strings.version.temp
	@touch lib/mmck.strings.version.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.strings.version.$(OBJEXT) $(<) -prologue lib/config.scm \
		-emit-types-file lib/mmck.strings.version.types.scm
	@$(MV) mmck.strings.version.import.scm lib/mmck.strings.version.import.scm
	@mv -f lib/mmck.strings.version.temp $@

lib/mmck.strings.version.types.scm: lib/mmck.strings.version.stamp

lib/mmck.strings.version.import.scm lib/mmck.strings.version.$(OBJEXT): lib/mmck.strings.version.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.strings.version.lock lib/mmck.strings.version.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.strings.version.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.strings.version.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.strings.version.stamp; \
	    $$dry rmdir lib/mmck.strings.version.lock; \
	  else \
	    while test -d lib/mmck.strings.version.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.strings.version.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.strings.version.import.$(MMCK_SOEXT): lib/mmck.strings.version.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck strings): module assertions

MMCK_STRINGS_MODULES		+= lib/mmck.strings.assertions.scm
MMCK_STRINGS_RUNTIME_OBJECTS	+= lib/mmck.strings.assertions.$(OBJEXT)
MMCK_STRINGS_IMPORT_SOURCES	+= lib/mmck.strings.assertions.import.scm
MMCK_STRINGS_IMPORT_DEPS		+= lib/mmck.strings.assertions.import.$(MMCK_SOEXT)
MMCK_STRINGS_IMPORT_SHARED_LIBS	+= lib/mmck.strings.assertions.import.$(MMCK_SOEXT)
MMCK_STRINGS_TYPES_FILES_MODULES	+= lib/mmck.strings.assertions.types.scm
MMCK_STRINGS_TEMP_FILES		+= \
	lib/mmck.strings.assertions.temp	\
	lib/mmck.strings.assertions.lock	\
	lib/mmck.strings.assertions.stamp

MMCK_STRINGS_ASSERTIONS_DEPS = \
	$(MMCK_OBJECTS_DEPS)

lib/mmck.strings.assertions.stamp: lib/mmck.strings.assertions.scm $(MMCK_STRINGS_ASSERTIONS_DEPS)
	@rm -f lib/mmck.strings.assertions.temp
	@touch lib/mmck.strings.assertions.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.strings.assertions.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.strings.assertions.types.scm
	@$(MV) mmck.strings.assertions.import.scm lib/mmck.strings.assertions.import.scm
	@mv -f lib/mmck.strings.assertions.temp $@

lib/mmck.strings.assertions.types.scm: lib/mmck.strings.assertions.stamp

lib/mmck.strings.assertions.import.scm lib/mmck.strings.assertions.$(OBJEXT): lib/mmck.strings.assertions.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.strings.assertions.lock lib/mmck.strings.assertions.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.strings.assertions.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.strings.assertions.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.strings.assertions.stamp; \
	    $$dry rmdir lib/mmck.strings.assertions.lock; \
	  else \
	    while test -d lib/mmck.strings.assertions.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.strings.assertions.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.strings.assertions.import.$(MMCK_SOEXT): lib/mmck.strings.assertions.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck strings): module core

MMCK_STRINGS_MODULES		+= lib/mmck.strings.core.scm
MMCK_STRINGS_RUNTIME_OBJECTS	+= lib/mmck.strings.core.$(OBJEXT)
MMCK_STRINGS_IMPORT_SOURCES	+= lib/mmck.strings.core.import.scm
MMCK_STRINGS_IMPORT_DEPS		+= lib/mmck.strings.core.import.$(MMCK_SOEXT)
MMCK_STRINGS_IMPORT_SHARED_LIBS	+= lib/mmck.strings.core.import.$(MMCK_SOEXT)
MMCK_STRINGS_TYPES_FILES_MODULES	+= lib/mmck.strings.core.types.scm
MMCK_STRINGS_TEMP_FILES		+= \
	lib/mmck.strings.core.temp	\
	lib/mmck.strings.core.lock	\
	lib/mmck.strings.core.stamp

MMCK_STRINGS_CORE_DEPS = \
	lib/mmck.strings.assertions.import.$(MMCK_SOEXT)	\
	$(MMCK_OBJECTS_DEPS)

lib/mmck.strings.core.stamp: lib/mmck.strings.core.scm $(MMCK_STRINGS_CORE_DEPS)
	@rm -f lib/mmck.strings.core.temp
	@touch lib/mmck.strings.core.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.strings.core.$(OBJEXT) $(<) \
		-emit-types-file lib/mmck.strings.core.types.scm
	@$(MV) mmck.strings.core.import.scm lib/mmck.strings.core.import.scm
	@mv -f lib/mmck.strings.core.temp $@

lib/mmck.strings.core.types.scm: lib/mmck.strings.core.stamp

lib/mmck.strings.core.import.scm lib/mmck.strings.core.$(OBJEXT): lib/mmck.strings.core.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.strings.core.lock lib/mmck.strings.core.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.strings.core.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.strings.core.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.strings.core.stamp; \
	    $$dry rmdir lib/mmck.strings.core.lock; \
	  else \
	    while test -d lib/mmck.strings.core.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.strings.core.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.strings.core.import.$(MMCK_SOEXT): lib/mmck.strings.core.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck strings): main module

MMCK_STRINGS_MODULES		+= lib/mmck.strings.scm
MMCK_STRINGS_RUNTIME_OBJECTS	+= lib/mmck.strings.$(OBJEXT)
MMCK_STRINGS_IMPORT_SOURCES	+= lib/mmck.strings.import.scm
MMCK_STRINGS_IMPORT_SHARED_LIBS	+= lib/mmck.strings.import.$(MMCK_SOEXT)
MMCK_STRINGS_TEMP_FILES		+= \
	lib/mmck.strings.temp			\
	lib/mmck.strings.lock			\
	lib/mmck.strings.stamp

# This is the main compilation unit and it defines the main module; it
# depends upon all the import libraries of the other modules.
#
lib/mmck.strings.stamp: lib/mmck.strings.scm $(MMCK_STRINGS_IMPORT_DEPS) $(MMCK_OBJECTS_DEPS)
	@rm -f lib/mmck.strings.temp
	@touch lib/mmck.strings.temp
	$(MMCK_LIB_ENV) $(CSC_COMPILE_OBJECT_SHARED) lib/mmck.strings.$(OBJEXT) $(<)
	@$(MV) mmck.strings.import.scm lib/mmck.strings.import.scm
	@mv -f lib/mmck.strings.temp $@

lib/mmck.strings.import.scm lib/mmck.strings.$(OBJEXT): lib/mmck.strings.stamp
	@dry=; for f in x $$MAKEFLAGS; do \
	  case $$f in \
	    *=*|--*);; \
	    *n*) dry=:;; \
	  esac; \
	done; \
	if test -f $@; then :; else \
	  $$dry trap 'rm -rf lib/mmck.strings.lock lib/mmck.strings.stamp' 1 2 13 15; \
	  if $$dry mkdir lib/mmck.strings.lock 2>/dev/null; then \
	    $$dry rm -f lib/mmck.strings.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) lib/mmck.strings.stamp; \
	    $$dry rmdir lib/mmck.strings.lock; \
	  else \
	    while test -d lib/mmck.strings.lock && test -z "$$dry"; do \
	      sleep 1; \
	    done; \
	    $$dry test -f lib/mmck.strings.stamp; exit $$?; \
	  fi; \
	fi

lib/mmck.strings.import.$(MMCK_SOEXT): lib/mmck.strings.import.scm
	$(MMCK_LIB_ENV) $(CSC_COMPILE_LIBRARY) $(@) $(<)


#### library (mmck strings): wrapping up

EXTRA_DIST	+= $(MMCK_STRINGS_MODULES)

CLEANFILES	+= \
	$(MMCK_STRINGS_RUNTIME_SHARED_LIBS)		\
	$(MMCK_STRINGS_RUNTIME_OBJECTS)		\
	$(MMCK_STRINGS_IMPORT_SHARED_LIBS)		\
	$(MMCK_STRINGS_IMPORT_SOURCES)			\
	$(MMCK_STRINGS_TYPES_FILES_MODULES)		\
	$(MMCK_STRINGS_TYPES_FILES_SHARED_LIBS)	\
	$(MMCK_STRINGS_TEMP_FILES)

lib/mmck.strings.$(MMCK_SOEXT): $(MMCK_STRINGS_RUNTIME_OBJECTS)
	$(MMCK_LIB_ENV) $(CSC_LINK_LIBRARY) $(@) $(^)

lib/mmck.strings.types: $(MMCK_STRINGS_TYPES_FILES_MODULES)
	cat $(MMCK_STRINGS_TYPES_FILES_MODULES) >$@


#### shared libraries installation

# All the import shared libraries that we want to installe.
#
MMCK_ALL_IMPORT_SHARED_LIBS	= \
	$(MMCK_STRINGS_IMPORT_SHARED_LIBS)

# All the runtime shared libraries that we want to install.
#
MMCK_ALL_RUNTIME_SHARED_LIBS	= \
	$(MMCK_STRINGS_RUNTIME_SHARED_LIBS)

# All the types specification files that we want to install.
#
MMCK_ALL_TYPES_FILES	= \
	$(MMCK_STRINGS_TYPES_FILES_SHARED_LIBS)

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
sharedlib_SCRIPTS	= \
	$(MMCK_ALL_RUNTIME_SHARED_LIBS)		\
	$(MMCK_ALL_IMPORT_SHARED_LIBS)		\
	$(MMCK_ALL_TYPES_FILES)


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

EXTRA_DIST		+= tests/test-strings-core.scm
MMCK_CHECK_OBJECTS	+= tests/test-strings-core.$(OBJEXT)
MMCK_CHECK_PROGS	+= tests/test-strings-core.exe

tests/test-strings-core.$(OBJEXT): tests/test-strings-core.scm $(MMCK_CHECK_DEPS)
	$(MMCK_CHECK_ENV) $(CSC_COMPILE_OBJECT_STATIC) $(@) $(<)

tests/test-strings-core.exe: tests/test-strings-core.$(OBJEXT)
	$(MMCK_CHECK_ENV) $(CSC_LINK_PROGRAM) $(@) $(<)


#### wrapping up tests

CLEANFILES	+= $(MMCK_CHECK_PROGS) $(MMCK_CHECK_OBJECTS)

if MMCK_TESTING_ENABLED
TESTS		= $(MMCK_CHECK_PROGS)
endif

### end of file
