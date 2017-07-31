dnl -*- Mode: autoconf -*-
dnl
dnl configure.ac - top level autoconf file for Redland API bindings
dnl (Process this file with autoconf to produce a configure script.)
dnl
dnl Copyright (C) 2000-2011, David Beckett http://www.dajobe.org/
dnl Copyright (C) 2000-2005 Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
dnl
dnl This package is Free Software and part of Redland http://librdf.org/
dnl
dnl It is licensed under the following three licenses as alternatives:
dnl   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
dnl   2. GNU General Public License (GPL) V2 or any newer version
dnl   3. Apache License, V2.0 or any newer version
dnl
dnl You may not use this file except in compliance with at least one of
dnl the above three licenses.
dnl
dnl See LICENSE.html or LICENSE.txt at the top of this package for the
dnl complete terms and further detail along with the license texts for
dnl the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
dnl
dnl


AC_INIT([Redland librdf Language bindings],[1.0.17.2],[http://bugs.librdf.org/],[redland-bindings])
AC_PREREQ([2.62])
AC_CONFIG_SRCDIR(perl/example.pl)
AC_REVISION($Revision$)dnl
AC_CONFIG_HEADERS([bindings_config.h])
AC_CONFIG_AUX_DIR(build)
AC_CONFIG_MACRO_DIR(build)
AM_INIT_AUTOMAKE([1.11 check-news std-options -Wobsolete -Wportability -Wsyntax -Wunsupported])
AM_MAINTAINER_MODE

dnl 1.0.14 probably also works (Raptor and Rasqal dependencies the same)
MIN_REDLAND_VERSION=1.0.15

AC_CANONICAL_HOST

dnl Checks for programs.
AM_SANITY_CHECK
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_ARG_ENABLE([debug],
[  --enable-debug    Turn on debugging],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
esac],[debug=false])
AM_CONDITIONAL([DEBUG], [test x$debug = xtrue])

m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])
LT_INIT

# Find a tar command for 'make dist'
AC_CHECK_PROGS(TAR, gnutar gtar tar)

AC_CHECK_PROGS(LUA, lua)
AC_CHECK_PROGS(PYTHON, python)
AC_CHECK_PROGS(PERL, perl)
AC_CHECK_PROGS(PHP, php)
AC_CHECK_PROGS(RUBY, ruby)
AC_CHECK_PROGS(SWIG, swig)
# Gnome
AC_CHECK_PROGS(PKG_CONFIG, pkg-config)

if test "X$PERL" != "X"; then
  PERLINSTALLSITEARCH=`$PERL -MConfig -e 'print $Config{installsitearch};'`
  PERLMAN3DIR=`$PERL -MConfig -e 'print $Config{man3dir};'`
else
  PERLINSTALLSITEARCH=none
  PERLMAN3DIR=none
fi
AC_SUBST(PERLINSTALLSITEARCH)
AC_SUBST(PERLMAN3DIR)

dnl FIXME - require SWIG 2+ from next release
SWIG_MIN_VERSION=1.3.26
SWIG_RECOMMENDED_VERSION=2.0.0

AC_MSG_CHECKING(SWIG support)
if test "X$SWIG" != "X"; then
  SWIG_VERSION=`$SWIG -version 2>&1 |sed -ne 's/^SWIG Version //p'`
  SWIG_VERSION_DEC=`echo $SWIG_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  SWIG_MIN_VERSION_DEC=`echo $SWIG_MIN_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`
  SWIG_REC_VERSION_DEC=`echo $SWIG_RECOMMENDED_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  if test $SWIG_VERSION_DEC -ge $SWIG_MIN_VERSION_DEC; then
    if test $SWIG_VERSION_DEC -ge $SWIG_REC_VERSION_DEC; then
      AC_MSG_RESULT($SWIG_VERSION - OK)
    else
      AC_MSG_RESULT($SWIG_VERSION - OK but $SWIG_RECOMMENDED_VERSION recommended)
      dnl FIXME
      AC_MSG_WARN(SWIG $SWIG_RECOMMENDED_VERSION will be REQUIRED from the next release)
    fi
  else
    AC_MSG_RESULT($SWIG_VERSION - too old to generate language interfaces)
    if test -r $srcdir/perl/CORE_wrap.c ; then
      AC_MSG_WARN(Pre-generated language interface files are present)
      AC_MSG_WARN(If you want to change the interfaces you will need)
      AC_MSG_WARN(SWIG version $SWIG_MIN_VERSION from http://www.swig.org/)
    else
      AC_MSG_WARN(There are no pre-generated language interface files)
      AC_MSG_WARN(Redland language interfaces will NOT build.)
      AC_MSG_WARN(If you want to build them you will need)
      AC_MSG_WARN(SWIG version $SWIG_MIN_VERSION from http://www.swig.org/)
      AC_MSG_WARN(Alternatively copy the pre-generated interface)
      AC_MSG_WARN(files from a released version)
    fi
  fi
else
  AC_MSG_RESULT(not present - using pre-generated interface files)
fi

AC_SUBST(SWIG_PYTHON_ARGS)


languages=
languages_available=


dnl Checks for languages

AC_ARG_WITH(lua, [  --with-lua(=LUA)      Enable the Lua interface], with_lua="$withval", with_lua="no")
AC_MSG_CHECKING(Enable LUA API)
if test "X$with_lua" != Xno ; then
  if test "X$with_lua" != Xyes; then
    LUA=$with_lua
  fi
  with_lua=yes
fi
AC_MSG_RESULT($with_lua)

LUA_VERSION=
LUA_CFLAGS=
LUA_LDFLAGS=
if test "X$with_lua" = Xyes -a -n "$PKG_CONFIG"; then
  AC_MSG_CHECKING(LUA Version)
  LUA_PKGCONFIG=$LUA
  LUA_VERSION=`$PKG_CONFIG $LUA_PKGCONFIG --modversion`
  LUA_CFLAGS=`$PKG_CONFIG $LUA_PKGCONFIG --cflags`
  LUA_LDFLAGS=`$PKG_CONFIG $LUA_PKGCONFIG --libs`
  AC_MSG_RESULT($LUA_VERSION)
fi
AC_SUBST(LUA_CFLAGS)
AC_SUBST(LUA_LDFLAGS)

if test "X$LUA_VERSION" != X; then
  languages_available="$languages_available lua($LUA_VERSION)"
  if test $with_lua = yes; then
    languages="$languages lua"
  fi
fi


AC_CHECK_PROG(PHP_CONFIG, php-config, php-config)
if test "X$PHP_CONFIG" != X ; then

  PHP_VERSION=`$PHP_CONFIG --version`
  PHP_INCLUDES=`$PHP_CONFIG --includes`
  PHP_EXTENSION_DIR=`$PHP_CONFIG --extension-dir`

  if test $PHP_VERSION '>' 5.0.0; then
    PHP_SWIG="-php5"
  else
    PHP_SWIG="-php4"
  fi

  # Configure these if PHP_SO_SUFFIX is missing
  if test "X$PHP_SO_SUFFIX" = "X"; then
    # OSX/Darwin default is bundle
    if test `uname` = Darwin; then
      # FIXME - how to decide between bundle and dylib?
      php_linking=bundle
    else
      php_linking=so
    fi
    AC_ARG_WITH(php-linking, [  --with-php-linking=TYPE Set PHP module linking type (so, dylib, bundle)], php_linking="$withval")
    case $php_linking in
      so)
	PHP_LDFLAGS="-shared"
	PHP_CFLAGS=
	;;
      dylib)
	PHP_LDFLAGS="-flat_namespace -undefined suppress"
	PHP_CFLAGS="-fno-common"
	;;
      bundle)
	PHP_LDFLAGS="-bundle -flat_namespace -undefined suppress"
	PHP_CFLAGS="-fno-common"
	;;
      *)
	AC_MSG_ERROR(No such PHP linking type $php_linking)
	;;
    esac
    PHP_SO_SUFFIX=$php_linking
  fi
  AC_MSG_CHECKING(PHP Linking)
  AC_MSG_RESULT(compile '$PHP_CFLAGS' link '$PHP_LDFLAGS' suffix $PHP_SO_SUFFIX)

else
  # This allows 'make clean' in the php directory to work when
  # php isn't available
  PHP=:
  PHP_SWIG=
  PHP_INCLUDES=
  PHP_EXTENSION_DIR=
  PHP_CFLAGS=
  PHP_LDFLAGS=
  PHP_SO_SUFFIX=
fi
AC_SUBST(PHP_INCLUDES)
AC_SUBST(PHP_EXTENSION_DIR)
AC_SUBST(PHP_CFLAGS)
AC_SUBST(PHP_LDFLAGS)
AC_SUBST(PHP_SO_SUFFIX)
AC_SUBST(PHP_SWIG)


AC_ARG_WITH(perl, [  --with-perl(=PERL)      Enable the Perl interface], with_perl="$withval", with_perl="no")
AC_MSG_CHECKING(Enable Perl API)
if test "X$with_perl" != Xno ; then
  if test "X$with_perl" != Xyes; then
    PERL=$with_perl
  fi
  with_perl=yes
fi

PERL_VERSION=`$PERL -MConfig -e 'print $Config{version}' 2>/dev/null`
if test "X$PERL_VERSION" != X; then
  languages_available="$languages_available perl($PERL_VERSION)"
  if test $with_perl = yes; then
    languages="$languages perl"
  fi
fi
AC_MSG_RESULT($with_perl)
AC_SUBST(PERL_VERSION)

AC_ARG_WITH(perl-makemaker-args, [  --with-perl-makemaker-options=ARGS Set args for Perl Makefile.PL invocation (default '')], perl_makemaker_args="$withval", perl_makemaker_args="")
PERL_MAKEMAKER_ARGS="$perl_makemaker_args"
AC_SUBST(PERL_MAKEMAKER_ARGS)


AC_ARG_WITH(python, [  --with-python(=PYTHON)  Enable the Python interface], with_python="$withval", with_python="no")

# envariable override --with-python-ldflags and --with-libext
AC_ARG_VAR(PYTHON_LDFLAGS, [Python shared library link lines])
AC_ARG_VAR(PYTHON_LIBEXT, [Python shared library file extension])
AC_ARG_VAR(PYTHON_INCLUDES, [Install location of python includes])
AC_ARG_VAR(PYTHON_LIB, [Install location of python libraries])

AC_MSG_CHECKING(Enable Python API)
if test "X$with_python" != Xno; then
  if test "X$with_python" != Xyes; then
    PYTHON=$with_python
  fi
  with_python=yes
fi

dnl need to change quotes to allow square brackets
changequote(<<, >>)dnl
PYTHON_VERSION=`$PYTHON -c 'import sys; print(sys.version[:3])' 2>/dev/null`
changequote([, ])dnl
if test "X$PYTHON_VERSION" != X; then
  languages_available="$languages_available python($PYTHON_VERSION)"
  if test $with_python = yes; then
    languages="$languages python"
  fi
fi
AC_MSG_RESULT($with_python $PYTHON_VERSION)
AC_SUBST(PYTHON_VERSION)

if test "X$PYTHON" != X; then

  if test "X$PYTHON_LDFLAGS" = X; then
    AC_ARG_WITH(python_ldflags, [  --with-python-ldflags(=FLAGS)  Set the Python linker flags], with_python_ldflags="$withval", with_python_ldflags="")
    if test "X$with_python_ldflags" != X; then
      PYTHON_LDFLAGS=$with_python_ldflags
    fi
  fi

  if test "X$PYTHON_LIBEXT" = X; then
    python_default_libext=`$PYTHON -c 'from distutils import sysconfig; print(sysconfig.get_config_var("SO"))'`
    AC_ARG_WITH(python-libext, [  --with-python-libext(=EXT) Python shared library file extension (default .so)], with_python_libext="$withval", with_python_libext=$python_default_libext)
    if test "X$with_python_libext" != X; then
      PYTHON_LIBEXT=$with_python_libext
    fi
  fi

  AC_MSG_CHECKING(Python includes)
  python_prefix=`$PYTHON -c 'import sys; print(sys.prefix)' 2>/dev/null`
  python_exec_prefix=`$PYTHON -c "import sys; print(sys.exec_prefix)"`

  if test "X$PYTHON_INCLUDES" != X; then
    AC_MSG_RESULT(using user includes: $PYTHON_INCLUDES)
  else
    python_includes=`$PYTHON -c 'from distutils import sysconfig; print(sysconfig.get_python_inc())'`
    PYTHON_INCLUDES="-I$python_includes"
    AC_MSG_RESULT($PYTHON_INCLUDES)
  fi

  AC_ARG_WITH(python-get-python-lib-args, [  --with-python-get-python-lib-args=ARGS Set args for Python distutils.sysconfig.get_python_lib() function (default '')], get_python_lib_args="$withval", get_python_lib_args="")

  AC_MSG_CHECKING(Python libs)
  if test "X$PYTHON_LIB" != X; then
    AC_MSG_RESULT(using user lib: $PYTHON_LIB)
  else
    PYTHON_LIB=`$PYTHON -c "import distutils.sysconfig; print(distutils.sysconfig.get_python_lib($get_python_lib_args))"`
    AC_MSG_RESULT($PYTHON_LIB)
  fi

  AC_MSG_CHECKING(Python LDFLAGS)
  if test "X$PYTHON_LDFLAGS" != X; then
    AC_MSG_RESULT(using user ldflags: $PYTHON_LDFLAGS)
  else
    # Guess PYTHON_LDFLAGS if not passed in
    if test `uname` = Darwin; then
      PYTHON_LDFLAGS="-Wl,-F. -Wl,-F. -bundle"
      if echo $python_prefix | grep Frameworks >/dev/null; then
        fpath=`echo $python_prefix | sed -e 's,Frameworks/.*$,Frameworks,'`
	PYTHON_LDFLAGS="$PYTHON_LDFLAGS -F$fpath -framework Python"
      fi
    else
      PYTHON_LDFLAGS="-shared"
    fi
    AC_MSG_RESULT(guessed flags: $PYTHON_LDFLAGS)
  fi

  AC_MSG_CHECKING(Python shared library file extension)
  AC_MSG_RESULT($PYTHON_LIBEXT)
else
  # This allows 'make clean' in the python directory to work when
  # python isn't available
  PYTHON=:
fi


AC_ARG_WITH(ruby-arch-install-dir-variable, [  --with-ruby-arch-install-dir-variable=NAME Set Ruby arch install directory config variable (default 'archdir')], ruby_arch_install_dir_variable="$withval", ruby_arch_install_dir_variable="archdir")

dnl Setting this overrides ruby-arch-install-dir-variable above
AC_ARG_WITH(ruby-arch-install-dir, [  --with-ruby-arch-install-dir=NAME Set Ruby install directory (default 'auto')], ruby_arch_install_dir="$withval", ruby_arch_install_dir="auto")

AC_ARG_WITH(ruby-install-dir-variable, [  --with-ruby-install-dir-variable=NAME Set Ruby install directory config variable (default 'rubylibdir')], ruby_install_dir_variable="$withval", ruby_install_dir_variable="rubylibdir")

dnl Setting this overrides ruby-install-dir-variable above
AC_ARG_WITH(ruby-install-dir, [  --with-ruby-install-dir=NAME Set Ruby install directory (default 'auto')], ruby_install_dir="$withval", ruby_install_dir="auto")

RUBY_PACKAGE=redland
RUBY_CFLAGS=
RUBY_LDFLAGS=
RUBY_INCLUDES=
RUBY_SO_SUFFIX=
RUBY_INSTALLDIR=
RUBY_ARCH_INSTALLDIR=
if test "X$RUBY" != X; then
  dnl need to change quotes to allow square brackets
  changequote(<<, >>)dnl
  RUBY_ARCHDIR=`$RUBY -rrbconfig -e "print RbConfig::CONFIG['archdir']"`
  ruby_install_dir_config=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['$ruby_install_dir_variable']"`
  ruby_header_dir_config=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['rubyhdrdir']"`
  ruby_arch_config=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['arch']"`
  ruby_arch_header_dir_config=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['rubyarchhdrdir']"`
  ruby_arch_install_dir_config=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['$ruby_arch_install_dir_variable']"`
  librubyarg_shared=`$RUBY -rrbconfig -e "puts RbConfig::CONFIG['LIBRUBYARG_SHARED']"`
  changequote([, ])dnl

  AC_MSG_CHECKING(Ruby installation dir)
  if test "X$ruby_install_dir" != "Xauto"; then
    RUBY_INSTALLDIR=$ruby_install_dir
    AC_MSG_RESULT($RUBY_INSTALLDIR from user)
  else
    RUBY_INSTALLDIR=$ruby_install_dir_config
    AC_MSG_RESULT($RUBY_INSTALLDIR from RbConfig::CONFIG $ruby_install_dir_variable)
  fi

  AC_MSG_CHECKING(Ruby arch installation dir)
  if test "X$ruby_arch_install_dir" != "Xauto"; then
    RUBY_ARCH_INSTALLDIR=$ruby_arch_install_dir
    AC_MSG_RESULT($RUBY_ARCH_INSTALLDIR from user)
  else
    RUBY_ARCH_INSTALLDIR=$ruby_arch_install_dir_config
    AC_MSG_RESULT($RUBY_ARCH_INSTALLDIR from RbConfig::CONFIG $ruby_arch_install_dir_variable)
  fi

  AC_MSG_CHECKING(Ruby headers dir)
  if test "X$ruby_header_dir_config" != "Xnil" -a "X$ruby_header_dir_config" != "X"; then
    if test -d "$ruby_arch_header_dir_config"; then
      RUBY_INCLUDES="$RUBY_INCLUDES -I$ruby_header_dir_config -I$ruby_arch_header_dir_config"
      AC_MSG_RESULT($RUBY_INCLUDES from RbConfig::CONFIG rubyhdrdir and rubyarchhdrdir)
    elif test -d "$ruby_header_dir_config/$ruby_arch_config"; then
      RUBY_INCLUDES="$RUBY_INCLUDES -I$ruby_header_dir_config -I$ruby_header_dir_config/$ruby_arch_config"
      AC_MSG_RESULT($RUBY_INCLUDES from RbConfig::CONFIG rubyhdrdir and arch)
    fi
  else
    if test -r "$RUBY_ARCHDIR/ruby.h"; then
      RUBY_INCLUDES="$RUBY_INCLUDES -I$RUBY_ARCHDIR"
      AC_MSG_RESULT($RUBY_INCLUDES from arch dir)
    else
      AC_MSG_RESULT(ruby.h not found - unable to compile Redland ruby bindings)
    fi
  fi

  # OSX/Darwin default is bundle
  if test `uname` = Darwin; then
    # FIXME - how to decide between bundle and dylib?
    ruby_linking=bundle
  else
    ruby_linking=so
  fi
  AC_ARG_WITH(ruby-linking, [  --with-ruby-linking=TYPE Set Ruby module linking type (so, dylib, bundle)], ruby_linking="$withval")
  case $ruby_linking in
    so)
	RUBY_LDFLAGS="$RUBY_LDFLAGS -shared $librubyarg_shared"
	;;
    dylib)
	RUBY_LDFLAGS="$RUBY_LDFLAGS -flat_namespace -undefined suppress $librubyarg_shared"
	RUBY_CFLAGS="$RUBY_CFLAGS -fno-common"
	;;
    bundle)
	RUBY_LDFLAGS="$RUBY_LDFLAGS -bundle -flat_namespace -undefined suppress $librubyarg_shared"
	RUBY_CFLAGS="$RUBY_CFLAGS -fno-common"
	;;
    *)
	AC_MSG_ERROR(No such RUBY linking type $ruby_linking)
	;;
  esac
  RUBY_SO_SUFFIX=$ruby_linking
  AC_MSG_CHECKING(Ruby Linking)
  AC_MSG_RESULT(compile '$RUBY_CFLAGS' link '$RUBY_LDFLAGS' suffix $RUBY_SO_SUFFIX)
else
  # This allows 'make clean' in the ruby directory to work when
  # ruby isn't available
  RUBY=:
fi
AC_SUBST(RUBY_PACKAGE)
AC_SUBST(RUBY_CFLAGS)
AC_SUBST(RUBY_LDFLAGS)
AC_SUBST(RUBY_INCLUDES)
AC_SUBST(RUBY_SO_SUFFIX)
AC_SUBST(RUBY_INSTALLDIR)
AC_SUBST(RUBY_ARCH_INSTALLDIR)


AC_ARG_WITH(php, [  --with-php(=PHP)        Enable the PHP interface], with_php="$withval", with_php="no")
AC_MSG_CHECKING(Enable PHP API)
if test "X$with_php" != Xno ; then
  if test "X$with_php" != Xyes; then
    PHP=$with_php
  fi
  with_php=yes
fi

PHP_VERSION=`php-config --version 2>/dev/null`
if test "X$PHP_VERSION" != X; then
  languages_available="$languages_available php($PHP_VERSION)"
  if test $with_php = yes; then
    languages="$languages php"
  fi
fi
AC_MSG_RESULT($with_php)
AC_SUBST(PHP_VERSION)


AC_ARG_WITH(ruby, [  --with-ruby(=RUBY)      Enable the Ruby interface], with_ruby="$withval", with_ruby="no")
AC_MSG_CHECKING(Enable Ruby API)
if test "X$with_ruby" != Xno ; then
  if test "X$with_ruby" != Xyes; then
    RUBY=$with_ruby
  fi
  with_ruby=yes
fi

RUBY_VERSION=`$RUBY --version 2>/dev/null | $AWK '{print $2}'`
if test "X$RUBY_VERSION" != X; then
  languages_available="$languages_available ruby($RUBY_VERSION)"
  if test $with_ruby = yes; then
    languages="$languages ruby"
  fi
fi
AC_MSG_RESULT($with_ruby)
AC_SUBST(RUBY_VERSION)


AC_ARG_WITH(ada, [  --with-ada              Enable the Ada interface], with_ada="$withval", with_ada="no")
AC_MSG_CHECKING(Enable Ada API)

if test "X$with_ada" != Xno; then
  languages="$languages ada"
fi
AC_MSG_RESULT($with_ada)

if test "X$with_ada" != Xno; then
  AC_ARG_WITH(gprbuild,   [  --with-gprbuild(=gprbuild)   Ada builder], GPRBUILD="$withval")
  AC_ARG_WITH(gprclean,   [  --with-gprclean(=gprclean)   Ada cleaner], GPRCLEAN="$withval")
  AC_ARG_WITH(gprinstall, [  --with-gprbuild(=gprinstall) Ada installer], GPRINSTALL="$withval")

  AC_CHECK_PROGS(GPRBUILD, gprbuild)
  AC_CHECK_PROGS(GPRCLEAN, gprclean)
  AC_CHECK_PROGS(GPRINSTALL, gprinstall)
fi

AC_SUBST(GPRBUILD)
AC_SUBST(GPRCLEAN)
AC_SUBST(GPRINSTALL)


AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)


release_version=no
AC_ARG_ENABLE(release, [  --enable-release        Turn on optimizations (for maintainer).  ], \
 if test "$enableval" = "yes"; then \
	  release_version=yes
fi;)

if test "$USE_MAINTAINER_MODE" = yes -a $release_version = no; then
  CFLAGS=`echo $CFLAGS | sed s/-O2//`
  CXXFLAGS=`echo $CXXFLAGS | sed s/-O2//`
  CPPFLAGS=`echo $CPPFLAGS | sed s/-O2//`
fi



dnl compiler checks
# if using gcc...
if test "$ac_cv_c_compiler_gnu" = yes; then
  STANDARD_CFLAGS=
  MAINTAINER_CFLAGS="-Wall -Wshadow -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls"
else
  STANDARD_CFLAGS=
  MAINTAINER_CFLAGS=
fi


# Externally linked libraries - appear in redland-config
# -Dfoo -Idir
LIBRDF_CPPFLAGS=$CPPFLAGS
# -Ldir -llib
LIBRDF_LDFLAGS="$LDFLAGS $LIBS"




# redland is REQUIRED
changequote(<<, >>)dnl
OUR_REDLAND_VERSION=`echo $PACKAGE_VERSION | sed -e 's,\.[0-9]$,,'`
changequote([, ])dnl
OUR_REDLAND_VERSION_DEC=`echo $OUR_REDLAND_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

REDLAND_VERSION=

AC_MSG_CHECKING(for redland)
if $PKG_CONFIG --exists redland; then
  REDLAND_CONFIG="$PKG_CONFIG redland"
  REDLAND_VERSION=`$PKG_CONFIG redland --modversion`
  REDLAND_VERSION_DEC=`echo $REDLAND_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  AC_MSG_RESULT($REDLAND_VERSION)
else
  AC_MSG_RESULT(not found)

  AC_MSG_ERROR(Redland is not installed - see http://librdf.org/ to get a version $OUR_REDLAND_VERSION or newer)
fi


if $PKG_CONFIG --atleast-version=$MIN_REDLAND_VERSION redland; then
  :
else
  AC_MSG_ERROR(Redland version $REDLAND_VERSION found. $MIN_REDLAND_VERSION or newer is required - see http://librdf.org/)
fi


REDLAND_RUN=

AC_SUBST(REDLAND_CONFIG)
AC_SUBST(REDLAND_VERSION)
AC_SUBST(REDLAND_RUN)

REDLAND_SWIG=`$PKG_CONFIG redland --variable=swig`
AC_SUBST(REDLAND_SWIG)

LIBRDF_CPPFLAGS="$LIBRDF_CPPFLAGS `$PKG_CONFIG redland --cflags`"
LIBRDF_LDFLAGS="$LIBRDF_LDFLAGS `$PKG_CONFIG redland --libs`"

dnl needed for some bindings such as python to link raptor locator functions
RAPTOR_LDFLAGS=`$PKG_CONFIG raptor2 --libs`
RASQAL_LDFLAGS=`$PKG_CONFIG rasqal --libs`



dnl Checks for building sub-packages in sub directories
SD="$languages"

AC_SUBST(SD)


# Make final changes to cflags
MEM=
MEM_LIBS=

STANDARD_CFLAGS="$STANDARD_CFLAGS $CFLAGS"
if test "$USE_MAINTAINER_MODE" = yes; then
  CFLAGS="$MAINTAINER_CFLAGS $CFLAGS"
fi

LIBS=
CPPFLAGS=
LDFLAGS=

AC_SUBST(LIBRDF_CPPFLAGS)
AC_SUBST(LIBRDF_LDFLAGS)

AC_SUBST(RAPTOR_LDFLAGS)
AC_SUBST(RASQAL_LDFLAGS)

AC_SUBST(MEM)
AC_SUBST(MEM_LIBS)

AC_SUBST(ECHO)

AC_SUBST(STANDARD_CFLAGS)

abs_top_srcdir=`cd $srcdir; pwd`
AC_SUBST(abs_top_srcdir)
abs_top_builddir=`pwd`
AC_SUBST(abs_top_builddir)

AC_CONFIG_FILES([Makefile
bindings.rdf
docs/Makefile
perl/Makefile
perl/t/Makefile
perl/lib/Makefile
perl/lib/RDF/Makefile
perl/lib/RDF/Redland/Makefile
lua/Makefile
php/Makefile
python/Makefile
python/test/Makefile
ruby/Makefile
ruby/test/Makefile
ruby/lib/Makefile
ruby/lib/rdf/Makefile
ruby/lib/rdf/redland/Makefile
ruby/lib/rdf/redland/schemas/Makefile
ada/Makefile
data/Makefile
])
AC_CONFIG_FILES([redland-bindings.spec])
AC_CONFIG_FILES([perl/lib/RDF/Redland.pm])

AC_OUTPUT

AC_MSG_RESULT([
Redland build summary:
  Redland:              $REDLAND_VERSION
  Language APIs built:   $languages
])
