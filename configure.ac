dnl -*- Mode: autoconf -*-
dnl
dnl configure.in - top level autoconf file for Redland API bindings
dnl (Process this file with autoconf to produce a configure script.)
dnl
dnl Copyright (C) 2000-2007, David Beckett http://purl.org/net/dajobe/
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


AC_INIT([Redland RDF Application Framework bindings], 1.0.7.1, http://bugs.librdf.org/, redland-bindings)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR(perl/example.pl)
AC_REVISION($Revision$)dnl

#AM_INIT_AUTOMAKE([1.7 check-news std-options -Wobsolete -Wportability -Wsyntax -Wunsupported])
AM_INIT_AUTOMAKE([1.7 check-news])
AM_MAINTAINER_MODE

dnl Checks for programs.
AM_PROG_CC_STDC
AM_SANITY_CHECK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])
AC_PROG_LIBTOOL

# Find a tar command for 'make dist'
AC_CHECK_PROGS(TAR, gnutar gtar tar)

AC_CHECK_PROGS(JAVA, java jre)
AC_CHECK_PROGS(JAVAC, javac)
AC_CHECK_PROGS(PYTHON, python)
AC_CHECK_PROGS(PERL, perl)
AC_CHECK_PROGS(PHP, php)
AC_CHECK_PROGS(TCL, tclsh)
AC_CHECK_PROGS(RUBY, ruby)
AC_CHECK_PROGS(SWIG, swig)
# dotGNU
AC_CHECK_PROGS(ILRUN, ilrun)
AC_CHECK_PROGS(CSCC, cscc)
AC_CHECK_PROGS(ILGAC, ilgac)
# Mono
AC_CHECK_PROGS(MONO, mono)
AC_CHECK_PROGS(MCS, mcs)
AC_CHECK_PROGS(GACUTIL, gacutil)
AC_CHECK_PROGS(SN, sn)
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

SWIG_MIN_VERSION=1.3.26

AC_MSG_CHECKING(SWIG support)
if test "X$SWIG" != "X"; then
  SWIG_VERSION=`$SWIG -version 2>&1 |sed -ne 's/^SWIG Version //p'`
  SWIG_VERSION_DEC=`echo $SWIG_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

 if test $SWIG_VERSION_DEC -ge 010326; then
   SWIG_PYTHON_ARGS=-noproxy
 fi

  SWIG_MIN_VERSION_DEC=`echo $SWIG_MIN_VERSION | $AWK -F. '{printf("%d\n", 10000*$1 + 100*$2 + $3)};'`

  if test $SWIG_VERSION_DEC -ge $SWIG_MIN_VERSION_DEC; then
    AC_MSG_RESULT($SWIG_VERSION - OK)
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
    AC_ARG_WITH(php-linking, [  --with-php-linking=TYPE      Set PHP dynamic linking (so, dylib, bundle)], php_linking="$withval")
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


AC_ARG_WITH(python, [  --with-python(=PYTHON)  Enable the Python interface], with_python="$withval", with_python="no")
AC_ARG_WITH(python_ldflags, [  --with-python_ldflags(=FLAGS)  Set the Python linker flags], with_python_ldflags="$withval", with_python_ldflags="")
if test "X$with_python_ldflags" != X; then
  PYTHON_LDFLAGS=$with_python_ldflags
fi

AC_MSG_CHECKING(Enable Python API)
if test "X$with_python" != Xno; then
  if test "X$with_python" != Xyes; then
    PYTHON=$with_python
  fi
  with_python=yes
fi

dnl need to change quotes to allow square brackets
changequote(<<, >>)dnl
PYTHON_VERSION=`$PYTHON -c 'import sys; print sys.version[:3]' 2>/dev/null`
changequote([, ])dnl
if test "X$PYTHON_VERSION" != X; then
  languages_available="$languages_available python($PYTHON_VERSION)"
  if test $with_python = yes; then
    languages="$languages python"
  fi
fi
AC_MSG_RESULT($with_python)
AC_SUBST(PYTHON_VERSION)

if test "X$PYTHON" != X; then
  AC_MSG_CHECKING(Python includes)
  python_prefix=`$PYTHON -c 'import sys; print sys.prefix' 2>/dev/null`
  python_exec_prefix=`$PYTHON -c "import sys; print sys.exec_prefix"`
  PYTHON_INCLUDES="-I${python_prefix}/include/python${PYTHON_VERSION}"
  if test "$python_prefix" != "$python_exec_prefix"; then
    PYTHON_INCLUDES="$PYTHON_INCLUDES -I${python_exec_prefix}/include/python${PYTHON_VERSION}"
  fi
  AC_SUBST(PYTHON_INCLUDES)
  AC_MSG_RESULT($PYTHON_INCLUDES)

  AC_MSG_CHECKING(Python libs)
  PYTHON_LIB="${python_prefix}/lib/python${PYTHON_VERSION}/site-packages"
  AC_SUBST(PYTHON_LIB)
  AC_MSG_RESULT($PYTHON_LIB)

  AC_MSG_CHECKING(Python LDFLAGS)
  if test "X$PYTHON_LDFLAGS" != X; then
    AC_MSG_RESULT(using user flags: $PYTHON_LDFLAGS)
  else
    # Guess PYTHON_LDFLAGS if not passed in
    if test `uname` = Darwin; then
      PYTHON_LDFLAGS="-Wl,-F. -Wl,-F. -bundle"
      if $PYTHON -c 'import sys, string; sys.exit(string.find(sys.prefix,"Framework")+1)'; then
	:
      else
	PYTHON_LDFLAGS="$PYTHON_LDFLAGS -framework Python"
      fi
    else
      PYTHON_LDFLAGS="-shared"
    fi
    AC_MSG_RESULT(guessed flags: $PYTHON_LDFLAGS)
  fi
  AC_SUBST(PYTHON_LDFLAGS)

else
  # This allows 'make clean' in the python directory to work when
  # python isn't available
  PYTHON=:
fi


if test "X$RUBY" != X; then

  dnl need to change quotes to allow square brackets
  changequote(<<, >>)dnl
  ruby_prefix=`$RUBY -rrbconfig -e "print Config::CONFIG['archdir']"`
  RUBY_LIB=`$RUBY -rrbconfig -e "puts Config::CONFIG['ruby_install_name']"`
  LIBRUBYARG_SHARED=`$RUBY -rrbconfig -e "puts Config::CONFIG['LIBRUBYARG_SHARED']"`
  RUBY_DIR=`$RUBY -rrbconfig -e "puts Config::CONFIG['archdir']"`
  RUBY_LIBDIR=`$RUBY -rrbconfig -e "puts Config::CONFIG['rubylibdir']"`
  changequote([, ])dnl

  RUBY_INCLUDES="-I$ruby_prefix"
else
  # This allows 'make clean' in the ruby directory to work when
  # ruby isn't available
  RUBY=:
  RUBY_INCLUDES=
  LIBRUBYARG_SHARED=
  RUBY_LIB=
  RUBY_DIR=
  RUBY_LIBDIR=
fi
AC_SUBST(RUBY_INCLUDES)
AC_SUBST(LIBRUBYARG_SHARED)
AC_SUBST(RUBY_LIB)
AC_SUBST(RUBY_DIR)
AC_SUBST(RUBY_LIBDIR)


AC_ARG_WITH(java, [  --with-java(=JAVA)      Enable the java interface], with_java="$withval", with_java="no")
AC_MSG_CHECKING(Enable Java API)
if test "X$with_java" != Xno ; then
  if test "X$with_java" != Xyes; then
    JAVA=$with_java
  fi
  with_java=yes
fi

dnl need to change quotes to allow square brackets
changequote(<<, >>)dnl
JAVA_VERSION=`$JAVA -version 2>&1 | $SED -ne 's/java version "\([^"]*\)".*/\1/p' 2>/dev/null`
changequote([, ])dnl
if test "X$JAVA_VERSION" != X; then
  languages_available="$languages_available java($JAVA_VERSION)"
  if test $with_java = yes; then
    languages="$languages java"
  fi
fi
AC_MSG_RESULT($with_java)
AC_SUBST(JAVA_VERSION)


AC_ARG_WITH(tcl, [  --with-tcl(=TCL)        Enable the Tcl interface], with_tcl="$withval", with_tcl="no")
AC_MSG_CHECKING(Enable Tcl API)
if test "X$with_tcl" != Xno ; then
  if test "X$with_tcl" != Xyes; then
    TCL=$with_tcl
  fi
  with_tcl=yes
fi

changequote(<<, >>)dnl
TCL_VERSION=`echo 'puts -nonewline [info tclversion]' | $TCL 2>/dev/null`
TCL_DIR=`echo 'puts $tcl_library' | $TCL 2>/dev/null`
changequote([, ])dnl
if test "X$TCL_VERSION" != X; then
  languages_available="$languages_available tcl($TCL_VERSION)"
  if test $with_tcl = yes; then
    languages="$languages tcl"
  fi
fi
AC_MSG_RESULT($with_tcl)
AC_SUBST(TCL_VERSION)
AC_SUBST(TCL_DIR)

AC_ARG_WITH(tcl_include, [  --with-tcl-include=DIR  Tcl include directory], tcl_include="$withval", tcl_include="")
AC_MSG_CHECKING(TCL include)
TCL_INCLUDES=
if test "X$TCL_VERSION" != X; then
  if test "X$tcl_include" = X ; then
    for dir in /usr/include/tcl$TCL_VERSION /usr/local/include/tcl$TCL_VERSION; do
      if test -r $dir/tcl.h; then
	TCL_INCLUDES="-I$dir"
	break
      fi
    done
  elif test "X$tcl_include" != X -a -r $tcl_include/tcl.h; then
    TCL_INCLUDES="-I$tcl_include"
  fi
fi
AC_MSG_RESULT($TCL_INCLUDES)
AC_SUBST(TCL_INCLUDES)


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



with_ecma_cla=
dnl Pick an ECMA CLI implementation
AC_ARG_WITH(ecma-cli, [  --with-ecma-cli=SYSTEM  Enable ECMA CLI interface - mono, dotgnu (default=no)], ecma_cli_name="$withval", ecma_cli_name="no")


AC_MSG_CHECKING(ECMA CLI implementations available)
result=
if test "X$MONO" != X -a "X$MCS" != X -a "X$GACUTIL" != X -a "X$SN" != X; then
  result="$result Mono"
  languages_available="$languages_available C#(Mono)"
fi
if test "X$ILRUN" != X -a "X$CSCC" != X -a "X$ILGAC" != X; then
  result="$result dotGNU"
  languages_available="$languages_available C#(dotGNU)"
fi
AC_MSG_RESULT($result)

AC_MSG_CHECKING(ECMA CLI implementations to use)
with_ecma_cli=no
CLIRUN=
CSHARPC=
CSHARPCFLAGS=
CLI_PREFIX=
for name in $ecma_cli_name mono dotgnu no; do
  case $name in
    mono)
      if test "X$MONO" != X -a "X$MCS" != X -a "X$GACUTIL" != X -a "X$SN" != X; then
        CLIRUN=$MONO
        CSHARPC=$MCS
        CSHARPCFLAGS="-debug+"
        CLI_PREFIX=`$PKG_CONFIG mono --variable=prefix`
        ecma_cli=Mono
      fi
      ;;

    dotgnu)
      if test "X$ILRUN" != X -a "X$CSCC" != X -a "X$ILGAC" != X; then
        CLIRUN=$ILRUN
        CSHARPC=$CSCC
        CSHARPCFLAGS="-g"
        CLI_PREFIX=/usr
        ecma_cli=dotGNU
      fi
      ;;

    no)
      ;;
    *)
      AC_MSG_ERROR(No such ECMA CLI implementation $name)
      ;;
esac
done
AC_MSG_RESULT($ecma_cli)
AC_SUBST(CLIRUN)
AC_SUBST(CSHARPC)
AC_SUBST(CSHARPCFLAGS)
AC_SUBST(CLI_PREFIX)

if test "$with_ecma_cli" != no; then
  languages="$languages C#($ecma_cli)"
fi


jdk=
jdkinc=
AC_ARG_WITH(jdk, [  --with-jdk=DIR          Java JDK location (default=auto)], user_jdk="$withval", user_jdk="")
AC_MSG_CHECKING(Java JDK includes)
if test "X$JAVA_VERSION" != X; then
  for jdkdir in $user_jdk $JAVA_HOME `ls -1dr /usr/local/jdk* 2>/dev/null` /usr/j2se `ls -1dr /usr/lib/java* 2>/dev/null` /System/Library/Frameworks/JavaVM.framework /usr/local/packages/j2sdk /opt/blackdown-jdk-*; do
    if test ! -d $jdkdir; then
      continue
    fi
    for idir in include Headers; do
      if test -r $jdkdir/$idir/jni.h ; then
        jdk=$jdkdir
        jdkinc=$jdkdir/$idir
        break 2
      fi
    done
  done
fi

if test "X$jdk" != X -a -r $jdkinc/jni.h; then
  inc2=`find $jdkinc -type d | grep -v "$jdkinc\$" | head -n 1`
  JDK_INCLUDES="-I$jdkinc"
  if test -r $inc2/jni_md.h; then
    JDK_INCLUDES="$JDK_INCLUDES -I$inc2"
  fi
fi

AC_SUBST(JDK_INCLUDES)
AC_MSG_RESULT($JDK_INCLUDES)


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
if test "$ac_cv_prog_gcc" = yes; then
  STANDARD_CFLAGS=
  MAINTAINER_CFLAGS="-Wall -Wshadow -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls"
else
  STANDARD_CFLAGS=
  MAINTAINER_CFLAGS=
fi


# Externally linked libraries - appear in redland-config
# -Dfoo -Idir
LIBRDF_CPPFLAGS=$CPPFLAGS
# -Ldir
LIBRDF_LDFLAGS=$LDFLAGS
# -llib
LIBRDF_LIBS=$LIBS




# redland is REQUIRED
AC_CHECK_PROG(REDLAND_CONFIG, redland-config, redland-config)
AC_ARG_WITH(redland, [  --with-redland=SOURCE   Pick redland source - system/internal (default auto)], with_redland="$withval", with_redland="auto")

changequote(<<, >>)dnl
OUR_REDLAND_VERSION=`echo $PACKAGE_VERSION | sed -e 's,\.[0-9]$,,'`
changequote([, ])dnl

AC_SUBST(REDLAND_MIN_VERSION)
if test "X$REDLAND_CONFIG" = X; then
  with_redland=internal
else
  REDLAND_VERSION=`$REDLAND_CONFIG --version 2>/dev/null`

  if test "X$with_redland" = Xauto; then
    if test "X$REDLAND_VERSION" = X; then
      with_redland=internal
    elif test $REDLAND_VERSION = $OUR_REDLAND_VERSION; then
      with_redland=system
    else
      AC_MSG_WARN(System redland $REDLAND_VERSION does not match - need $OUR_REDLAND_VERSION)
      with_redland=internal
    fi
  elif test $with_redland = system; then
    if test $REDLAND_VERSION != $OUR_REDLAND_VERSION; then
      AC_MSG_WARN(System redland $REDLAND_VERSION does not match - need $OUR_REDLAND_VERSION)
      AC_MSG_WARN(Proceeding anyway since --with-redland=system was given)
      AC_MSG_WARN(To use the internal redland use --with-redland=internal)
      AC_MSG_WARN(or omit any --with-redland argument.)
    fi
  fi
fi

AC_MSG_CHECKING(redland library source)
if test $with_redland = system; then
  REDLAND_VERSION_VERBOSE="system $REDLAND_VERSION"
  REDLAND_RUN=
else
  dir=
  redland_source_dirs="../redland ../librdf .."
  for testdir in $redland_source_dirs; do 
    if test -x $testdir/redland-src-config; then
      dir=`cd $testdir; pwd`
      break
    fi
  done
  if test "X$dir" = X; then
    AC_MSG_ERROR(internal redland sources not found in directories: $redland_source_dirs)
  fi
  REDLAND_CONFIG=$dir/redland-src-config
  REDLAND_VERSION=`$REDLAND_CONFIG --version 2>/dev/null`
  REDLAND_VERSION_VERBOSE="internal $REDLAND_VERSION in $dir"
  REDLAND_RUN="$REDLAND_CONFIG --run "
fi
AC_MSG_RESULT($REDLAND_VERSION_VERBOSE)

AC_SUBST(REDLAND_VERSION)
AC_SUBST(REDLAND_CONFIG)
AC_SUBST(REDLAND_RUN)

REDLAND_SWIG=`$REDLAND_CONFIG --swig`
AC_SUBST(REDLAND_SWIG)


LIBRDF_CPPFLAGS="$LIBRDF_CPPFLAGS `$REDLAND_CONFIG --cflags`"
LIBRDF_LIBS="$LIBRDF_LIBS `$REDLAND_CONFIG --libs`"


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

LIBRDF_LIBTOOLLIBS=`$REDLAND_CONFIG --libtool-libs`
AC_SUBST(LIBRDF_LIBTOOLLIBS)

AC_SUBST(LIBRDF_CPPFLAGS)
AC_SUBST(LIBRDF_LDFLAGS)

AC_SUBST(MEM)
AC_SUBST(MEM_LIBS)
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
php/Makefile
python/Makefile
python/test/Makefile
tcl/Makefile
java/Makefile
java/org/Makefile
java/org/librdf/Makefile
java/org/librdf/redland/Makefile
ruby/Makefile
ruby/test/Makefile
ruby/lib/Makefile
ruby/lib/rdf/Makefile
ruby/lib/rdf/redland/Makefile
ruby/lib/rdf/redland/schemas/Makefile
csharp/Makefile
csharp/examples/Makefile
demos/Makefile
data/Makefile
])
AC_CONFIG_FILES([redland-bindings.spec])
AC_CONFIG_FILES([perl/lib/RDF/Redland.pm])

AC_OUTPUT

AC_MSG_RESULT([
Redland build summary:
  Redland:              $REDLAND_VERSION_VERBOSE
  Language APIs built:   $languages
])
