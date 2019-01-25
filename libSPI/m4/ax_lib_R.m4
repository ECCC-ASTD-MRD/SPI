# This macros checks for R library and headers
# and defines compilation flags.
#
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-R - path to R HOME directory containing everything needed (include, lib, modules, ...)
# 2) Three-options usage (all options are required):
#       --with-R-home - path to the R HOME directory
#       --with-R-inc - path to the R base include directory
#       --with-R-lib - path the the R shared lib

AC_DEFUN([AX_LIB_R],[
    R_WANTED=0

    #----- Check for a path given by a --with-R option
    AC_ARG_WITH([R],[AC_HELP_STRING([--with-R=@<:@PATH@:>@],[Specify the R base directory])],[
        R_WANTED=1
        R_RBASE=""
        if test -d "$withval"; then
            R_RBASE="$withval"
        else
            AC_MSG_WARN([The path given with the --with-R option is not a directory. It will therefore not be used.])
        fi
    ],[
    ])

    #----- Check for a path given by a --with-R-home option
    AC_ARG_WITH([R-home],[AC_HELP_STRING([--with-R-home=@<:@PATH@:>@],[Specify the RHOME path])],[
        R_WANTED=1
        R_RHOME=""
        if test -d "$withval"; then
            R_RHOME="$withval"
        else
            AC_MSG_WARN([The path given with the --with-R-home option is not a directory. It will therefore not be used.])
        fi
    ],[
    ]
    )

    #----- Check for a path given by a --with-R-inc option
    AC_ARG_WITH([R-inc],[AC_HELP_STRING([--with-R-inc=@<:@PATH@:>@],[Specify the R include path])],[
        R_WANTED=1
        R_RINC=""
        if test -d "$withval"; then
            R_RINC="$withval"
        else
            AC_MSG_WARN([The path given with the --with-R-inc option is not a directory. It will therefore not be used.])
        fi
    ],[
    ]
    )

    #----- Check for a path given by a --with-R-lib option
    AC_ARG_WITH([R-lib],[AC_HELP_STRING([--with-R-lib=@<:@PATH@:>@],[Specify the R library path])],[
        R_WANTED=1
        R_RLIB=""
        if test -d "$withval"; then
            R_RLIB="$withval"
        else
            AC_MSG_WARN([The path given with the --with-R-lib option is not a directory. It will therefore not be used.])
        fi
    ],[
    ]
    )

    #----- If we have missing values, fill them up the best we can

    if test -z "$R_RHOME"; then
        if test -n "$R_RBASE"; then
            R_RHOME="$R_RBASE"
        elif test -d "$RHOME"; then
            R_RHOME="$RHOME"
        elif which R >/dev/null 2>&1; then
            R_RHOME="$(R RHOME)"
        elif test -d "/usr/lib/R"; then
            R_RHOME="/usr/lib/R"
        elif test "$R_WANTED" -eq 1; then
            AC_MSG_ERROR([Could not find a valid RHOME. Either specify one via the --with-R option or the --with-R-home option.])
        fi
    fi

    if test -z "$R_RINC"; then
        if test -d "$R_INCLUDE_DIR"; then
            R_RINC="$R_INCLUDE_DIR"
        elif test -r "$R_RHOME/bin/R"; then
            R_RINC="$(sed -n 's/\(^\| \)R_INCLUDE_DIR=\(.*\)/\2/p' "$R_RHOME/bin/R")"
            test -d "$R_RINC" || R_RINC=""
        fi
    
        if test -z "$R_RINC"; then
            if test -d "/usr/share/R/include"; then
                R_RINC="/usr/share/R/include"
            else
                AC_MSG_NOTICE([Could not find the R include dir. You can specify one using the --with-R-inc option. Maybe it's already available in CFLAGS?])
            fi
        fi
    fi

    if test -z "$R_RLIB"; then
        if test -n "$R_RBASE" && test -d "$R_RBASE/lib"; then
            R_RLIB="$R_RBASE/lib"
        elif test -d "$R_RHOME/lib"; then
            R_RLIB="$R_RHOME/lib"
        elif test -d "/usr/lib/R/lib"; then
            R_RLIB="/usr/lib/R/lib"
        else
            AC_MSG_NOTICE([Could not find the R lib dir. You can specify one using the --with-R-lib option. Maybe it's already available in LDFLAGS?])
        fi
    fi

    R_OK=1

    #----- Check for R Home

    AC_MSG_CHECKING([for valid R HOME ($R_RHOME)])
    if test -x "$R_RHOME/bin/exec/R"; then
        AC_MSG_RESULT([valid])
    else
        AC_MSG_RESULT([invalid])
        if test "$R_WANTED" -eq 1; then
            AC_MSG_ERROR([A valid RHOME is necessary. You can specify one by using the --with-R-home option])
        else
            R_OK=0
        fi
    fi
    

    #----- Check for R headers

    saved_CPPFLAGS="$CPPFLAGS"
    R_CFLAGS=""
    test -n "$R_RINC" && R_CFLAGS="-I$R_RINC"
    CPPFLAGS="$R_CFLAGS $CPPFLAGS"

    AC_MSG_CHECKING([for R headers ($R_RINC)])

    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [[#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <Rdefines.h>
#include <Rinterface.h>]],
            [[]]
        )
    ],[
        AC_MSG_RESULT([found])
    ],[
        AC_MSG_RESULT([not found])
        if test "$R_WANTED" -eq 1; then
            AC_MSG_ERROR([One or more R include file can't be found. You can specify their directory by using the --with-R-inc option.])
        else
            R_OK=0
        fi
    ])
    AC_LANG_POP([C])

    #----- Check for R library

    saved_LIBS="$LIBS"
    R_LDFLAGS="-lR"
    test -n "$R_RLIB" && { R_LDFLAGS="-L$R_RLIB $R_LDFLAGS"; LIBS="-L$R_RLIB $LIBS"; }

    if test -n "$R_RLIB"; then
        AC_MSG_CHECKING([for R lib ($R_RLIB)])
        if test -r "$R_RLIB/libR.so"; then
            AC_MSG_RESULT([found])
        else
            AC_MSG_RESULT([not found])
        fi
    else
        AC_MSG_NOTICE([No available R lib path, could not check for libR.so])
    fi

    AC_CHECK_LIB(
        [R],
        [[Rf_initEmbeddedR]],
    [
    ],[
        if test "$R_WANTED" -eq 1; then
            AC_MSG_ERROR([Could not link with the R lib. You can specify its directory by using the --with-R-lib option.])
        else
            R_OK=0
        fi
    ])

    #----- Restore the env

    CPPFLAGS="$saved_CPPFLAGS"
    LIBS="$saved_LIBS"

    #----- Substitue the @VAR@ by the values

    AC_MSG_CHECKING([for R])
    if test "$R_WANTED" -eq 1; then
        if test "$R_OK" -eq 1; then
            AC_MSG_RESULT([yes])
            HAVE_R=yes

            AC_SUBST([R_RHOME])
            AC_SUBST([R_CFLAGS])
            AC_SUBST([R_LDFLAGS])
        else
            HAVE_R=no
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([R was requested and could not be provided])
        fi
    else
        AC_MSG_RESULT([no])
        HAVE_R=no
        AC_MSG_NOTICE([R is available but was not requested. It will therefore not be provided])
    fi
])

