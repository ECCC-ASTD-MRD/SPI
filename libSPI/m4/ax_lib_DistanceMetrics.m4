# This macros checks for DistanceMetrics library and headers
# and defines compilation flags.
#
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-DistanceMetrics - yes, no, opt, dynamic, static or path to directory containing everything needed (include, lib, modules, ...)
# 2) Two-options usage (all options are required):
#       --with-DistanceMetrics-inc - path to the DistanceMetrics include directory
#       --with-DistanceMetrics-lib - path the the DistanceMetrics shared lib directory
# 3) Additional options:
#       --enable-DistanceMetrics-static - Link statically to the DistanceMetrics library instead of dynamically

AC_DEFUN([AX_LIB_DISTANCEMETRICS],[
    DM_WANTED=0
    DM_STATIC=0

    #----- Check for static option
    AC_ARG_ENABLE([DistanceMetrics-static],[Link to DistanceMetrics using the static library instead of the dynamic one],[if test "$enableval" = "yes"; then DM_STATIC=1; DM_WANTED=1; fi])

    #----- Check for a path given by a --with-DistanceMetrics option
    AC_ARG_WITH([DistanceMetrics],[AC_HELP_STRING(
        [--with-DistanceMetrics@<:@=ARG@:>@],[
            Use DistanceMetrics (ARG=yes, the default);
            Use DistanceMetrics if available in the environment (ARG=opt);
            Use DistanceMetrics from prefix (ARG=PATH);
            Use DistanceMetrics with the static library (ARG=static, same as --enable-DistanceMetrics-static),
            Use DistanceMetrics with the dynamic library (ARG=dynamic, same as ARG=yes or --enable-DistanceMetrics-static=no),
            Do not use DistanceMetrics (ARG=no)
        ])
    ],[
        DM_WANTED=1
        DM_BASE=""
        if test -n "$withval" && test "$withval" != "yes"; then
            if test "$withval" = "no"; then
                DM_WANTED=0
            elif test "$withval" = "opt"; then
                DM_WANTED=2
            elif test "$withval" = "dynamic"; then
                DM_STATIC=0
            elif test "$withval" = "static"; then
                DM_STATIC=1
            elif test -d "$withval"; then
                DM_BASE="$withval"
            else
                AC_MSG_WARN([The path given with the --with-DistanceMetrics option is not a directory. It will therefore not be used.])
            fi
        fi
    ])

    #----- Check for a path given by a --with-DistanceMetrics-inc option
    AC_ARG_WITH([DistanceMetrics-inc],[AC_HELP_STRING([--with-DistanceMetrics-inc=PATH],[Specify the DistanceMetrics include path])],[
        DM_WANTED=1
        DM_INC=""
        if test -d "$withval"; then
            DM_INC="$withval"
        else
            AC_MSG_WARN([The path given with the --with-DistanceMetrics-inc option is not a directory. It will therefore not be used.])
        fi
    ])

    #----- Check for a path given by a --with-DistanceMetrics-lib option
    AC_ARG_WITH([DistanceMetrics-lib],[AC_HELP_STRING([--with-DistanceMetrics-lib=PATH],[Specify the DistanceMetrics library path])],[
        DM_WANTED=1
        DM_LIB=""
        if test -d "$withval"; then
            DM_LIB="$withval"
        else
            AC_MSG_WARN([The path given with the --with-DistanceMetrics-lib option is not a directory. It will therefore not be used.])
        fi
    ])

    #----- If we have missing values, fill them up the best we can

    if test -z "$DM_INC"; then
        if test -n "$DM_BASE" && test -d "$DM_BASE/include"; then
            DM_INC="$DM_BASE/include"
        fi
    fi

    if test -z "$DM_LIB"; then
        if test -n "$DM_BASE" && test -d "$DM_BASE/lib"; then
            DM_LIB="$DM_BASE/lib"
        fi
    fi

    DM_OK=1

    #----- Check for DistanceMetrics headers

    saved_CPPFLAGS="$CPPFLAGS"
    DM_CFLAGS=""
    test -n "$DM_INC" && DM_CFLAGS="-I$DM_INC"
    CPPFLAGS="$DM_CFLAGS $CPPFLAGS"

    AC_MSG_CHECKING([for DistanceMetrics headers])

    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [[#include <DistanceMetrics.h>]],
            [[]]
        )
    ],[
        AC_MSG_RESULT([found])
    ],[
        AC_MSG_RESULT([not found])
        if test "$DM_WANTED" -eq 1; then
            AC_MSG_ERROR([One or more DistanceMetrics include file can't be found. You can specify their directory by using the --with-DistanceMetrics-inc option.])
        else
            DM_OK=0
        fi
    ])
    AC_LANG_POP([C])

    #----- Check for DistanceMetrics library

    saved_LIBS="$LIBS"
    test $DM_STATIC -eq 1 && DM_LIB_NAME="DistanceMetrics_static" || DM_LIB_NAME="DistanceMetrics"
    DM_LDFLAGS="-l$DM_LIB_NAME"
    test -n "$DM_LIB" && { DM_LDFLAGS="-L$DM_LIB $DM_LDFLAGS"; LIBS="-L$DM_LIB $LIBS"; }
    LIBS="$OPENMP_CFLAGS $LIBS -lm"

    AC_MSG_CHECKING([for DistanceMetrics library])
    if test -n "$DM_LIB"; then
        if test $DM_STATIC -eq 1 && test -r "$DM_LIB/lib${DM_LIB_NAME}.a"; then
            AC_MSG_RESULT([found])
        elif test $DM_STATIC -ne 1 && test -r "$DM_LIB/lib${DM_LIB_NAME}.so"; then
            AC_MSG_RESULT([found])
        else
            AC_MSG_RESULT([not found])
        fi
    else
        AC_MSG_RESULT([no lib path given, can't check for library presence])
    fi

    AC_CHECK_LIB(
        [$DM_LIB_NAME],
        [[DM_GDM]],
    [
    ],[
        if test "$DM_WANTED" -eq 1; then
            AC_MSG_ERROR([Could not link with $DM_LIB_NAME lib. You can specify its directory by using the --with-DistanceMetrics-lib option.])
        else
            DM_OK=0
        fi
    ])

    #----- Restore the env

    CPPFLAGS="$saved_CPPFLAGS"
    LIBS="$saved_LIBS"

    #----- Substitue the @VAR@ by the values

    AC_MSG_CHECKING([for DistanceMetrics])
    if test "$DM_WANTED" -ne 0; then
        if test "$DM_OK" -eq 1; then
            AC_MSG_RESULT([yes])
            HAVE_DM=yes

            AC_SUBST([DM_CFLAGS])
            AC_SUBST([DM_LDFLAGS])
        else
            HAVE_DM=no
            AC_MSG_RESULT([no])
            if test "$DM_WANTED" -eq 1; then
               AC_MSG_ERROR([DistanceMetrics was requested and could not be provided])
            fi
        fi
    else
        AC_MSG_RESULT([no])
        HAVE_DM=no
        if test "$DM_OK" -eq 1; then
            AC_MSG_NOTICE([DistanceMetrics is available but was not requested. It will therefore not be provided.])
        fi
    fi
])

