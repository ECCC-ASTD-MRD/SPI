# This macros checks for TclGeoEER library and headers
# and defines compilation flags.
#
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-TclGeoEER - path to TclGeoEER directory containing everything needed (include, lib, ...)
# 2) Two-options usage (all options are required):
#       --with-TclGeoEER-inc - path to the TclGeoEER base include directory
#       --with-TclGeoEER-lib - path the the TclGeoEER shared lib

AC_DEFUN([AX_LIB_TCLGEOEER],[
    TGE_WANTED=0

    #----- Check for a path given by a --with-TclGeoEER option
    AC_ARG_WITH([TclGeoEER],[AC_HELP_STRING([--with-TclGeoEER=@<:@PATH@:>@],[Specify the TclGeoEER base directory])],[
        TGE_WANTED=1
        TGE_BASE=""
        if test -d "$withval"; then
            TGE_BASE="$withval"
        else
            AC_MSG_WARN([The path given with the --with-TclGeoEER option is not a directory. It will therefore not be used.])
        fi
    ],[
    ])

    #----- Check for a path given by a --with-TclGeoEER-inc option
    AC_ARG_WITH([TclGeoEER-inc],[AC_HELP_STRING([--with-TclGeoEER-inc=@<:@PATH@:>@],[Specify the TclGeoEER include path])],[
        TGE_WANTED=1
        TGE_INC=""
        if test -d "$withval"; then
            TGE_INC="$withval"
        else
            AC_MSG_WARN([The path given with the --with-TclGeoEER-inc option is not a directory. It will therefore not be used.])
        fi
    ],[
    ]
    )

    #----- Check for a path given by a --with-TclGeoEER-lib option
    AC_ARG_WITH([TclGeoEER-lib],[AC_HELP_STRING([--with-TclGeoEER-lib=@<:@PATH@:>@],[Specify the TclGeoEER library path])],[
        TGE_WANTED=1
        TGE_LIB=""
        if test -d "$withval"; then
            TGE_LIB="$withval"
        else
            AC_MSG_WARN([The path given with the --with-TclGeoEER-lib option is not a directory. It will therefore not be used.])
        fi
    ],[
    ]
    )

    #----- If we have missing values, fill them up the best we can

    if test -z "$TGE_INC"; then
        if test -n "$TGE_BASE" && test -d "$TGE_BASE/lib"; then
            TGE_INC="$TGE_BASE/lib"
        else
            AC_MSG_WARN([Could not find the TclGeoEER include dir. You can specify one using the --with-TclGeoEER-inc option. Maybe it's already available in CFLAGS?])
        fi
    fi

    if test -z "$TGE_LIB"; then
        if test -n "$TGE_BASE" && test -d "$TGE_BASE/include"; then
            TGE_INC="$TGE_BASE/include"
        else
            AC_MSG_WARN([Could not find the TclGeoEER lib dir. You can specify one using the --with-TclGeoEER-lib option. Maybe it's already available in LDFLAGS?])
        fi
    fi

    #----- Check for TclGeoEER headers

    saved_CPPFLAGS="$CPPFLAGS"
    TclGeoEER_CFLAGS=""
    test -n "$TGE_INC" && TclGeoEER_CFLAGS="-I$TGE_INC"
    CPPFLAGS="$TclGeoEER_CFLAGS $CPPFLAGS $PKG_CFLAGS"

    AC_MSG_CHECKING([for TclGeoEER headers ($TGE_INC)])

    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [[#include <tclData.h>]],
            [[]]
        )
    ],[
        AC_MSG_RESULT([found])
        TGE_INC_OK=1
    ],[
        TGE_INC_OK=0
        AC_MSG_RESULT([not found])
        AC_MSG_NOTICE([One or more TclGeoEER include file can't be found. You can specify their directory by using the --with-TclGeoEER-inc option.])
    ])
    AC_LANG_POP([C])

    #----- Check for TclGeoEER library

    saved_LIBS="$LIBS"
    TclGeoEER_LDFLAGS="-lTclGeoEER"
    test -n "$TGE_LIB" && TclGeoEER_LDFLAGS="-L$TGE_LIB $TclGeoEER_LDFLAGS"
    LIBS="-Wl,--unresolved-symbols=ignore-in-shared-libs $TclGeoEER_LDFLAGS $LIBS $PKG_LIBS"

    if test -n "$TGE_LIB"; then
        AC_MSG_CHECKING([for TclGeoEER library in ($TGE_LIB)])
        if test -r "$TGE_LIB/libTclGeoEER.so"; then
            AC_MSG_RESULT([found])
        else
            AC_MSG_RESULT([not found])
        fi
    else
        AC_MSG_NOTICE([No available TclGeoEER lib path, could not check for libTclGeoEER.so])
    fi


    AC_MSG_CHECKING([for TclGeoEER library (linking)])
    AC_LANG_PUSH([C])
    AC_LINK_IFELSE([
        AC_LANG_PROGRAM(
            [[@%:@include <tclData.h>]],
            [[Data_Get((char*)0);]]
        )],
        [
            AC_MSG_RESULT([yes])
            TGE_LIB_OK=1
        ],
        [
            AC_MSG_RESULT([no])
            TGE_LIB_OK=0
            AC_MSG_NOTICE([Could not link with the TclGeoEER lib. You can specify its directory by using the --with-TclGeoEER-lib option.])
        ]
    )
    AC_LANG_POP([C])

    #----- Restore the env

    CPPFLAGS="$saved_CPPFLAGS"
    LIBS="$saved_LIBS"

    #----- Substitue the @VAR@ by the values

    AC_MSG_CHECKING([for TclGeoEER])
    if test "$TGE_WANTED" -eq 1; then
        if test "$TGE_INC_OK" -eq 1 && test "$TGE_LIB_OK" -eq 1; then
            AC_MSG_RESULT([yes])
            AC_SUBST([TclGeoEER_CFLAGS])
            AC_SUBST([TclGeoEER_LDFLAGS])
            HAVE_TclGeoEER="yes"
        elif test "$TGE_WANTED" -eq 1; then
            AC_MSG_RESULT([no])
            HAVE_TclGeoEER="no"
            AC_MSG_ERROR([TclGeoEER was requested and could not be provided])
        fi
    else
        HAVE_TclGeoEER="no"
        AC_MSG_RESULT([no])
        AC_MSG_NOTICE([TclGeoEER is available but was not requested. It will therefore not be provided.])
    fi
])
