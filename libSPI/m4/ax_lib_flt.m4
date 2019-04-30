# This macros checks for flt headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-flt - yes, no or path to flt installation prefix
# 2) Three-options usage (all options are required):
#       --with-flt=yes
#       --with-flt-inc - path to base directory with  headers
#       --with-flt-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(FLT_CFLAGS)
#   AC_SUBST(FLT_LDFLAGS)
#   AC_SUBST(FLT_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_FLT
#
AC_DEFUN([AX_LIB_FLT],
[
    AC_ARG_WITH([flt],
        AC_HELP_STRING([--with-flt=@<:@ARG@:>@],
            [use FLT from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/flt.h ; then 
                flt_prefix=/usr/local
            elif test -d /usr/include/flt.h ; then
                flt_prefix=/usr
            else
                flt_prefix=""
            fi
            flt_requested="yes"
        elif test -d "$withval"; then
            flt_prefix="$withval"
            flt_requested="yes"
        else
            flt_prefix=""
            flt_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/flt.h ; then 
            flt_prefix=/usr/local
        elif test -d /usr/include/flt.h ; then
            flt_prefix=/usr
        else
            flt_prefix="" 
        fi
        ]
    )

    flt_include_dir="/"
    AC_ARG_WITH([flt-inc],
        AC_HELP_STRING([--with-flt-inc=@<:@DIR@:>@],
            [path to FLT headers]
        ),
        [flt_include_dir="$withval"],
        [flt_include_dir=""]
    )
    flt_lib_flags="-lflt"
    AC_ARG_WITH([flt-lib],
        AC_HELP_STRING([--with-flt-lib=@<:@ARG@:>@],
            [link options for FLT libraries]
        ),
        [flt_lib_flags="$withval"],
        [flt_lib_flags=""]
    )

    FLT_CFLAGS=""
    FLT_LDFLAGS=""
    FLT_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_flt_test="no"

    if test -n "$flt_prefix"; then
        flt_include_dir="$flt_prefix/include"
        if test "$flt_prefix" = "/usr"; then
            flt_lib_flags="-lflt"
        else
            flt_lib_flags="-L$flt_prefix/lib -lflt"
        fi
        run_flt_test="yes"
    elif test "$flt_requested" = "yes"; then
        run_flt_test="yes"
    else
        run_flt_test="no"
    fi

    #
    # Check flt headers/libraries
    #
    if test "$run_flt_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$flt_include_dir -Dlinux"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $flt_lib_flags -lm"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for FLT headers in $flt_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <flt.h>
                ]],
                [[]]
            )],
            [
            FLT_CFLAGS="-I$flt_include_dir"
            flt_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            flt_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$flt_header_found" = "yes"; then

            AC_MSG_CHECKING([for FLT libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <flt.h>
@%:@include <math.h>
                    ]],
                    [[
fltOpen("test");
                    ]]
                )],
                [
                FLT_LDFLAGS="$flt_lib_flags"
                flt_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                flt_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for FLT])

    if test "$run_flt_test" = "yes"; then
        if test "$flt_header_found" = "yes" -a "$flt_lib_found" = "yes"; then

            AC_SUBST([FLT_CFLAGS])
            AC_SUBST([FLT_LDFLAGS])

            HAVE_FLT="yes"
        else 
            HAVE_FLT="no"
        fi

        AC_MSG_RESULT([$HAVE_FLT])

    else
        HAVE_FLT="no"
        AC_MSG_RESULT([$HAVE_FLT])

        if test "$flt_requested" = "yes"; then
            AC_MSG_WARN([FLT support requested but headers or library not found. Specify valid prefix of libflt using --with-flt=@<:@DIR@:>@ or provide include directory and linker flags using --with-flt-inc and --with-flt-lib])
        fi
    fi
])
