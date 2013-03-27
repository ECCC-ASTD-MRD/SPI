# This macros checks for grib headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-grib - yes, no or path to grib installation prefix
# 2) Three-options usage (all options are required):
#       --with-grib=yes
#       --with-grib-inc - path to base directory with  headers
#       --with-grib-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(GRIB_CFLAGS)
#   AC_SUBST(GRIB_LDFLAGS)
#   AC_SUBST(GRIB_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_GRIB
#
AC_DEFUN([AX_LIB_GRIB],
[
    AC_ARG_WITH([grib],
        AC_HELP_STRING([--with-grib=@<:@ARG@:>@],
            [use GRIB from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/grib.h ; then 
                grib_prefix=/usr/local
            elif test -d /usr/include/grib. ; then
                grib_prefix=/usr
            else
                grib_prefix=""
            fi
            grib_requested="yes"
        elif test -d "$withval"; then
            grib_prefix="$withval"
            grib_requested="yes"
        else
            grib_prefix=""
            grib_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/grib.h ; then 
            grib_prefix=/usr/local
        elif test -d /usr/include/grib.h ; then
            grib_prefix=/usr
        else
            grib_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([grib-inc],
        AC_HELP_STRING([--with-grib-inc=@<:@DIR@:>@],
            [path to GRIB headers]
        ),
        [grib_include_dir="$withval"],
        [grib_include_dir=""]
    )
    AC_ARG_WITH([grib-lib],
        AC_HELP_STRING([--with-grib-lib=@<:@ARG@:>@],
            [link options for GRIB libraries]
        ),
        [grib_lib_flags="$withval"],
        [grib_lib_flags=""]
    )

    GRIB_CFLAGS=""
    GRIB_LDFLAGS=""
    GRIB_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_grib_test="no"

    if test -n "$grib_prefix"; then
        grib_include_dir="$grib_prefix/include"
        if test "$grib_prefix" = "/usr"; then
            grib_lib_flags="-lgrib_api"
        else
            grib_lib_flags="-L$grib_prefix/lib -lgrib_api"
        fi
        run_grib_test="yes"
    elif test "$grib_requested" = "yes"; then
        if test -n "$grib_include_dir" -a -n "$grib_lib_flags"; then
            run_grib_test="yes"
        fi
    else
        run_grib_test="no"
    fi

    #
    # Check grib headers/libraries
    #
    if test "$run_grib_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$grib_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $grib_lib_flags"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for GRIB headers in $grib_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <grib_api.h>
                ]],
                [[]]
            )],
            [
            GRIB_CFLAGS="-I$grib_include_dir"
            grib_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            grib_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$grib_header_found" = "yes"; then

            AC_MSG_CHECKING([for GRIB libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <grib_api.h>
                    ]],
                    [[
grib_context_get_default();
                    ]]
                )],
                [
                GRIB_LDFLAGS="$grib_lib_flags"
                grib_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                grib_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for GRIB])

    if test "$run_grib_test" = "yes"; then
        if test "$grib_header_found" = "yes" -a "$grib_lib_found" = "yes"; then

            AC_SUBST([GRIB_CFLAGS])
            AC_SUBST([GRIB_LDFLAGS])

            HAVE_GRIB="yes"
        else 
            HAVE_GRIB="no"
        fi

        AC_MSG_RESULT([$HAVE_GRIB])

    else
        HAVE_GRIB="no"
        AC_MSG_RESULT([$HAVE_GRIB])

        if test "$grib_requested" = "yes"; then
            AC_MSG_WARN([GRIB support requested but headers or library not found. Specify valid prefix of libgrib using --with-grib=@<:@DIR@:>@ or provide include directory and linker flags using --with-grib-inc and --with-grib-lib])
        fi
    fi
])
