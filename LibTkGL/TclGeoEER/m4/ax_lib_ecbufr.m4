# This macros checks for bufr headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-bufr - yes, no or path to bufr installation prefix
# 2) Three-options usage (all options are required):
#       --with-bufr=yes
#       --with-bufr-inc - path to base directory with  headers
#       --with-bufr-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(ECBUFR_CFLAGS)
#   AC_SUBST(ECBUFR_LDFLAGS)
#   AC_SUBST(ECBUFR_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_ECBUFR
#
AC_DEFUN([AX_LIB_ECBUFR],
[
    AC_ARG_WITH([ecbufr],
        AC_HELP_STRING([--with-ecbufr=@<:@ARG@:>@],
            [use ECBUFR from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/bufr_api.h ; then 
                ecbufr_prefix=/usr/local
            elif test -d /usr/include/bufr_api.h ; then
                ecbufr_prefix=/usr
            else
                ecbufr_prefix=""
            fi
            ecbufr_requested="yes"
        elif test -d "$withval"; then
            ecbufr_prefix="$withval"
            ecbufr_requested="yes"
        else
            ecbufr_prefix=""
            ecbufr_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/bufr_api.h ; then 
            ecbufr_prefix=/usr/local
        elif test -d /usr/include/bufr_api.h ; then
            ecbufr_prefix=/usr
        else
            ecbufr_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([ecbufr-inc],
        AC_HELP_STRING([--with-ecbufr-inc=@<:@DIR@:>@],
            [path to ECBUFR headers]
        ),
        [ecbufr_include_dir="$withval"],
        [ecbufr_include_dir=""]
    )
    AC_ARG_WITH([ecbufr-lib],
        AC_HELP_STRING([--with-ecbufr-lib=@<:@ARG@:>@],
            [link options for ECBUFR libraries]
        ),
        [ecbufr_lib_flags="$withval"],
        [ecbufr_lib_flags=""]
    )

    ECBUFR_CFLAGS=""
    ECBUFR_LDFLAGS=""
    ECBUFR_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_ecbufr_test="no"

    if test -n "$ecbufr_prefix"; then
        ecbufr_include_dir="$ecbufr_prefix/include"
        if test "$ecbufr_prefix" = "/usr"; then
            ecbufr_lib_flags="-lecbufr"
        else
            ecbufr_lib_flags="-L$ecbufr_prefix/lib -lecbufr"
        fi
        run_ecbufr_test="yes"
    elif test "$ecbufr_requested" = "yes"; then
        if test -n "$ecbufr_include_dir" -a -n "$ecbufr_lib_flags"; then
            run_ecbufr_test="yes"
        fi
    else
        run_ecbufr_test="no"
    fi

    #
    # Check ecbufr headers/libraries
    #
    if test "$run_ecbufr_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$ecbufr_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $ecbufr_lib_flags -lm"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for ECBUFR headers in $ecbufr_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <bufr_api.h>
@%:@include <bufr_array.h>
@%:@include <bufr_local.h>
                ]],
                [[]]
            )],
            [
            ECBUFR_CFLAGS="-I$ecbufr_include_dir"
            ecbufr_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            ecbufr_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$ecbufr_header_found" = "yes"; then

            AC_MSG_CHECKING([for ECBUFR libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <bufr_api.h>
@%:@include <bufr_array.h>
@%:@include <bufr_local.h>
                    ]],
                    [[
BUFR_Message   *msg;
bufr_read_message(NULL,&msg);
                    ]]
                )],
                [
                ECBUFR_LDFLAGS="$ecbufr_lib_flags"
                ecbufr_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                ecbufr_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for ECBUFR])

    if test "$run_ecbufr_test" = "yes"; then
        if test "$ecbufr_header_found" = "yes" -a "$ecbufr_lib_found" = "yes"; then

            AC_SUBST([ECBUFR_CFLAGS])
            AC_SUBST([ECBUFR_LDFLAGS])

            HAVE_ECBUFR="yes"
        else 
            HAVE_ECBUFR="no"
        fi

        AC_MSG_RESULT([$HAVE_ECBUFR])

    else
        HAVE_ECBUFR="no"
        AC_MSG_RESULT([$HAVE_ECBUFR])

        if test "$ecbufr_requested" = "yes"; then
            AC_MSG_WARN([ECBUFR support requested but headers or library not found. Specify valid prefix of libecbufr using --with-ecbufr=@<:@DIR@:>@ or provide include directory and linker flags using --with-ecbufr-inc and --with-ecbufr-lib])
        fi
    fi
])
