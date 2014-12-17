# This macros checks for eer headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-eer - yes, no or path to eer installation prefix
# 2) Three-options usage (all options are required):
#       --with-eer=yes
#       --with-eer-inc - path to base directory with  headers
#       --with-eer-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(EER_CFLAGS)
#   AC_SUBST(EER_LDFLAGS)
#   AC_SUBST(EER_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_EER
#
AC_DEFUN([AX_LIB_EER],
[
    AC_ARG_WITH([eer],
        AC_HELP_STRING([--with-eer=@<:@ARG@:>@],
            [use EERUtils from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/eerUtils.h ; then 
                eer_prefix=/usr/local
            elif test -d /usr/include/eerUtils.h ; then
                eer_prefix=/usr
            else
                eer_prefix=""
            fi
            eer_requested="yes"
        elif test -d "$withval"; then
            eer_prefix="$withval"
            eer_requested="yes"
        elif test "$withval" = "no"; then
            eer_prefix=""
            eer_requested="no"
        else
            eer_prefix=""
            eer_requested="yes"
            AC_MSG_ERROR([--with-eer option was used with an invalid value ($withval). It can either be 'yes', 'no' or a valid directory.])
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/eerUtils.h ; then 
            eer_prefix=/usr/local
        elif test -d /usr/include/eerUtils.h ; then
            eer_prefix=/usr
        else
            eer_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([eer-inc],
        AC_HELP_STRING([--with-eer-inc=@<:@DIR@:>@],
            [path to EERUtils headers]
        ),
        [eer_include_dir="$withval"],
        [eer_include_dir=""]
    )
    AC_ARG_WITH([eer-lib],
        AC_HELP_STRING([--with-eer-lib=@<:@ARG@:>@],
            [link options for EERUtils libraries]
        ),
        [eer_lib_flags="$withval"],
        [eer_lib_flags=""]
    )

    EER_CFLAGS=""
    EER_LDFLAGS=""
    EER_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_eer_test="no"

    if test -n "$eer_prefix"; then
        eer_include_dir="$eer_prefix/include"
        if test "$eer_prefix" = "/usr"; then
            eer_lib_flags="-leerUtils"
        else
            eer_lib_flags="-L$eer_prefix/lib -leerUtils"
        fi
        run_eer_test="yes"
    elif test "$eer_requested" = "yes"; then
        if test -n "$eer_include_dir" -a -n "$eer_lib_flags"; then
            run_eer_test="yes"
        fi
    else
        run_eer_test="no"
    fi

    #
    # Check eer headers/libraries
    #
    if test "$run_eer_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$eer_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $eer_lib_flags -lm $RMN_LDFLAGS"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for EERUtils headers in $eer_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <eerUtils.h>
                ]],
                [[]]
            )],
            [
            EER_CFLAGS="-I$eer_include_dir"
            eer_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            eer_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$eer_header_found" = "yes"; then

            AC_MSG_CHECKING([for EERUtils libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <eerUtils.h>
                    ]],
                    [[
System_IsBigEndian();
                    ]]
                )],
                [
                EER_LDFLAGS="$eer_lib_flags"
                eer_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                eer_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for EER])

#EER_LDFLAGS="$eer_lib_flags"
#eer_header_found="yes"
#eer_lib_found="yes"

    if test "$run_eer_test" = "yes"; then
        if test "$eer_header_found" = "yes" -a "$eer_lib_found" = "yes"; then

            AC_SUBST([EER_CFLAGS])
            AC_SUBST([EER_LDFLAGS])

            HAVE_EER="yes"
        else 
            HAVE_EER="no"
        fi

        AC_MSG_RESULT([$HAVE_EER])

    else
        HAVE_EER="no"
        AC_MSG_RESULT([$HAVE_EER])

        if test "$eer_requested" = "yes"; then
            AC_MSG_ERROR([EERUtils is necessary but headers or library not found. Specify valid prefix of libeer using --with-eer=@<:@DIR@:>@ or provide include directory and linker flags using --with-eer-inc and --with-eer-lib])
        fi
    fi
])
