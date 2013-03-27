# This macros checks for dmv headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-dmv - yes, no or path to dmv installation prefix
# 2) Three-options usage (all options are required):
#       --with-dmv=yes
#       --with-dmv-inc - path to base directory with  headers
#       --with-dmv-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(DMV_CFLAGS)
#   AC_SUBST(DMV_LDFLAGS)
#   AC_SUBST(DMV_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_DMV
#
AC_DEFUN([AX_LIB_DMV],
[
    AC_ARG_WITH([dmv],
        AC_HELP_STRING([--with-dmv=@<:@ARG@:>@],
            [use DMV from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/dmv.h ; then 
                dmv_prefix=/usr/local
            elif test -d /usr/include/dmv. ; then
                dmv_prefix=/usr
            else
                dmv_prefix=""
            fi
            dmv_requested="yes"
        elif test -d "$withval"; then
            dmv_prefix="$withval"
            dmv_requested="yes"
        else
            dmv_prefix=""
            dmv_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/dmv.h ; then 
            dmv_prefix=/usr/local
        elif test -d /usr/include/dmv.h ; then
            dmv_prefix=/usr
        else
            dmv_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([dmv-inc],
        AC_HELP_STRING([--with-dmv-inc=@<:@DIR@:>@],
            [path to DMV headers]
        ),
        [dmv_include_dir="$withval"],
        [dmv_include_dir=""]
    )
    AC_ARG_WITH([dmv-lib],
        AC_HELP_STRING([--with-dmv-lib=@<:@ARG@:>@],
            [link options for DMV libraries]
        ),
        [dmv_lib_flags="$withval"],
        [dmv_lib_flags=""]
    )

    DMV_CFLAGS=""
    DMV_LDFLAGS=""
    DMV_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_dmv_test="no"

    if test -n "$dmv_prefix"; then
        dmv_include_dir="$dmv_prefix/include"
        if test "$dmv_prefix" = "/usr"; then
            dmv_lib_flags="-ldmv"
        else
            dmv_lib_flags="-L$dmv_prefix/lib -ldmv"
        fi
        run_dmv_test="yes"
    elif test "$dmv_requested" = "yes"; then
        if test -n "$dmv_include_dir" -a -n "$dmv_lib_flags"; then
            run_dmv_test="yes"
        fi
    else
        run_dmv_test="no"
    fi

    #
    # Check dmv headers/libraries
    #
    if test "$run_dmv_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$dmv_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $dmv_lib_flags"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for DMV headers in $dmv_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <libdmv.h>
                ]],
                [[]]
            )],
            [
            DMV_CFLAGS="-I$dmv_include_dir"
            dmv_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            dmv_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$dmv_header_found" = "yes"; then

            AC_MSG_CHECKING([for DMV libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <libdmv.h>
                    ]],
                    [[
dmvinit_c();
                    ]]
                )],
                [
                DMV_LDFLAGS="$dmv_lib_flags"
                dmv_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                dmv_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for DMV])

    if test "$run_dmv_test" = "yes"; then
        if test "$dmv_header_found" = "yes" -a "$dmv_lib_found" = "yes"; then

            AC_SUBST([DMV_CFLAGS])
            AC_SUBST([DMV_LDFLAGS])

            HAVE_DMV="yes"
        else 
            HAVE_DMV="no"
        fi

        AC_MSG_RESULT([$HAVE_DMV])

    else
        HAVE_DMV="no"
        AC_MSG_RESULT([$HAVE_DMV])

        if test "$dmv_requested" = "yes"; then
            AC_MSG_WARN([DMV support requested but headers or library not found. Specify valid prefix of libdmv using --with-dmv=@<:@DIR@:>@ or provide include directory and linker flags using --with-dmv-inc and --with-dmv-lib])
        fi
    fi
])
