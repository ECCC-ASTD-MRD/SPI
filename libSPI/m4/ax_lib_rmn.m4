# This macros checks for rmn headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-rmn - yes, no or path to rmn installation prefix
# 2) Three-options usage (all options are required):
#       --with-rmn=yes
#       --with-rmn-inc - path to base directory with  headers
#       --with-rmnib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(RMN_CFLAGS)
#   AC_SUBST(RMN_LDFLAGS)
#   AC_SUBST(RMN_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_RMN
#
AC_DEFUN([AX_LIB_RMN],
[
    AC_ARG_WITH([rmn],
        AC_HELP_STRING([--with-rmn=@<:@ARG@:>@],
            [use RMNLIB from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/rmn.h ; then 
                rmn_prefix=/usr/local
            elif test -d /usr/include/rmn.h ; then
                rmn_prefix=/usr
            else
                rmn_prefix=""
            fi
            rmn_requested="yes"
        elif test -d "$withval"; then
            rmn_prefix="$withval"
            rmn_requested="yes"
        else
            rmn_prefix=""
            rmn_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/rmn.h ; then 
            rmn_prefix=/usr/local
        elif test -d /usr/include/rmn.h ; then
            rmn_prefix=/usr
        else
            rmn_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([rmn-inc],
        AC_HELP_STRING([--with-rmn-inc=@<:@DIR@:>@],
            [path to RMNLIB headers]
        ),
        [rmn_include_dir="$withval"],
        [rmn_include_dir=""]
    )
    AC_ARG_WITH([rmn-lib],
        AC_HELP_STRING([--with-rmn-lib=@<:@ARG@:>@],
            [link options for RMNLIB libraries]
        ),
        [rmn_lib_flags="$withval"],
        [rmn_lib_flags=""]
    )

    RMN_CFLAGS=""
    RMN_LDFLAGS=""
    RMN_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_rmn_test="no"

    if test -n "$rmn_prefix"; then
        rmn_include_dir="$rmn_prefix/include"
        if test "$rmn_prefix" = "/usr"; then
            rmn_lib_flags="-lrmn"
        else
            rmn_lib_flags="-Wl,-rpath $rmn_prefix/lib -L$rmn_prefix/lib -lrmn"
        fi
        run_rmn_test="yes"
    elif test "$rmn_requested" = "yes"; then
        if test -n "$rmn_include_dir" -a -n "$rmn_lib_flags"; then
            run_rmn_test="yes"
        fi
    else
        run_rmn_test="no"
    fi

    #
    # Check rmn headers/libraries
    #
    if test "$run_rmn_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$rmn_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $rmn_lib_flags"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for RMN headers in $rmn_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <rpnmacros.h>
                ]],
                [[]]
            )],
            [
            RMN_CFLAGS="-I$rmn_include_dir"
            rmn_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            rmn_header_found="no"                      
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$rmn_header_found" = "yes"; then

            AC_MSG_CHECKING([for RMN libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <rpnmacros.h>
                    ]],
                    [[
char rmn[128];
int print=1;
f77name(rmnlib_version)(rmn,&print,127);
rmn[89]='\0';
                    ]]
                )],
                [
                RMN_LDFLAGS="$rmn_lib_flags"
                rmn_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                rmn_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for RMN])

    if test "$run_rmn_test" = "yes"; then
        if test "$rmn_header_found" = "yes" -a "$rmn_lib_found" = "yes"; then

            AC_SUBST([RMN_CFLAGS])
            AC_SUBST([RMN_LDFLAGS])

            HAVE_RMN="yes"
        else 
            HAVE_RMN="no"
        fi

        AC_MSG_RESULT([$HAVE_RMN])

    else
        HAVE_RMN="no"
        AC_MSG_RESULT([$HAVE_RMN])

        if test "$rmn_requested" = "yes"; then
            AC_MSG_WARN([RMNLIB support requested but headers or library not found. Specify valid prefix of librmn using --with-rmn=@<:@DIR@:>@ or provide include directory and linker flags using --with-rmn-inc and --with-rmn-lib])
        fi
    fi
])
