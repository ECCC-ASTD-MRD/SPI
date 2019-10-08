# This macros checks for eccodes headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-eccodes - yes, no or path to eccodes installation prefix
# 2) Three-options usage (all options are required):
#       --with-eccodes=yes
#       --with-eccodes-inc - path to base directory with  headers
#       --with-eccodes-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(ECCODES_CFLAGS)
#   AC_SUBST(ECCODES_LDFLAGS)
#   AC_SUBST(ECCODES_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_ECCODES
#
AC_DEFUN([AX_LIB_ECCODES],
[
    AC_ARG_WITH([eccodes],
        AC_HELP_STRING([--with-eccodes=@<:@ARG@:>@],
            [use ECCODES from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/eccodes.h ; then 
                eccodes_prefix=/usr/local
            elif test -d /usr/include/eccodes.h ; then
                eccodes_prefix=/usr
            else
                eccodes_prefix=""
            fi
            eccodes_requested="yes"
        elif test -d "$withval"; then
            eccodes_prefix="$withval"
            eccodes_requested="yes"
        else
            eccodes_prefix=""
            eccodes_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/eccodes.h ; then 
            eccodes_prefix=/usr/local
        elif test -d /usr/include/eccodes.h ; then
            eccodes_prefix=/usr
        else
            eccodes_prefix="" 
        fi
        ]
    )

    eccodes_include_dir="/"
    AC_ARG_WITH([eccodes-inc],
        AC_HELP_STRING([--with-eccodes-inc=@<:@DIR@:>@],
            [path to ECCODES headers]
        ),
        [eccodes_include_dir="$withval"],
        [eccodes_include_dir=""]
    )
    eccodes_lib_flags="-leccodes"
    AC_ARG_WITH([eccodes-lib],
        AC_HELP_STRING([--with-eccodes-lib=@<:@ARG@:>@],
            [link options for ECCODES libraries]
        ),
        [eccodes_lib_flags="$withval"],
        [eccodes_lib_flags=""]
    )

    ECCODES_CFLAGS=""
    ECCODES_LDFLAGS=""
    ECCODES_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_eccodes_test="no"

    if test -n "$eccodes_prefix"; then
        eccodes_include_dir="$eccodes_prefix/include"
        if test "$eccodes_prefix" = "/usr"; then
            eccodes_lib_flags="-leccodes"
        else
            eccodes_lib_flags="-L$eccodes_prefix/lib -leccodes"
        fi
        run_eccodes_test="yes"
    elif test "$eccodes_requested" = "yes"; then
         run_eccodes_test="yes"
    else
        run_eccodes_test="no"
    fi

    #
    # Check eccodes headers/libraries
    #
    if test "$run_eccodes_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$eccodes_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $eccodes_lib_flags"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for ECCODES headers in $eccodes_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <eccodes.h>
                ]],
                [[]]
            )],
            [
            ECCODES_CFLAGS="-shared -I$eccodes_include_dir"
            eccodes_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            eccodes_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$eccodes_header_found" = "yes"; then

            AC_MSG_CHECKING([for ECCODES libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <eccodes.h>
                    ]],
                    [[
grib_context_get_default();
                    ]]
                )],
                [
                ECCODES_LDFLAGS="$eccodes_lib_flags"
                eccodes_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                eccodes_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for ECCODES])

    if test "$run_eccodes_test" = "yes"; then
        if test "$eccodes_header_found" = "yes" -a "$eccodes_lib_found" = "yes"; then

            AC_SUBST([ECCODES_CFLAGS])
            AC_SUBST([ECCODES_LDFLAGS])

            HAVE_ECCODES="yes"
        else 
            HAVE_ECCODES="no"
        fi

        AC_MSG_RESULT([$HAVE_ECCODES])

    else
        HAVE_ECCODES="no"
        AC_MSG_RESULT([$HAVE_ECCODES])

        if test "$eccodes_requested" = "yes"; then
            AC_MSG_WARN([ECCODES support requested but headers or library not found. Specify valid prefix of libeccodes using --with-eccodes=@<:@DIR@:>@ or provide include directory and linker flags using --with-eccodes-inc and --with-eccodes-lib])
        fi
    fi
])
