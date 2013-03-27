# This macros checks for EC gdb headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-gdb - yes, no or path to gdb installation prefix
# 2) Three-options usage (all options are required):
#       --with-gdb=yes
#       --with-gdb-inc - path to base directory with  headers
#       --with-gdb-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(GDB_CFLAGS)
#   AC_SUBST(GDB_LDFLAGS)
#   AC_SUBST(GDB_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_GDB
#
AC_DEFUN([AX_LIB_GDB],
[
    AC_ARG_WITH([gdb],
        AC_HELP_STRING([--with-gdb=@<:@ARG@:>@],
            [use EC GDB from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/gdb.h ; then 
                gdb_prefix=/usr/local
            elif test -d /usr/include/gdb. ; then
                gdb_prefix=/usr
            else
                gdb_prefix=""
            fi
            gdb_requested="yes"
        elif test -d "$withval"; then
            gdb_prefix="$withval"
            gdb_requested="yes"
        else
            gdb_prefix=""
            gdb_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/gdb.h ; then 
            gdb_prefix=/usr/local
        elif test -d /usr/include/gdb.h ; then
            gdb_prefix=/usr
        else
            gdb_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([gdb-inc],
        AC_HELP_STRING([--with-gdb-inc=@<:@DIR@:>@],
            [path to EC GDB headers]
        ),
        [gdb_include_dir="$withval"],
        [gdb_include_dir=""]
    )
    AC_ARG_WITH([gdb-lib],
        AC_HELP_STRING([--with-gdb-lib=@<:@ARG@:>@],
            [link options for EC GDB libraries]
        ),
        [gdb_lib_flags="$withval"],
        [gdb_lib_flags=""]
    )

    GDB_CFLAGS=""
    GDB_LDFLAGS=""
    GDB_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_gdb_test="no"

    if test -n "$gdb_prefix"; then
        gdb_include_dir="$gdb_prefix/include"
        if test "$gdb_prefix" = "/usr"; then
            gdb_lib_flags="-lgdb"
        else
            gdb_lib_flags="-L$gdb_prefix/lib -lgdb"
        fi
        run_gdb_test="yes"
    elif test "$gdb_requested" = "yes"; then
        if test -n "$gdb_include_dir" -a -n "$gdb_lib_flags"; then
            run_gdb_test="yes"
        fi
    else
        run_gdb_test="no"
    fi

    #
    # Check gdb headers/libraries
    #
    if test "$run_gdb_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$gdb_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $gdb_lib_flags -ldl"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for EC GDB headers in $gdb_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <gdb.h>
                ]],
                [[]]
            )],
            [
            GDB_CFLAGS="-I$gdb_include_dir"
            gdb_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            gdb_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$gdb_header_found" = "yes"; then

            AC_MSG_CHECKING([for GDB libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <gdb.h>
                    ]],
                    [[
gdb_init();
                    ]]
                )],
                [
                GDB_LDFLAGS="$gdb_lib_flags"
                gdb_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                gdb_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for GDB])

    if test "$run_gdb_test" = "yes"; then
        if test "$gdb_header_found" = "yes" -a "$gdb_lib_found" = "yes"; then

            AC_SUBST([GDB_CFLAGS])
            AC_SUBST([GDB_LDFLAGS])

            HAVE_GDB="yes"
        else 
            HAVE_GDB="no"
        fi

        AC_MSG_RESULT([$HAVE_GDB])

    else
        HAVE_GDB="no"
        AC_MSG_RESULT([$HAVE_GDB])

        if test "$gdb_requested" = "yes"; then
            AC_MSG_WARN([EC GDB support requested but headers or library not found. Specify valid prefix of libgdb using --with-gdb=@<:@DIR@:>@ or provide include directory and linker flags using --with-gdb-inc and --with-gdb-lib])
        fi
    fi
])
