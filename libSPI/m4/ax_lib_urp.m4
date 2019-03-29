# This macros checks for urp headers and libraries 
# and defines compilation flags.
# 
# Macro supports following options and their values:
# 1) Single-option usage:
#       --with-urp - yes, no or path to urp installation prefix
# 2) Three-options usage (all options are required):
#       --with-urp=yes
#       --with-urp-inc - path to base directory with  headers
#       --with-urp-lib - linker flags for 
#
# This macro calls:
#
#   AC_SUBST(URP_CFLAGS)
#   AC_SUBST(URP_LDFLAGS)
#   AC_SUBST(URP_VERSION) - only if version requirement is used
#
# And sets:
#
#   HAVE_URP
#
AC_DEFUN([AX_LIB_URP],
[
    AC_ARG_WITH([urp],
        AC_HELP_STRING([--with-urp=@<:@ARG@:>@],
            [use URP from given prefix (ARG=path); check standard prefixes (ARG=yes); disable (ARG=no)]
        ),
        [
        if test "$withval" = "yes"; then
            if test -d /usr/local/include/urp.h ; then 
                urp_prefix=/usr/local
            elif test -d /usr/include/urp.h ; then
                urp_prefix=/usr
            else
                urp_prefix=""
            fi
            urp_requested="yes"
        elif test -d "$withval"; then
            urp_prefix="$withval"
            urp_requested="yes"
        else
            urp_prefix=""
            urp_requested="no"
        fi
        ],
        [
        # Default behavior is implicit yes
        if test -d /usr/local/include/urp.h ; then 
            urp_prefix=/usr/local
        elif test -d /usr/include/urp.h ; then
            urp_prefix=/usr
        else
            urp_prefix="" 
        fi
        ]
    )

    AC_ARG_WITH([urp-inc],
        AC_HELP_STRING([--with-urp-inc=@<:@DIR@:>@],
            [path to URP headers]
        ),
        [urp_include_dir="$withval"],
        [urp_include_dir=""]
    )
    AC_ARG_WITH([urp-lib],
        AC_HELP_STRING([--with-urp-lib=@<:@ARG@:>@],
            [link options for URP libraries]
        ),
        [urp_lib_flags="$withval"],
        [urp_lib_flags=""]
    )

    URP_CFLAGS=""
    URP_LDFLAGS=""
    URP_VERSION=""

    #
    # Collect include/lib paths and flags
    # 
    run_urp_test="no"

    if test -n "$urp_prefix"; then
        urp_include_dir="$urp_prefix/include"
        if test "$urp_prefix" = "/usr"; then
            urp_lib_flags="-lmut -ldrp -lurp -ldsp -lm -lxml2 -lz -lbz2"
        else
            urp_lib_flags="-L$urp_prefix/lib -lmut -ldrp -lurp -ldsp -lm -lxml2 -lz -lbz2"
        fi
        run_urp_test="yes"
    elif test "$urp_requested" = "yes"; then
        urp_lib_flags="-lmut -ldrp -lurp -ldsp -lm -lxml2 -lz -lbz2"
        urp_include_dir="/"
        run_urp_test="yes"
    else
        run_urp_test="no"
    fi

    #
    # Check urp headers/libraries
    #
    if test "$run_urp_test" = "yes"; then

        saved_CPPFLAGS="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$urp_include_dir"

        saved_LIBS="$LIBS"
        LIBS="$LIBS $urp_lib_flags"

        #
        # Check headers
        #
        AC_MSG_CHECKING([for URP headers in $urp_include_dir])

        AC_LANG_PUSH([C])
        AC_COMPILE_IFELSE([
            AC_LANG_PROGRAM(
                [[
@%:@include <URP.h>
@%:@include <drpdecode.h>
@%:@include <radarEqs.h>
@%:@include <GetRadarHeaderParameter.h>
                ]],
                [[]]
            )],
            [
            URP_CFLAGS="-I$urp_include_dir"
            urp_header_found="yes"
            AC_MSG_RESULT([found])
            ],
            [
            urp_header_found="no"
            AC_MSG_RESULT([not found])
            ]
        )
        AC_LANG_POP([C])
        
        #
        # Check libraries
        #
        if test "$urp_header_found" = "yes"; then

            AC_MSG_CHECKING([for URP libraries])

            AC_LANG_PUSH([C])
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM(
                    [[
@%:@include <URP.h>
@%:@include <drpdecode.h>
@%:@include <radarEqs.h>
@%:@include <GetRadarHeaderParameter.h>
                    ]],
                    [[
DecodeRadarData(NULL,0,NULL);
                    ]]
                )],
                [
                URP_LDFLAGS="$urp_lib_flags"
                urp_lib_found="yes"
                AC_MSG_RESULT([found])
                ],
                [
                urp_lib_found="no"
                AC_MSG_RESULT([not found])
                ]
            )
            AC_LANG_POP([C])
        fi

        CPPFLAGS="$saved_CPPFLAGS"
        LIBS="$saved_LIBS"
    fi

    AC_MSG_CHECKING([for URP])

    if test "$run_urp_test" = "yes"; then
        if test "$urp_header_found" = "yes" -a "$urp_lib_found" = "yes"; then

            AC_SUBST([URP_CFLAGS])
            AC_SUBST([URP_LDFLAGS])

            HAVE_URP="yes"
        else 
            HAVE_URP="no"
        fi

        AC_MSG_RESULT([$HAVE_URP])

    else
        HAVE_URP="no"
        AC_MSG_RESULT([$HAVE_URP])

        if test "$urp_requested" = "yes"; then
            AC_MSG_WARN([URP support requested but headers or library not found. Specify valid prefix of liburp using --with-urp=@<:@DIR@:>@ or provide include directory and linker flags using --with-urp-inc and --with-urp-lib])
        fi
    fi
])
