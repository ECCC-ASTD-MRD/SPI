#
# GDAL_INIT (MINIMUM_VERSION)
#
# Test for GDAL: define HAVE_GDAL, GDAL_LIBS, GDAL_CFLAGS, GDAL_VERSION
# 
# Call as GDAL_INIT or GDAL_INIT(minimum version) in configure.in. Test
# HAVE_GDAL (yes|no) afterwards. If yes, all other vars above can be 
# used in program.
#

AC_DEFUN([GDAL_INIT],[
  AC_SUBST(GDAL_LIBS)
  AC_SUBST(GDAL_CFLAGS)
  AC_SUBST(HAVE_GDAL) 
  AC_SUBST(GDAL_VERSION)

  AC_ARG_WITH(gdal,
    AS_HELP_STRING([--with-gdal[=ARG]],
                   [Include GDAL support (ARG=yes, no or gdal-config path)]),,)

  ac_gdal_config_auto=no

  if test x"$with_gdal" = x"no" ; then

    AC_MSG_RESULT([GDAL support disabled])
    GDAL_CONFIG=no

  elif test x"$with_gdal" = x"yes" -o x"$with_gdal" = x"" ; then

    AC_PATH_PROG(GDAL_CONFIG, gdal-config, no)
    ac_gdal_config_auto=yes

  else

   ac_gdal_config=`basename "$with_gdal"`
   ac_gdal_config_dir=`AS_DIRNAME(["$with_gdal"])`

   AC_CHECK_PROG(
        GDAL_CONFIG,
        "$ac_gdal_config",
        $with_gdal,
        [no],
        ["$ac_gdal_config_dir"],
        []
   )

  fi

  if test x"$GDAL_CONFIG" != x"no" ; then

    min_gdal_version=ifelse([$1], ,1.0.0,$1)

    AC_MSG_CHECKING(for GDAL version >= $min_gdal_version)

    gdal_major_version=`$GDAL_CONFIG --version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\1/'`
    gdal_minor_version=`$GDAL_CONFIG --version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\2/'`
    gdal_micro_version=`$GDAL_CONFIG --version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\3/'`

    req_major=`echo $min_gdal_version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\1/'`
    req_minor=`echo $min_gdal_version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\2/'`
    req_micro=`echo $min_gdal_version | \
       sed 's/\([[0-9]]*\)\.\([[0-9]]*\)\.\([[0-9]]*\).*/\3/'`
      
    version_ok="no"
    ac_req_version=`expr $req_major \* 100000 \+  $req_minor \* 100 \+ $req_micro`
    ac_gdal_version=`expr $gdal_major_version \* 100000 \+  $gdal_minor_version \* 100 \+ $gdal_micro_version`

    if test $ac_req_version -le $ac_gdal_version; then
        version_ok="yes"
        AC_MSG_RESULT([yes])
    fi

    if test $version_ok = "no"; then

      HAVE_GDAL="no"
      AC_MSG_RESULT(no)

      if test $ac_gdal_config_auto = "yes" ; then
        AC_MSG_WARN([GDAL was found on your system, but gdal-config reports version ${gdal_major_version}.${gdal_minor_version}.${gdal_micro_version}, need at least $min_gdal_version. GDAL support disabled.])
      else
        AC_MSG_ERROR([gdal-config reports version ${gdal_major_version}.${gdal_minor_version}.${gdal_micro_version}, need at least $min_gdal_version or configure --without-gdal])
      fi

    else
      
      HAVE_GDAL="no"

      GDAL_LIBS="`${GDAL_CONFIG} --libs`"
      GDAL_CFLAGS="`${GDAL_CONFIG} --cflags`"
      GDAL_VERSION="`${GDAL_CONFIG} --version`"

      ax_save_LIBS="${LIBS}"
      LIBS=${GDAL_LIBS}
      ax_save_CFLAGS="${CFLAGS}"
      CFLAGS="${GDAL_CFLAGS}"

      AC_CHECK_LIB([gdal],
        [GDALAllRegister],
        [HAVE_GDAL="yes"],
        [HAVE_GDAL="no"],
        [-Wl,--allow-shlib-undefined]
      )

      if test x"$HAVE_GDAL" = "xno"; then
          GDAL_CFLAGS=""
      fi

      CFLAGS="${ax_save_CFLAGS}"
      LIBS="${ax_save_LIBS}"

    fi

  fi
])

