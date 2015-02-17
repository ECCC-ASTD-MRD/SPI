#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m | tr _ -`
VERSION=7.9.0

echo "Architecture: ${ARCH}_${PROC}"

#----- Location of the library source code
ARCH_PATH_OPS=/users/dor/afsr/ops/Links/devfs/Archive
ARCH_PATH=$ARCH_PATH_OPS
#ARCH_PATH=$HOME/data/Archive

#----- Location of libraries we won't compile
LIB_PATH_OPS=/users/dor/afsr/ops/Links/devfs/Lib/${ORDENV_PLAT}
#LIB_PATH_OPS=/users/dor/afsr/ops/Links/devfs_cnfs/Lib/${ORDENV_PLAT}

#----- Where to install libraries
LIB_PATH=$HOME/Links/libfs${VERSION}/depends/${ORDENV_PLAT}

#------ Temporary directory where the object files will be compiled
TMP_PATH=${TMPBASE-/tmp/$USER}/$$/${ORDENV_PLAT}

export LD_LIBRARY_PATH=${LIB_PATH}/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=${LIB_PATH}/lib:$LIBRARY_PATH

TCL_VERSION=8.6.3
TCLLIB=1.16
TKIMG=tkimg1.4
TKTABLE=Tktable2.10
TKDND=tkdnd2.7

XML=libxml2-2.9.1
TDOM=tdom-master
EXPAT=expat-2.0.1
CURL=curl-7.21.3
SQLITE=sqlite-3.6.23
GEOS=geos-3.4.0
GDAL=gdal-1.11.0
MYSQL=mysql-5.1
JASPER=jasper-1.900.1
JPEG=jpeg-6b
HDF4=hdf-4.2.5
SZIP=szip-2.1
HDF5=hdf5-1.8.11
POSTGRESQL=postgresql-8.4.1
ODBC=unixODBC-2.3.0
GRIB=grib_api-1.12.0
ECBUFR=libecbufr0.8.2rc1
OCI=instantclient_11_2
FGDB=FileGDB_API
PROJ=proj-4.8.0
MESA=Mesa-7.9.2

#----- not recompiled yet
NETCDF=netcdf-4.1.1
KKDU=kakadu-6.3
FLT=fltlib-0.5.1
RMN=librmn-15.1

#----- Define the list of compiled items and package name that this script will use
ARCH_LIBS="tcl${TCL_VERSION} tk${TCL_VERSION} ${TKIMG} ${TKTABLE} ${TKDND} Tcllib-${TCLLIB} $XML $TDOM $EXPAT $CURL $SQLITE.1 $GEOS $GDAL $JASPER $JPEG $HDF4 $SZIP $HDF5 $POSTGRESQL $ODBC $GRIB $ECBUFR $PROJ $MESA $NETCDF"

#----- Define the list of items we won't compile but will link with
if [[ $ARCH != "AIX" ]]; then
   SLINK_LIBS="$FGDB URP $FLT gdb $MYSQL $OCI"
fi

#----- Create output directories
rm -f -r ${LIB_PATH}
mkdir -p ${LIB_PATH}/lib ${LIB_PATH}/include

[ -e "${TMP_PATH}" ] && rm -rf "${TMP_PATH}"
mkdir -p "${TMP_PATH}"
echo "Temporary directory is [$TMP_PATH]"

#----- Either copy missing packages or output an error if they are non-existant

if [ "$1" = "localcopy" ]; then
#   set -e

   #----- Only necessary If using a local cache

   if [[ $ARCH_PATH_OPS != $ARCH_PATH ]]; then

      for l in $ARCH_LIBS; do
         if [[ -d $ARCH_PATH/$l && $ARCH_PATH_OPS/$l -nt $ARCH_PATH/$l ]]; then
            echo deleting $ARCH_PATH/$l
                #rm -rf "$ARCH_PATH/$l"
         fi
         if [[ ! -d $ARCH_PATH/$l ]]; then
            echo "Copying $ARCH_PATH_OPS/$l to $ARCH_PATH/"
            sscp -r -p "$ARCH_PATH_OPS/$l" "$ARCH_PATH/"
         fi
      done
   fi

   #----- The library we don't compile doesn't need to be copied, so link them to the original dir

   if [[ $LIB_PATH_OPS != $LIB_PATH ]]; then
 
      for l in $SLINK_LIBS; do
         if [[ -e $LIB_PATH_OPS/$l ]]; then
            echo "Copying precompiled $LIB_PATH/$l -> $LIB_PATH"
            cp -frd $LIB_PATH_OPS/$l/lib/*.so* $LIB_PATH/lib
            if [[ -e $LIB_PATH_OPS/$l/include ]]; then
               cp -frd $LIB_PATH_OPS/$l/include/* $LIB_PATH/include
            fi
         else
            echo "Library $l could not be found : symlink failed."
            exit 1
         fi
      done
   fi

#   set +e
else
   #----- Make sure every package we need to compile is available

   for l in $ARCH_LIBS; do
      if [[ ! -e $ARCH_PATH/$l ]]; then
         echo "Package $ARCH_PATH/$l should exist and doesn't. I strongly suggest checking this script's config."
         exit 1
      fi
   done

   #----- Make sure every library we don't compile is available in the lib path

   for l in $SLINK_LIBS; do
      if [[ ! -e $LIB_PATH/$l ]]; then
         echo "Library $LIB_PATH/$l should exist and doesn't. I strongly suggest checking this script's config."
         exit 1
      fi
   done
fi

if [[ $PROC == "x86_64" ]]; then
   x64=yes
   export CFLAGS=-m64
   export CXXFLAGS=-m64
else
   x64=no
fi

if [[ $ARCH == "AIX" ]]; then
   export CC=xlc
   export CXX=xlc++
   export make=gmake
fi

set -e

#----- RMN, copies libs and BUFR and RPN dictionnaries
cp -frd $LIB_PATH_OPS/$RMN/lib/* $LIB_PATH/lib
cp -frd $LIB_PATH_OPS/$RMN/include/* $LIB_PATH/include
mkdir -p $LIB_PATH/share/rmn
cd $AFSISIO/datafiles/constants/
cp -frd table_{b,d}_bufr table_{b,d}_bufr_{e,f} ops.variable_dictionary.xml dict-2.0.dtd iso-lat1.ent $LIB_PATH/share/rmn

#----- Mesa
mkdir ${TMP_PATH}/${MESA}
cd ${TMP_PATH}/${MESA}
cp -r -p ${ARCH_PATH}/${MESA}/ ${TMP_PATH}/
./configure --prefix=${LIB_PATH}/GL --disable-gallium --with-x --with-driver=xlib --disable-driglx-direct
make
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tcl Specifics

#----- Tcl
mkdir ${TMP_PATH}/tcl${TCL_VERSION}
cd ${TMP_PATH}/tcl${TCL_VERSION}
${ARCH_PATH}/tcl${TCL_VERSION}/unix/configure --prefix=${LIB_PATH}/TCL --enable-threads  --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tk
mkdir ${TMP_PATH}/tk${TCL_VERSION}
cd ${TMP_PATH}/tk${TCL_VERSION}
${ARCH_PATH}/tk${TCL_VERSION}/unix/configure --prefix=${LIB_PATH}/TCL --enable-threads --enable-64bit=${x64} --with-tcl=${LIB_PATH}/TCL/lib --enable-xft=no

#----- Remove visibility-hidden flag from makefile for glCanvas to work
mv Makefile Makefile.hidden
sed 's/-DMODULE_SCOPE=\(\\ \|[^ ]\)*[^\\] //' <Makefile.hidden >Makefile
make install

#----- Tcllib
cd ${ARCH_PATH}/Tcllib-${TCLLIB}
./installer.tcl -no-gui -no-nroff -no-examples -no-apps -no-wait -pkg-path ${LIB_PATH}/TCL/lib/tcllib${TCLLIB}

#----- TkImg
mkdir ${TMP_PATH}/${TKIMG}
cd ${TMP_PATH}/${TKIMG}
${ARCH_PATH}/${TKIMG}/configure --prefix=${LIB_PATH}/TCL --enable-threads --enable-64bit=${x64} --with-tcl=${LIB_PATH}/TCL/lib --with-tk=${LIB_PATH}/TCL/lib
#----- TCLLIBPATH is necessary to make sure tcl script /usr/bin/dtplite executes on a tclsh with a valid doctools package
TCLLIBPATH="${LIB_PATH}/TCL/lib $TCLLIBPATH" make install

#----- TkTable
mkdir ${TMP_PATH}/${TKTABLE}
cd ${TMP_PATH}/${TKTABLE}
${ARCH_PATH}/${TKTABLE}/configure --prefix=${LIB_PATH}/TCL --enable-threads --enable-64bit=${x64} --with-tcl=${LIB_PATH}/TCL/lib
make clean
make install

#----- Tkdnd
mkdir ${TMP_PATH}/${TKDND}
cd ${TMP_PATH}/${TKDND}
${ARCH_PATH}/${TKDND}/configure --prefix=${LIB_PATH}/TCL --enable-threads --enable-64bit=${x64} --with-tcl=${LIB_PATH}/TCL/lib
make install

#----- libxml2
mkdir ${TMP_PATH}/${XML}
cd ${TMP_PATH}/${XML}
${ARCH_PATH}/${XML}/configure --prefix=${LIB_PATH} --enable-64bit=${x64} --without-python
make install

#----- tDOM
mkdir ${TMP_PATH}/${TDOM}
cd ${TMP_PATH}/${TDOM}
${ARCH_PATH}/${TDOM}/configure --prefix=${LIB_PATH}/TCL --enable-threads --enable-64bit=${x64} --with-tclinclude=${LIB_PATH}/TCL/include --with-tcl=${LIB_PATH}/TCL/lib
make install

#----- GDAL Specifics

#----- expat
mkdir ${TMP_PATH}/${EXPAT}
cd ${TMP_PATH}/${EXPAT}
${ARCH_PATH}/${EXPAT}/configure --prefix=${LIB_PATH} --enable-shared=yes
make install

#----- curl
mkdir ${TMP_PATH}/${CURL}
cd ${TMP_PATH}/${CURL}
${ARCH_PATH}/${CURL}/configure --prefix=${LIB_PATH} --enable-shared=no --without-libssh2 --without-ssl
make install

#----- sqlite
mkdir ${TMP_PATH}/${SQLITE}.1
cd ${TMP_PATH}/${SQLITE}.1
${ARCH_PATH}/${SQLITE}.1/configure --prefix=${LIB_PATH} --enable-shared=yes --enable-threadsafe
make install

#----- geos
mkdir ${TMP_PATH}/${GEOS}
cd ${TMP_PATH}/${GEOS}
${ARCH_PATH}/${GEOS}/configure --prefix=${LIB_PATH} --enable-shared --enable-python=no --enable-ruby=no --with-gnu-ld=yes
make install

#----- jasper
mkdir ${TMP_PATH}/${JASPER}
cd ${TMP_PATH}/${JASPER}
CFLAGS="-I${TMP_PATH}/${JASPER}/src/libjasper/include $CFLAGS" ${ARCH_PATH}/${JASPER}/configure --prefix=${LIB_PATH} --enable-shared=yes
make install

#----- JPEG
mkdir ${TMP_PATH}/${JPEG}
cd ${TMP_PATH}/${JPEG}
${ARCH_PATH}/${JPEG}/configure --prefix=${LIB_PATH} --enable-shared --enable-static
make install

#----- HDF-4
mkdir ${TMP_PATH}/${HDF4}
cd ${TMP_PATH}/${HDF4}
${ARCH_PATH}/${HDF4}/configure --prefix=${LIB_PATH} --enable-shared=yes --disable-netcdf --disable-fortran --with-jpeg=${LIB_PATH}
make install

#----- SZIP
mkdir ${TMP_PATH}/${SZIP}
cd ${TMP_PATH}/${SZIP}
${ARCH_PATH}/${SZIP}/configure --prefix=${LIB_PATH} --enable-shared=yes 
make install

#----- HDF-5
mkdir ${TMP_PATH}/${HDF5}
cd ${TMP_PATH}/${HDF5}
${ARCH_PATH}/${HDF5}/configure --prefix=${LIB_PATH} --enable-shared=yes --with-szlib=${LIB_PATH} --disable-fortran
make install

#----- netCDF
mkdir ${TMP_PATH}/${NETCDF}
cd ${TMP_PATH}/${NETCDF}
${ARCH_PATH}/${NETCDF}/configure --prefix=${LIB_PATH} --disable-netcdf-4 --enable-shared=yes --enable-c-only --with-hdf5=${LIB_PATH} -with-hdf4=${LIB_PATH}
make install

#----- grib
mkdir ${TMP_PATH}/${GRIB}
cd ${TMP_PATH}/${GRIB}
${ARCH_PATH}/${GRIB}/configure --prefix=${LIB_PATH} --enable-shared=yes --enable-pthread --with-png-support --with-jasper=${LIB_PATH} \
    FCFLAGS="-I${ARCH_PATH}/${GRIB}/src -I${ARCH_PATH}/${GRIB}/fortran $FCFLAGS" CFLAGS="-I${ARCH_PATH}/${GRIB}/src $CFLAGS"

#----- Because the genius who developped the DEVEL_RULES mechanism never build outside his tree
touch ${TMP_PATH}/${GRIB}/{src,definitions}/dummy.am

#----- Because using variables to locate executables and source files instead of just hoping they magically appear in the current directory is just too difficult
cp -p ${ARCH_PATH}/${GRIB}/tools/grib1to2.txt ${TMP_PATH}/${GRIB}/tools/
cp -p ${ARCH_PATH}/${GRIB}/fortran/create_grib_f90.sh ${TMP_PATH}/${GRIB}/fortran/
cp -p ${ARCH_PATH}/${GRIB}/fortran/grib_f90_*.f90 ${TMP_PATH}/${GRIB}/fortran/

make install

#----- ECBUFR
mkdir ${TMP_PATH}/${ECBUFR}
cd ${TMP_PATH}/${ECBUFR}
${ARCH_PATH}/${ECBUFR}/configure --prefix=${LIB_PATH} --enable-shared=yes CFLAGS="-I${ARCH_PATH}/${ECBUFR}/API/Headers $CFLAGS"
#----- Because the headers won't be copied by the install directive if we don't do that
cp ${ARCH_PATH}/${ECBUFR}/API/Headers/*.h ${TMP_PATH}/${ECBUFR}/API/Headers/
make install

#----- Because ecbufr can't help himself and creates the wrong directory tree by adding an intermediary directory 
mv ${LIB_PATH}/lib/${ECBUFR}/* ${LIB_PATH}/lib/
mv ${LIB_PATH}/include/${ECBUFR}/* ${LIB_PATH}/include
rm -f -r ${LIB_PATH}/lib/${ECBUFR} ${LIB_PATH}/include/${ECBUFR}

#----- PostgreSQL
mkdir ${TMP_PATH}/${POSTGRESQL}
cd ${TMP_PATH}/${POSTGRESQL}
#./configure --prefix=${LIB_PATH} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
${ARCH_PATH}/${POSTGRESQL}/configure --prefix=${LIB_PATH} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-includes=${LIB_PATH}/include/libxml2
make install

#----- ODBC
mkdir ${TMP_PATH}/${ODBC}
cd ${TMP_PATH}/${ODBC}
${ARCH_PATH}/${ODBC}/configure --prefix=${LIB_PATH}
make install

#----- PROJ 4
mkdir ${TMP_PATH}/${PROJ}
cd ${TMP_PATH}/${PROJ}
${ARCH_PATH}/${PROJ}/configure --prefix=${LIB_PATH}
make install

#----- gdal (Don't forget to add stdio.h to frmts/msg/msgcommand.h)
#----- On AIX, have to wrap the int64 definition betweer ifndef _AIX in frmts/gtiff/libtiff/tiff.h, frmtsfit/gstTypes.h, ogr/ogrsf_frmts/shape/shpopen.c
mkdir ${TMP_PATH}/${GDAL}
cd ${TMP_PATH}/${GDAL}
cp -r -p ${ARCH_PATH}/${GDAL} ${TMP_PATH}/
./configure --prefix=${LIB_PATH} --with-threads=yes --disable-rpath \
--with-libz=internal \
--with-liblzma=yes \
--with-xml2=${LIB_PATH}/bin/xml2-config \
--with-pcidsk=internal \
--with-pcraster=internal \
--with-png=internal \
--with-libtiff=internal \
--with-geotiff=internal \
--with-jpeg=internal \
--with-gif=internal \
--with-msg=yes \
--with-jasper=${LIB_PATH} \
--with-mysql=${LIB_PATH_OPS}/${MYSQL}/bin/mysql_config \
--with-expat=${LIB_PATH} \
--with-curl=${LIB_PATH}/bin/curl-config \
--with-sqlite3=${LIB_PATH} \
--with-geos=${LIB_PATH}/bin/geos-config \
--with-hdf4=${LIB_PATH} \
--with-hdf5=${LIB_PATH} \
--with-pg=${LIB_PATH}/bin/pg_config \
--with-odbc=${LIB_PATH} \
--with-netcdf=${LIB_PATH} \
--with-fgdb=${LIB_PATH} \
--with-static-proj4=${LIB_PATH}

make install

#--with-oci-lib=${LIB_PATH}/${OCI} LDFLAGS="-Wl,--no-as-needed $LDFLAGS" \
#--with-oci-include=${LIB_PATH}/${OCI}/sdk/include \
#--with-kakadu=${LIB_PATH}/${KKDU}
#--with-ecw=/users/dor/afsr/005/Links/dev/Lib/Linux/libecwj2-3.3

#----- Delete the temporary dir
rm -rf "${TMP_PATH}"
