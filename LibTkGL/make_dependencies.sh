#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m | tr _ -`

echo "Architecture: ${ARCH}_${PROC}"

#----- Location of the library source codes
ARCH_PATH=/cnfs/ops/cmoe/afsr005/Archive

#----- Where to install libraries
LIB_PATH=/cnfs/ops/cmoe/afsr005/Lib/${ARCH}_${PROC}

#----- Where to install Tcl/Tk
TCL_PATH=/home/afsr/005/Projects/eerSPI/eer_SPI/Lib/${ARCH}_${PROC}/TclTk


HOME_PATH=/home/afsr/005/lib/${ARCH}_${PROC}

export LD_LIBRARY_PATH=${HOME_PATH}:$LD_LIBRARY_PATH

TCL_VERSION=8.6.0
TCL_LIB=1.14
TKTABLE=Tktable2.10
TLS=tls1.6

XML=libxml2-2.7.2
TDOM=tdom-master
EXPAT=expat-2.0.1
CURL=curl-7.21.3
SQLITE=sqlite-3.6.23
GEOS=geos-3.2.2
GDAL=gdal-1.10.0
MYSQL=mysql-5.1
JASPER=jasper-1.900.1
HDF4=hdf-4.2.5
SZIP=szip-2.1
HDF5=hdf5-1.8.11
POSTGRESQL=postgresql-8.4.1
ODBC=unixODBC-2.3.0
GRIB=grib_api-1.9.18
ECBUFR=libecbufr-0.8.2rc1
OCI=instantclient_11_2
PROJ=proj-4.8.0

#----- not recompiled yet
NETCDF=netcdf-4.1.1
KKDU=kakadu-6.3

if [[ $PROC == "x86_64" ]]; then
   x64=yes
   export CFLAGS=-m64
   export CXXFLAGS=-m64
else
   x64=no
fi

#----- Tcl Specifics

#----- Tcl
cd ${ARCH_PATH}/tcl${TCL_VERSION}/unix
make distclean
./configure --prefix=${TCL_PATH} --enable-threads  --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tk
#----- Remove visibility-hidden flag from makefile for glCanvas to work
#----- TkImgPhoto.c patch dans ImgGetPhoto - alphaOffset=blockPtr->offset[3];

cd ${ARCH_PATH}/tk${TCL_VERSION}/unix
make distclean
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCL_PATH}/lib --enable-xft=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- TkTable
cd ${ARCH_PATH}/${TKTABLE}
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCL_PATH}/lib
make clean
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- tls
cd ${ARCH_PATH}/${TLS}
make distclean
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-ssl-dir=/usr --with-tcl=${TCL_PATH}/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- libxml2
cd ${ARCH_PATH}/${XML}
make distclean
./configure --prefix=${LIB_PATH}/${XML} --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- tDOM-0.8.2
cd ${ARCH_PATH}/${TDOM}
make distclean
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-tclinclude=${TCL_PATH}/include --with-tcl=${TCL_PATH}/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- GDAL Specifics

#----- expath
cd ${ARCH_PATH}/${EXPAT}
make distclean
./configure --prefix=${LIB_PATH}/${EXPAT} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${EXPAT}/lib/*.so* ${HOME_PATH}

#----- curl
cd ${ARCH_PATH}/${CURL}
make distclean
./configure --prefix=${LIB_PATH}/${CURL} --enable-shared=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- sqlite
cd ${ARCH_PATH}/${SQLITE}.1
make distclean
./configure --prefix=${LIB_PATH}/${SQLITE} --enable-shared=yes --enable-threadsafe
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- geos
cd ${ARCH_PATH}/${GEOS}
make distclean
./configure --prefix=${LIB_PATH}/${GEOS} --enable-shared --enable-python=no --enable-ruby=no --with-gnu-ld=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${GEOS}/lib/*.so* ${HOME_PATH}

#----- jasper
cd ${ARCH_PATH}/${JASPER}
make distclean
./configure --prefix=${LIB_PATH}/${JASPER} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${JASPER}/lib/*.so* ${HOME_PATH}

#----- HDF-4
cd ${ARCH_PATH}/${HDF4}
make distclean
./configure --prefix=${LIB_PATH}/${HDF4} --enable-shared=yes --disable-netcdf --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${HDF4}/lib/*.so* ${HOME_PATH}

#----- SZIP
cd ${ARCH_PATH}/${SZIP}
make distclean
./configure --prefix=${LIB_PATH}/${SZIP} --enable-shared=yes 
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${SZIP}/lib/*.so* ${HOME_PATH}

#----- HDF-5
cd ${ARCH_PATH}/${HDF5}
make distclean
./configure --prefix=${LIB_PATH}/${HDF5} --enable-shared=yes --with-szlib=${LIB_PATH}/${SZIP} --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${HDF5}/lib/*.so* ${HOME_PATH}

#----- netCDF
cd ${ARCH_PATH}/${NETCDF}
make distclean
./configure --prefix=${LIB_PATH}/${NETCDF} --disable-netcdf-4 --enable-shared=yes --enable-c-only --with-hdf5=${LIB_PATH}/${HDF5} -with-hdf4=${LIB_PATH}/${HDF4}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${NETCDF}/lib/*.so* ${HOME_PATH}

#----- grib
cd ${ARCH_PATH}/${GRIB}
make distclean
./configure --prefix=${LIB_PATH}/${GRIB} --enable-shared=yes --enable-pthread  --with-png-support --with-jasper=${LIB_PATH}/${JASPER}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${GRIB}/lib/*.so* ${HOME_PATH}
cp -r {LIB_PATH}/${GRIB}/share/grib_api/definitions ${HOME_PATH}/../Data/grib

#----- ECBUFR
cd ${ARCH_PATH}/${ECBUFR}
make distclean
./configure --prefix=${LIB_PATH}/${ECBUFR} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${ECBUFR}/lib/*.so* ${HOME_PATH}

#----- PostgreSQL
cd ${ARCH_PATH}/${POSTGRESQL}
make distclean
./configure --prefix=${LIB_PATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${POSTGRESQL}/lib/*.so* ${HOME_PATH}

#----- ODBC
cd ${ARCH_PATH}/${ODBC}
make distclean
./configure --prefix=${LIB_PATH}/${ODBC}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${ODBC}/lib/*.so* ${HOME_PATH}

#----- PROJ 4
cd ${ARCH_PATH}/${PROJ}
make distclean
./configure --prefix=${LIB_PATH}/${PROJ}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${PROJ}/lib/*.so* ${HOME_PATH}

#----- gdal (Don't forget to patch histogram for nodata and add stdio.h to frmts/msg/msgcommand.h)
cd ${ARCH_PATH}/${GDAL}
make distclean
./configure --prefix=${LIB_PATH}/${GDAL} --with-threads=yes \
--with-libz=internal \
--with-liblzma=yes \
--with-xml2=${LIB_PATH}/${XML}/bin \
--with-pcidsk=internal \
--with-pcraster=internal \
--with-png=internal \
--with-libtiff=internal \
--with-geotiff=internal \
--with-jpeg=internal \
--with-gif=internal \
--with-msg=yes \
--with-jasper=${LIB_PATH}/${JASPER} \
--with-mysql=${LIB_PATH}/${MYSQL}/bin/mysql_config \
--with-expat=${LIB_PATH}/${EXPAT} \
--with-curl=${LIB_PATH}/${CURL}/bin/curl-config \
--with-sqlite3=${LIB_PATH}/${SQLITE} \
--with-geos=${LIB_PATH}/${GEOS}/bin/geos-config \
--with-hdf4=${LIB_PATH}/${HDF4} \
--with-hdf5=${LIB_PATH}/${HDF5} \
--with-pg=${LIB_PATH}/${POSTGRESQL}/bin/pg_config \
--with-odbc=${LIB_PATH}/${ODBC} \
--with-netcdf=${LIB_PATH}/${NETCDF} \
--with-oci-lib=${LIB_PATH}/${OCI} \
--with-oci-include=${LIB_PATH}/${OCI}/sdk/include \
--with-static-proj4=${LIB_PATH}/${PROJ}

make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d {LIB_PATH}/${GDAL}/lib/*.so* ${HOME_PATH}
cp -d {LIB_PATH}/${GDAL}/share/gdal/* ${HOME_PATH}/../Data/gdal

#--with-kakadu=${LIB_PATH}/${KKDU}
#--with-ecw=/cnfs/ops/cmoe/afsr005/Lib/Linux/libecwj2-3.3
