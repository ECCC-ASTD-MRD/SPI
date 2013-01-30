#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m`

echo "Architecture: ${ARCH}_${PROC}"

ARCH_PATH=/cnfs/ops/cmoe/afsr005/Archive
LIB_PATH=/cnfs/ops/cmoe/afsr005/Lib/${ARCH}_${PROC}
TCL_PATH=/home/afsr/005/Projects/eerSPI/eer_SPI/Lib/${ARCH}_${PROC}/TclTk
HOME_PATH=/home/afsr/005/lib/${ARCH}_${PROC}

export LD_LIBRARY_PATH=${HOME_PATH}:$LD_LIBRARY_PATH

TCL_VERSION=8.5.7
THREAD=thread2.6.5
TKTABLE=Tktable2.10
TLS=tls1.5

XML=libxml2-2.7.2
TDOM=tDOM-0.8.2
EXPAT=expat-2.0.1
CURL=curl-7.21.3
SQLITE=sqlite-3.6.23
GEOS=geos-3.2.2
GDAL=gdal-1.9.2
MYSQL=mysql-5.1
JASPER=jasper-1.900.1
HDF4=hdf-4.2.5
HDF5=hdf5-1.6.10
POSTGRESQL=postgresql-8.4.1
ODBC=unixODBC-2.3.0
GRIB=grib_api-1.9.18
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
cd ${ARCH_PATH}/tk${TCL_VERSION}/unix
make distclean
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCL_PATH}/lib --enable-xft=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Thread
cd ${ARCH_PATH}/${THREAD}
./configure --prefix=${TCL_PATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCL_PATH}/lib
make clean
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

#----- tls-1.5.0
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

#----- sqlite-tea
cd ${ARCH_PATH}/${SQLITE}-tea
make distclean
./configure --prefix=${TCL_PATH} --enable-shared --enable-threads --with-tclinclude=${TCL_PATH}/include/ --with-tcl=${TCL_PATH}/lib/
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

#----- jasper
cd ${ARCH_PATH}/${JASPER}
make distclean
./configure --prefix=${LIB_PATH}/${JASPER} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- HDF-4
cd ${ARCH_PATH}/${HDF4}
make distclean
./configure --prefix=${LIB_PATH}/${HDF4} --enable-shared=yes --disable-netcdf --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- HDF-5
cd ${ARCH_PATH}/${HDF5}
make distclean
./configure --prefix=${LIB_PATH}/${HDF5} --enable-shared=yes --enable-hdf5v1_4 --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- netCDF
cd ${ARCH_PATH}/${NETCDF}
make distclean
./configure --prefix=${LIB_PATH}/${NETCDF} --disable-netcdf-4 --enable-shared=yes --enable-c-only --with-hdf5=${LIB_PATH}/${HDF5} -with-hdf4=${LIB_PATH}/${HDF4}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- grib
cd ${ARCH_PATH}/${GRIB}
make distclean
./configure --prefix=${LIB_PATH}/${GRIB} --enable-shared=yes --enable-pthread  --with-png-support --with-jasper=${LIB_PATH}/${JASPER}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- PostgreSQL
cd ${ARCH_PATH}/${POSTGRESQL}
make distclean
./configure --prefix=${LIB_PATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- ODBC
cd ${ARCH_PATH}/${ODBC}
make distclean
./configure --prefix=${LIB_PATH}/${ODBC}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi


#----- PROJ 4
cd ${ARCH_PATH}/${PROJ}
make distclean
./configure --prefix=${LIB_PATH}/${PROJ}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- gdal
cd ${ARCH_PATH}/${GDAL}
make distclean
./configure --prefix=${LIB_PATH}/${GDAL} --with-threads=yes \
--with-libz=internal \
--with-liblzma=yes \
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
--with-oci-lib=${HOME_PATH} \
--with-oci-include=${LIB_PATH}/${OCI}/sdk/include \
--with-static-proj4=${LIB_PATH}/${PROJ}

make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#--with-kakadu=${LIB_PATH}/${KKDU}
#--with-ecw=/cnfs/ops/cmoe/afsr005/Lib/Linux/libecwj2-3.3
