#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m`

echo "Architecture: ${ARCH}_${PROC}"

ARCHPATH=/cnfs/ops/cmoe/afsr005/Archive
LIBPATH=/cnfs/ops/cmoe/afsr005/Lib/${ARCH}_${PROC}
TCLPATH=/home/afsr/005/Projects/eerSPI/eer_SPI/Lib/${ARCH}_${PROC}/TclTk
HOMEPATH=/home/afsr/005/lib/${ARCH}_${PROC}

export LD_LIBRARY_PATH=${HOMEPATH}:$LD_LIBRARY_PATH

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
cd ${ARCHPATH}/tcl${TCL_VERSION}/unix
make distclean
./configure --prefix=${TCLPATH} --enable-threads  --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tk
cd ${ARCHPATH}/tk${TCL_VERSION}/unix
make distclean
./configure --prefix=${TCLPATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCLPATH}/lib --enable-xft=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Thread
cd ${ARCHPATH}/${THREAD}
./configure --prefix=${TCLPATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCLPATH}/lib
make clean
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- TkTable
cd ${ARCHPATH}/${TKTABLE}
./configure --prefix=${TCLPATH} --enable-threads --enable-64bit=${x64} --with-tcl=${TCLPATH}/lib
make clean
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- tls-1.5.0
cd ${ARCHPATH}/${TLS}
make distclean
./configure --prefix=${TCLPATH} --enable-threads --enable-64bit=${x64} --with-ssl-dir=/usr --with-tcl=${TCLPATH}/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- libxml2
cd ${ARCHPATH}/${XML}
make distclean
./configure --prefix=${LIBPATH}/${XML} --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- tDOM-0.8.2
cd ${ARCHPATH}/${TDOM}
make distclean
./configure --prefix=${TCLPATH} --enable-threads --enable-64bit=${x64} --with-tclinclude=${TCLPATH}/include --with-tcl=${TCLPATH}/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- GDAL Specifics

#----- expath
cd ${ARCHPATH}/${EXPAT}
make distclean
./configure --prefix=${LIBPATH}/${EXPAT} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- curl
cd ${ARCHPATH}/${CURL}
make distclean
./configure --prefix=${LIBPATH}/${CURL} --enable-shared=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- sqlite
cd ${ARCHPATH}/${SQLITE}.1
make distclean
./configure --prefix=${LIBPATH}/${SQLITE} --enable-shared=yes --enable-threadsafe
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- sqlite-tea
cd ${ARCHPATH}/${SQLITE}-tea
make distclean
./configure --prefix=${TCLPATH} --enable-shared --enable-threads --with-tclinclude=${TCLPATH}/include/ --with-tcl=${TCLPATH}/lib/
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- geos
cd ${ARCHPATH}/${GEOS}
make distclean
./configure --prefix=${LIBPATH}/${GEOS} --enable-shared --enable-python=no --enable-ruby=no --with-gnu-ld=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- jasper
cd ${ARCHPATH}/${JASPER}
make distclean
./configure --prefix=${LIBPATH}/${JASPER} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- HDF-4
cd ${ARCHPATH}/${HDF4}
make distclean
./configure --prefix=${LIBPATH}/${HDF4} --enable-shared=yes --disable-netcdf --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- HDF-5
cd ${ARCHPATH}/${HDF5}
make distclean
./configure --prefix=${LIBPATH}/${HDF5} --enable-shared=yes --enable-hdf5v1_4 --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- netCDF
cd ${ARCHPATH}/${NETCDF}
make distclean
./configure --prefix=${LIBPATH}/${NETCDF} --disable-netcdf-4 --enable-shared=yes --enable-c-only --with-hdf5=${LIBPATH}/${HDF5} -with-hdf4=${LIBPATH}/${HDF4}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- grib
cd ${ARCHPATH}/${GRIB}
make distclean
./configure --prefix=${LIBPATH}/${GRIB} --enable-shared=yes --enable-pthread  --with-png-support --with-jasper=${LIBPATH}/${JASPER}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- PostgreSQL
cd ${ARCHPATH}/${POSTGRESQL}
make distclean
./configure --prefix=${LIBPATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- ODBC
cd ${ARCHPATH}/${ODBC}
make distclean
./configure --prefix=${LIBPATH}/${ODBC}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi


#----- gdal
cd ${ARCHPATH}/${GDAL}
make distclean
./configure --prefix=${LIBPATH}/${GDAL} --with-threads=yes \
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
--with-jasper=${LIBPATH}/${JASPER} \
--with-mysql=${LIBPATH}/${MYSQL}/bin/mysql_config \
--with-expat=${LIBPATH}/${EXPAT} \
--with-curl=${LIBPATH}/${CURL}/bin/curl-config \
--with-sqlite3=${LIBPATH}/${SQLITE} \
--with-geos=${LIBPATH}/${GEOS}/bin/geos-config \
--with-hdf4=${LIBPATH}/${HDF4} \
--with-hdf5=${LIBPATH}/${HDF5} \
--with-pg=${LIBPATH}/${POSTGRESQL}/bin/pg_config \
--with-odbc=${LIBPATH}/${ODBC} \
--with-netcdf=${LIBPATH}/${NETCDF} \
--with-oci-lib=${HOMEPATH} \
--with-oci-include=${LIBPATH}/${OCI}/sdk/include

make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#--with-kakadu=${LIBPATH}/${KKDU}
#--with-ecw=/cnfs/ops/cmoe/afsr005/Lib/Linux/libecwj2-3.3
