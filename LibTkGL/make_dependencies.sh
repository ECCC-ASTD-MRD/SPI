#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m | tr _ -`
VERSION=7.7.1

echo "Architecture: ${ARCH}_${PROC}"

#----- Location of the library source codes
ARCH_PATH=/cnfs/ops/cmoe/afsr005/Archive

#----- Where to install libraries
LIB_PATH=/cnfs/ops/cmoe/afsr005/Lib/${VERSION}/${ORDENV_PLAT}

SPI_LIB=/cnfs/ops/cmoe/afsr005/Lib/${VERSION}/${ORDENV_PLAT}/SPI
SPI_PATH=/users/dor/afsr/005/Projects/eerSPI/eer_SPI

export LD_LIBRARY_PATH=${SPI_LIB}:$LD_LIBRARY_PATH

TCL_VERSION=8.6.0
TCL_LIB=1.14
TKTABLE=Tktable2.10
F90=/home/afsr/005/Projects/RMN/f90tcl

XML=libxml2-2.9.1
TDOM=tdom-master
EXPAT=expat-2.0.1
CURL=curl-7.21.3
SQLITE=sqlite-3.6.23
GEOS=geos-3.4.0
GDAL=gdal-1.11.0
MYSQL=mysql-5.1
JASPER=jasper-1.900.1
HDF4=hdf-4.2.5
SZIP=szip-2.1
HDF5=hdf5-1.8.11
POSTGRESQL=postgresql-8.4.1
ODBC=unixODBC-2.3.0
GRIB=grib_api-1.12.0
ECBUFR=libecbufr-0.8.2rc1
OCI=instantclient_11_2
FGDB=FileGDB_API
PROJ=proj-4.8.0
MESA=Mesa-7.6.1

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

if [[ $ARCH == "AIX" ]]; then
   export CC=xlc
   export make=gmake
fi

mkdir -p ${LIB_PATH}
mkdir -p ${SPI_LIB}

#----- Mesa
cd ${ARCH_PATH}/${MESA}
make distclean
./configure --prefix=${SPI_LIB}/GL --disable-gallium --with-x --with-driver=xlib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tcl Specifics

#----- Tcl
cd ${ARCH_PATH}/tcl${TCL_VERSION}/unix
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads  --enable-64bit=${x64}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tk
#----- Remove visibility-hidden flag from makefile for glCanvas to work
#----- TkImgPhoto.c patch dans ImgGetPhoto - alphaOffset=blockPtr->offset[3];

cd ${ARCH_PATH}/tk${TCL_VERSION}/unix
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib --enable-xft=no
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- f90 stuff (for RMN with fortran)
#cd ${F90}/
#export FC='pgf90 -Bdynamic -fPIC -byteswapio'
#s.cc -c -I${SPI_LIB}/TclTk/include f90tclsh.c f90wish.c
#./f90_call_c.sh f90tclsh f90tclsh.o -L${SPI_LIB}/TclTk/lib -ltcl8.6
#rm f90tclsh.o
#./f90_call_c.sh f90wish f90wish.o -L${SPI_LIB}/TclTk/lib -ltk8.6 -ltcl8.6
#rm f90wish.o
#ln -fs f90tclsh tclsh8.6
#ln -fs f90wish  wish8.6
#cp -d f90tclsh f90wish ${SPI_LIB}/TclTk/bin

#----- TkTable
cd ${ARCH_PATH}/${TKTABLE}
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib
make clean
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
cp -d ${LIB_PATH}/${XML}/lib/*.so* ${SPI_LIB}

#----- tDOM
cd ${ARCH_PATH}/${TDOM}
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tclinclude=${SPI_LIB}/TclTk/include --with-tcl=${SPI_LIB}/TclTk/lib
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
cp -d ${LIB_PATH}/${EXPAT}/lib/*.so* ${SPI_LIB}

#----- curl
cd ${ARCH_PATH}/${CURL}
make distclean
./configure --prefix=${LIB_PATH}/${CURL} --enable-shared=no --without-libssh2 --without-ssl
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
cp -d ${LIB_PATH}/${SQLITE}/lib/*.so* ${SPI_LIB}

#----- geos
cd ${ARCH_PATH}/${GEOS}
make distclean
./configure --prefix=${LIB_PATH}/${GEOS} --enable-shared --enable-python=no --enable-ruby=no --with-gnu-ld=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${GEOS}/lib/*.so* ${SPI_LIB}

#----- jasper
cd ${ARCH_PATH}/${JASPER}
make distclean
./configure --prefix=${LIB_PATH}/${JASPER} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${JASPER}/lib/*.so* ${SPI_LIB}

#----- HDF-4
cd ${ARCH_PATH}/${HDF4}
make distclean
./configure --prefix=${LIB_PATH}/${HDF4} --enable-shared=yes --disable-netcdf --disable-fortran 
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${HDF4}/lib/*.so* ${SPI_LIB}

#----- SZIP
cd ${ARCH_PATH}/${SZIP}
make distclean
./configure --prefix=${LIB_PATH}/${SZIP} --enable-shared=yes 
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${SZIP}/lib/*.so* ${SPI_LIB}

#----- HDF-5
cd ${ARCH_PATH}/${HDF5}
make distclean
./configure --prefix=${LIB_PATH}/${HDF5} --enable-shared=yes --with-szlib=${LIB_PATH}/${SZIP} --disable-fortran
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${HDF5}/lib/*.so* ${SPI_LIB}

#----- netCDF
cd ${ARCH_PATH}/${NETCDF}
make distclean
./configure --prefix=${LIB_PATH}/${NETCDF} --disable-netcdf-4 --enable-shared=yes --enable-c-only --with-hdf5=${LIB_PATH}/${HDF5} -with-hdf4=${LIB_PATH}/${HDF4}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${NETCDF}/lib/*.so* ${SPI_LIB}

#----- grib
cd ${ARCH_PATH}/${GRIB}
make distclean
./configure --prefix=${LIB_PATH}/${GRIB} --enable-shared=yes --enable-pthread  --with-png-support --with-jasper=${LIB_PATH}/${JASPER}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${GRIB}/lib/*.so* ${SPI_LIB}
cp -r ${LIB_PATH}/${GRIB}/share/grib_api/definitions ${SPI_PATH}/share/grib

#----- ECBUFR
cd ${ARCH_PATH}/${ECBUFR}
make distclean
./configure --prefix=${LIB_PATH}/${ECBUFR} --enable-shared=yes
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${ECBUFR}/lib/libecbufr0.8.2rc1/*.so* ${SPI_LIB}

#----- PostgreSQL
#cd ${ARCH_PATH}/${POSTGRESQL}
#make distclean
#./configure --prefix=${LIB_PATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
#make install
#if [[ $? -ne 0 ]] ; then
#   exit 1
#fi
#cp -d ${LIB_PATH}/${POSTGRESQL}/lib/*.so* ${SPI_LIB}

#----- ODBC
cd ${ARCH_PATH}/${ODBC}
make distclean
./configure --prefix=${LIB_PATH}/${ODBC}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${ODBC}/lib/*.so* ${SPI_LIB}

#----- PROJ 4
cd ${ARCH_PATH}/${PROJ}
make distclean
./configure --prefix=${LIB_PATH}/${PROJ}
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${PROJ}/lib/*.so* ${SPI_LIB}

#----- gdal (Don't forget to add stdio.h to frmts/msg/msgcommand.h)
export LD_LIBRARY_PATH={LIB_PATH}/${GEOS}/lib:$LD_LIBRARY_PATH

cd ${ARCH_PATH}/${GDAL}
make distclean
./configure --prefix=${LIB_PATH}/${GDAL} --with-threads=yes --disable-rpath \
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
--with-hdf5=${LIB_PATH}/${HDF5} \
--with-pg=${LIB_PATH}/${POSTGRESQL}/bin/pg_config \
--with-odbc=${LIB_PATH}/${ODBC} \
--with-netcdf=${LIB_PATH}/${NETCDF} \
--with-oci-lib=${LIB_PATH}/${OCI} \
--with-oci-include=${LIB_PATH}/${OCI}/sdk/include \
--with-fgdb=${LIB_PATH}/${FGDB} \
--with-static-proj4=${LIB_PATH}/${PROJ}

make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${GDAL}/lib/*.so* ${SPI_LIB}
cp -d ${LIB_PATH}/${GDAL}/share/gdal/* ${SPI_PATH}/share/gdal

#--with-hdf4=${LIB_PATH}/${HDF4} \
#--with-kakadu=${LIB_PATH}/${KKDU}
#--with-ecw=/cnfs/ops/cmoe/afsr005/Lib/Linux/libecwj2-3.3
