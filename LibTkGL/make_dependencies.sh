#!/bin/sh
ARCH=`uname -s`
PROC=`uname -m | tr _ -`
VERSION=7.8.0

echo "Architecture: ${ARCH}_${PROC}"

#----- Location of the library source codes
ARCH_PATH_INI=/fs/cetus/fs2/ops/cmoe/afsr005/Archive
ARCH_PATH=$ARCH_PATH_INI

#----- Where to install libraries
LIB_PATH_INI=/fs/cetus/fs2/ops/cmoe/afsr005/Lib/${ORDENV_PLAT}
LIB_PATH=$HOME/Links/libfs${VERSION}/${ORDENV_PLAT}

SPI_LIB=$LIB_PATH/SPI
SPI_PATH=$HOME/Projects/SPI/eer_SPI

export LD_LIBRARY_PATH=${SPI_LIB}:$LD_LIBRARY_PATH
export LIBRARY_PATH=${SPI_LIB}:$LIBRARY_PATH

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
ECBUFR=libecbufr-0.8.2rc1
OCI=instantclient_11_2
FGDB=FileGDB_API
PROJ=proj-4.8.0
MESA=Mesa-7.9.2

#----- not recompiled yet
NETCDF=netcdf-4.1.1
KKDU=kakadu-6.3
FLT=fltlib-0.5.1
RMN=librmn-15

#----- Define the list of compiled items and package name that this script will use
ARCH_LIBS="tcl${TCL_VERSION} tk${TCL_VERSION} ${TKIMG} ${TKTABLE} ${TKDND} Tcllib-${TCLLIB} $XML $TDOM $EXPAT $CURL $SQLITE.1 $GEOS $GDAL $JASPER $JPEG $HDF4 $SZIP $HDF5 $POSTGRESQL $ODBC $GRIB $ECBUFR $PROJ $MESA $NETCDF"

#----- Define the list of items we won't compile but will link with
SLINK_LIBS="$FGDB URP $FLT gdb $RMN $MYSQL"

#----- Create output directories

mkdir -p ${LIB_PATH}
mkdir -p ${SPI_LIB}

#----- Either copy missing packages or output an error if they are non-existant

if [ "$1" = "localcopy" ]; then
    set -e

    #----- Only necessary because I can't create the object files (therefore I can't compile) in JP's Archive directory
    if [[ $ARCH_PATH_INI != $ARCH_PATH ]]; then
        #----- Not added : KKDU MYSQL OCI FGDB

        for l in $ARCH_LIBS; do
            if [[ -d $ARCH_PATH/$l && $ARCH_PATH_INI/$l -nt $ARCH_PATH/$l ]]; then
                echo deleting $ARCH_PATH/$l
                #rm -rf "$ARCH_PATH/$l"
            fi
            if [[ ! -d $ARCH_PATH/$l ]]; then
                echo copying $ARCH_PATH_INI/$l to $ARCH_PATH/
                sscp -r -p "$ARCH_PATH_INI/$l" "$ARCH_PATH/"
            fi
        done
    fi

    #----- The library we don't compile doesn't need to be copied, so link them to the original dir

    if [[ $LIB_PATH_INI != $LIB_PATH ]]; then

        for l in $SLINK_LIBS; do
            if [[ ! -e $LIB_PATH/$l ]]; then
                echo "Creating symlink from $LIB_PATH_INI/$l to $LIB_PATH/$l"
                if [[ -e $LIB_PATH_INI/$l ]]; then
                    ln -fsT $LIB_PATH_INI/$l $LIB_PATH/$l
                    cp $LIB_PATH/$l/lib/lib* $SPI_LIB/
                else
                    echo "Library $l could not be found : symlink failed."
                    exit 1
                fi
            fi
        done
    fi

    set +e
else
    #----- Make sure every package we need to compile is available locally

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
   export make=gmake
fi

#----- Mesa
cd ${ARCH_PATH}/${MESA}
make distclean
./configure --prefix=${SPI_LIB}/GL --disable-gallium --with-x --with-driver=xlib --disable-driglx-direct
make
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
cd ${ARCH_PATH}/tk${TCL_VERSION}/unix
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib --enable-xft=no

#----- Remove visibility-hidden flag from makefile for glCanvas to work
mv Makefile Makefile.hidden
sed 's/-DMODULE_SCOPE=\(\\ \|[^ ]\)*[^\\] //' <Makefile.hidden >Makefile
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- TkImg
cd ${ARCH_PATH}/${TKIMG}
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib --with-tk=${SPI_LIB}/TclTk/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- TkTable
cd ${ARCH_PATH}/${TKTABLE}
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib
make clean
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

#----- Tkdnd
cd ${ARCH_PATH}/${TKDND}
make distclean
./configure --prefix=${SPI_LIB}/TclTk --enable-threads --enable-64bit=${x64} --with-tcl=${SPI_LIB}/TclTk/lib
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi

cd ${ARCH_PATH}/Tcllib-${TCLLIB}
./installer.tcl -no-gui -no-nroff -no-examples -no-apps -no-wait -pkg-path ${SPI_LIB}/TclTk/lib/tcllib${TCLLIB}

#----- libxml2
cd ${ARCH_PATH}/${XML}
make distclean
./configure --prefix=${LIB_PATH}/${XML} --enable-64bit=${x64} --without-python
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

#----- JPEG
cd ${ARCH_PATH}/${JPEG}
make clean
make -f makefile.ansi
mkdir -p ${LIB_PATH}/${JPEG}/lib
mkdir -p ${LIB_PATH}/${JPEG}/include

cp libjpeg.a ${LIB_PATH}/${JPEG}/lib
cp *.h ${LIB_PATH}/${JPEG}/include

#----- HDF-4
cd ${ARCH_PATH}/${HDF4}
make distclean
./configure --prefix=${LIB_PATH}/${HDF4} --enable-shared=yes --disable-netcdf --disable-fortran --with-jpeg=${LIB_PATH}/${JPEG}
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
mkdir -p ${SPI_PATH}/share/grib
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
cd ${ARCH_PATH}/${POSTGRESQL}
make distclean
#./configure --prefix=${LIB_PATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-libxslt
./configure --prefix=${LIB_PATH}/${POSTGRESQL} --enable-shared --disable-rpath  --without-readline --enable-thread-safety --with-openssl --with-libxml --with-includes=${ARCH_PATH}/${XML}/include
make install
if [[ $? -ne 0 ]] ; then
   exit 1
fi
cp -d ${LIB_PATH}/${POSTGRESQL}/lib/*.so* ${SPI_LIB}

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
--with-hdf4=${LIB_PATH}/${HDF4} \
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
mkdir -p ${SPI_PATH}/share/gdal
cp -d ${LIB_PATH}/${GDAL}/share/gdal/* ${SPI_PATH}/share/gdal

#--with-kakadu=${LIB_PATH}/${KKDU}
#--with-ecw=/users/dor/afsr/005/Links/dev/Lib/Linux/libecwj2-3.3
