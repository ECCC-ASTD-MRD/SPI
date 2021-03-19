# SPI Description

SPI is a scientific and meteorological virtual globe offering immense processing, analysis and visualization capabilities, with a user interface similar to Google Earth and NASA World Wind. SPI uses an "always 3D" rendering method. Everything is always rendered in three dimensions, looking from the top it looks like a regular 2D map, but moving the camera around gives a new perspective of things.

SPI is built in Tcl/Tk. Most of the rendering and scientific functionalities are built as binary extensions to Tcl in the form of objects and functions and then used in SPI’s Tcl source code.

An exhaustive API described in another document allows for very powerful scripting, going from generation of a map to calculating flux along the path of a shape like country boundary. This API can be used to interact with many kind of data like RPN standard file, CMC trajectories, ESRI shapefiles, GeoTIFF imagery, BURP observation files and more,.


# [SPI Documentation](https://wiki.cmc.ec.gc.ca/wiki/SPI)
# [SPI API Documentation](https://wiki.cmc.ec.gc.ca/wiki/SPI/Documentation#Developer_documentation)

# Getting the source code
```shell
git clone --recursive git@gitlab.science.gc.ca:ECCC_CMOE_APPS/SPI 
```

# Building SPI
You will need cmake with a version at least 3.16
```shell
. ssmuse-sh -x /fs/ssm/main/opt/cmake-3.16.4
```

## Dependencies
There are many dependencies to build SPI

### Optional dependencies (On ECCC/SCIENCE network)
[codetools](https://gitlab.science.gc.ca/RPN-SI/code-tools)
```shell
. r.load.dot rpn/codetools/1.5.1
```

[librmn](https://gitlab.science.gc.ca/RPN-SI/librmn)
```shell
. r.load.dot rpn/libs/19.7.0
```

[vgrid](https://gitlab.science.gc.ca/RPN-SI/vgrid)
```shell
. r.load.dot rpn/vgrid/6.5.0
```

External dependencies (GDAL,URP,ECCODES,LIBECBUFR,...). Within the ECCC/SCIENCE network, a package containing all the dependencies cna be loaded
```shell
export CMD_EXT_PATH=/fs/ssm/eccc/cmd/cmds/ext/20210211; . ssmuse-sh -x $CMD_EXT_PATH
```

### Mandatory dependencies
Even though you could load an SSM package, you should build [libeerUtils](https://gitlab.science.gc.ca/ECCC_CMOE_MODELS/libeerutils) first as it is intimely tied to SPI


### Environment setup
The build process requires the definition of a variable indicating where the build will occur
```shell
export SSM_DEV=[where to build]
mkdir -p $SSM_DEV/src $SSM_DEV/package $SSM_DEV/workspace $SSM_DEV/build
```

### Launching the build
```shell
cd libSPI
./makeit -ext
./makeit -reconf -build -ssm
```

# Building source package for distribution

To build a package for external distribution, use the root makeit script.

```shell
makeit -src
```

This will build a source package including SPI, [GenPhysX](https://gitlab.science.gc.ca/ECCC_CMOE_APPS/genphysx) and [libeerUtils](https://gitlab.science.gc.ca/ECCC_CMOE_MODELS/libeerutils).


# SPI test suite

# Automatic Testing using GitLab-CI

An automatic system of tests has been developed.  For each push in the
`master` branch the system tests are launched to guarantee that the
all the tests pass for the `master` branch.