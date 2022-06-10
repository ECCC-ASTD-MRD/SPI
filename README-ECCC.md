# SPI Description

SPI is a scientific and meteorological virtual globe offering simple and comprehensive processing, analysis and visualization capabilities, with a user interface similar to Google Earth and NASA World Wind. SPI uses an "always 3D" rendering method. Everything is always rendered in three dimensions, looking from the top it looks like a regular 2D map, but moving the camera around gives a new perspective of things.

SPI is built in Tcl/Tk. Most of the rendering and scientific functionalities are built as binary extensions to Tcl in the form of objects and functions and then used in SPI’s Tcl source code.

An exhaustive API described in another document allows for very powerful scripting, going from generation of a map to calculating flux along the path of a shape like country boundary. This API can be used to interact with many kind of data like RPN standard file, CMC trajectories, ESRI shapefiles, GeoTIFF imagery, BUFR observation files and more,.

# [SPI Documentation](https://wiki.cmc.ec.gc.ca/wiki/SPI)
# [SPI API Documentation](https://wiki.cmc.ec.gc.ca/wiki/SPI/Documentation#Developer_documentation)

# Getting the source code
```shell
git clone --recursive git@gitlab.science.gc.ca:ECCC_CMOE_APPS/SPI 
```

# Building SPI
If you wish to build SPI from source, you should use the [SPI-Bundle](https://gitlab.science.gc.ca/ECCC-ASTD-MRD/SPI-Bundle) repository.
