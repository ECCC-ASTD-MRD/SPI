# SPI Description

SPI is a scientific and meteorological virtual globe offering immense processing, analysis and visualization capabilities, with a user interface similar to Google Earth and NASA World Wind. SPI uses an "always 3D" rendering method. Everything is always rendered in three dimensions, looking from the top it looks like a regular 2D map, but moving the camera around gives a new perspective of things.

SPI is built in Tcl/Tk. Most of the rendering and scientific functionalities are built as binary extensions to Tcl in the form of objects and functions and then used in SPI’s Tcl source code.

An exhaustive API described in another document allows for very powerful scripting, going from generation of a map to calculating flux along the path of a shape like country boundary. This API can be used to interact with many kind of data like RPN standard file, CMC trajectories, ESRI shapefiles, GeoTIFF imagery, BURP observation files and more,.


# [SPI Documentation](https://wiki.cmc.ec.gc.ca/wiki/MIDAS/Coding_Standards)
# [SPI API Documentation](https://wiki.cmc.ec.gc.ca/wiki/SPI/Documentation#Developer_documentation)


# Building SPI

# Building source package for distribution

To build a package for external distribution, use the root makeit script.

```makeit -src```

This will build a source package including SPI, [GenPhysX](https://gitlab.science.gc.ca/ECCC_CMOE_APPS/genphysx) and [libeerUtils](https://gitlab.science.gc.ca/ECCC_CMOE_MODELS/libeerutils).


# SPI test suite

# Automatic Testing using GitLab-CI

An automatic system of tests has been developed.  For each push in the
`master` branch the system tests are launched to guarantee that the
all the tests pass for the `master` branch.  The [instructions for
automatic testing using GitLab-CI are available in a separate
file](CI.md).