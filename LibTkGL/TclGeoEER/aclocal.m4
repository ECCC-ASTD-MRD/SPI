#
# Include the TEA standard macro set
#

builtin(include,tclconfig/tcl.m4)

#
# Add here whatever m4 macros you want to define for your package
#

#builtin(include,m4/gdal.m4)
m4_include([m4/gdal.m4])
