#
# Include the TEA standard macro set
#

builtin(include,tclconfig/tcl.m4)

#
# Add here whatever m4 macros you want to define for your package
#

m4_include([../m4/ax_lib_eer.m4])
m4_include([../m4/ax_lib_gdal.m4])
m4_include([../m4/ax_lib_gdb.m4])
m4_include([../m4/ax_lib_rmn.m4])
m4_include([../m4/ax_lib_grib.m4])
m4_include([../m4/ax_lib_ecbufr.m4])
m4_include([../m4/ax_lib_flt.m4])
m4_include([../m4/ax_lib_urp.m4])
m4_include([../m4/m4_ax_openmp.m4])
